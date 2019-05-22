;; Package that allows external JSON-RPC calls to Emacs.

(require 'elnode)


(defun jrpc-nil-p (value)
  "Is `VALUE' exactly nil?

Other falsey values, such as 0, do not count. Note that the empty
  list is equivalent to nil, so the empty list counts."
  (eq value nil))


(defun jrpc--string-to-int (string)
  "Convert a string to an int. Return nil if it cannot be converted."
  (let ((number (ignore-errors
                  ;; Do not convert the empty string to converted to 0.
                  (and (not (jrpc-empty-string-p string))
                       (string-to-number string)))))
    ;; We don't want to return floats. Only ints. Do not cast floats.
    (and (integerp number)
         number)))


(defun jrpc-empty-string-p (value)
  "Is `VALUE' an empty string?"
  (and (stringp value)
       (string= value "")))


;; --------------------------------------------------------------------------


;; -*- lexical-binding: t -*-


;; External requires
(require 'elnode)


;; Elnode needs to be monkey-patched because of a small bug that makes it
;; unusable on Emacs 25.1+
(require 'jrpc-elnode-monkeypatch)


(defvar jrpc-exposed-functions '()
  "List of functions that can be executed via POST request.

This list is used by `jrpc-handler-eval-function' to determine
which functions it will accept. It's a security measure to ensure
only the functions you want are exposed. If you want to post a
function to \"/eval-function\", it must be on this list or it
will not be executed.")


(defun jrpc--extract-dash-prefix (prefix-arg)
  "If a `PREFIX-ARG' string is a dash, extract it.

This method attempts to convert a dash `PREFIX-ARG' into a dash
symbol. Returns nil if it cannot."
  (let ((prefix-symbol (ignore-errors (intern prefix-arg))))
    ;; Return the prefix symbol iff it is a dash.
    (and (eq prefix-symbol '-)
         prefix-symbol)))


(defun jrpc--prefix-from-string (prefix-arg)
  "Convert a string prefix argument into a proper prefix argument."
  ;; Try to convert to an int, then a dash. If these don't work, return nil.
  (or (jrpc--string-to-int prefix-arg)
      (jrpc--extract-dash-prefix prefix-arg)))


(defun jrpc-invoke-command-handler (httpcon)
  "Handle requests to invoke an interactive command.

This is an Elnode handler. `HTTPCON' is the request that should
be handled."
  (let* ((command (elnode-http-param httpcon "command" nil))
         (prefix-arg-string (elnode-http-param httpcon "prefix-arg" nil))
         (command-symbol (ignore-errors (intern command)))
         (is-valid-command (ignore-errors
                             (commandp command-symbol t)))
         (prefix-arg (jrpc--prefix-from-string prefix-arg-string))
         ;; We want to ensure that if a prefix argument was passed, it was
         ;; valid.
         ;;
         ;; We also want to accept an empty prefix argument.
         (prefix-arg-valid (or (jrpc-nil-p prefix-arg-string)
                               (jrpc-empty-string-p prefix-arg-string)
                               (not (jrpc-nil-p prefix-arg)))))
    ;; For each failure case, reply with a different message.
    (cond ((or (jrpc-nil-p command) (jrpc-empty-string-p command))
           (elnode-send-400 httpcon "No command provided."))
          ((not command-symbol)
           (elnode-send-400
            httpcon
            (format "Command \"%s\" could not be converted to a symbol."
                    command)))
          ((not is-valid-command)
           (elnode-send-400
            httpcon
            (format "Command \"%s\" is not an interactive command."
                    command)))
          ((not prefix-arg-valid)
           (elnode-send-400
            httpcon
            (format
             (concat "Prefix arg \"%s\" is not a valid prefix argument. "
                     "Please provide either an integer, or a single dash. "
                     "\n\n"
                     "To add an empty prefix argument (equivalent to simply "
                     "pressing `C-u`), please call the command "
                     "`universal-argument` directly.")
             prefix-arg-string)))
          (t
           ;; Inputs are ok. Invoke the command!
           (jrpc-emulate-command command-symbol prefix-arg)
           (elnode-send-json
            httpcon
            (json-encode "Command queued for execution."))))))


(defun jrpc-dispatcher-handler (httpcon)
  "Dispatch request to the correct handler based on its URL.

`HTTPCON' is the request to dispatch."
  (elnode-dispatcher httpcon jrpc-urls))


(defun jrpc-alist-get (key alist)
  "List alist-get, but works with string keys."
  (let ((pair (assoc key alist)))
    (and pair
         (eq (type-of pair) 'cons)
         (cdr pair))))


(defun jrpc--extract-body (httpcon)
  "Extract the body of an HTTP request.

This is ripped from `elnode--http-post-to-alist', extracted into
its own function to enable custom body handling."
  (with-current-buffer (process-buffer httpcon)
    (buffer-substring
     ;; we might have to add 2 to this because of trailing \r\n
     (process-get httpcon :elnode-header-end)
     (point-max))))


(defun jrpc--parse-json-body (httpcon)
  (let* ((body (jrpc--extract-body httpcon))
         (decoded-body (ignore-errors (json-read-from-string body))))
    (cond ((not body)
           (elnode-send-400
            httpcon
            "Body could not be extracted from the request."))
          ((not decoded-body)
           (elnode-send-400
            httpcon
            "Malformed JSON. Could not decode JSON from body."))
          (t decoded-body))))


(defun jrpc--parse-body (httpcon)
  "Parse the body of a request."
  (let* ((content-type (elnode-http-header httpcon 'content-type))
         (type-list
          (when content-type
            (mail-header-parse-content-type content-type)))
         (parsed-type (car type-list)))
    (cond
     ((not content-type)
      (elnode-send-400 httpcon "No content-type provided."))
     ((not parsed-type)
      (elnode-send-400 httpcon "Could not parse content type."))
     (t (if (string-match "/json$" parsed-type)
            ;; The document is json. Return the decoded json body.
            (jrpc--parse-json-body httpcon)
          (elnode-send-400
           httpcon
           (concat
            "Invalid `content-type`. JSON is the only content type "
            "currently supported. Make sure the datatype in the header "
            "is \"application/json\".")))))))


(defun jrpc--apply-exposed-function (func args httpcon)
  (if (member func jrpc-exposed-functions)
      (condition-case err
          (elnode-send-json httpcon (apply func args))
        (error
         (progn
           ;; Send the client information about the error.
           (elnode-send-500
            httpcon
            (format "Error while executing function: %s" err))
           ;; Re-raise the error for internal debugging.
           (signal (car err) (cdr err)))))
    (elnode-send-400
     httpcon
     (concat "Function has not been exposed (it may or may not exist). Cannot "
             "execute. Please expose this function with `jrpc-expose-function' "
             "if you want to call it remotely."))))


(defun jrpc-handler-eval-function (httpcon)
  "Evaluate a function and return the result, as JSON."
  (let* ((body (jrpc--parse-body httpcon))
         (func-string (alist-get 'function body))
         (func-symbol (ignore-errors (intern func-string)))
         (args-as-vector (alist-get 'args body))
         (args-as-list (append args-as-vector nil)))
    (cond ((not func-string)
           (elnode-send-400 httpcon "No function provided."))
          ((not func-symbol)
           (elnode-send-400 httpcon "Function could not be converted to a symbol."))
          (t (jrpc--apply-exposed-function func-symbol args-as-list httpcon)))))


(defun jrpc-expose-function (func)
  "Expose a function to \"/eval-function\".

"
  ;; TODO: We write documentation for json rpc server
  (add-to-list 'jrpc-exposed-functions func))


(defun jrpc-hide-funtion (func)
  "Hide `func' from public execution.

This reverses `jrpc-expose-function'."
  (setq jrpc-exposed-functions (remove func jrpc-exposed-functions)))


(defun jrpc-start-demo-server ()
  "Start a simple server that serves all commands.

This is intended to be used to test the API only.

This is just a prototype at the moment. For now, it always runs
on port 8002. This will be changed when a full server management
suite is implemented."
  (elnode-start 'jrpc-handler-eval-function :port 8002 :host "localhost"))


(defun jrpc-stop-demo-server ()
  "Stop the demonstration server."
  (elnode-stop 8002))


(provide 'json-rpc-server)
;;; json-rpc-server.el ends here
