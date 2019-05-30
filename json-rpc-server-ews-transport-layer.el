(require 'web-server)


(require 'json-rpc-server-rpc-layer)


(defvar jrpc--server nil
  "The currently running jrpc server.")


(define-error 'jrpc-http-error "An error occurred processing the HTTP request")
(define-error 'jrpc-malformed-http-request
  "The request was malformed"
  'jrpc-http-error)
(define-error 'jrpc-invalid-http-request
  "The request was invalid"
  'jrpc-http-error)

(define-error 'jrpc-missing-key "The specified key could not be found")


(defun jrpc-xor (arg1 arg2)
  "Exclusive or of two parameters, `ARG1' and `ARG2'.

This is not a bitwise comparison. It uses the truthiness of the
arguments to evaluate the result."
  (and (or arg1 arg2)
       (not (and arg1 arg2))))


(defun jrpc--extract-ws-post-content (request)
  "Manually extract the content from a `ws-request'.

`web-server' doesn't add content to the response unless it's of a
specific type. This is a hack to extract it manually."
  ;; TODO: Ensure request is a POST

  ;; This method relies on pending having the full request content.
  (with-slots (pending) request
    ;; TODO: Handle each way things might be malformed here
    (let* ((content-length-string (condition-case nil
                                      (jrpc--extract-header request :CONTENT-LENGTH)
                                    (error nil))))
      (when (eq nil content-length-string)
        (signal 'jrpc-malformed-http-request
                "No `Content-Length` parameter"))
      (let ((content-length (condition-case nil
                                (string-to-number content-length-string)
                              (error nil)))
            (headers-end (string-match "\r\n\r\n" pending)))
        (unless (integerp content-length)
          (signal 'jrpc-malformed-http-request
                  "`Content-Length` was not an integer."))
        (when (eq nil headers-end)
          (signal 'jrpc-malformed-http-request
                  "No double line breaks found in POST request. Content not acceptable"))
        (let* (
               ;; Have to offset the content start. It will be four characters
               ;; after the end of the headers because of the double newline.
               (content-start (+ 4 headers-end))
               (content-end (+ content-start content-length)))
          (condition-case nil
              (substring pending content-start content-end)
            (error
             (signal 'jrpc-malformed-http-request
                     "POST content was shorter than `Content-Length` parameter"))))))))


(defun jrpc--case-insensitive-comparison (string1 string2)
  "Check if two objects are identical strings, case insensitive.

Tolerates non-string input. Will simply return nil if a
non-string is supplied."
  (and (stringp string1) (stringp string2)
       (string= (downcase string1) (downcase string2))))


(defun jrpc--extract-header (request header-key)
  "Extract a header named by `HEADER-KEY'."
  (with-slots (headers) request
    (condition-case nil
        (cdr (assoc header-key headers))
      (signal 'jrpc-missing-key "Key %s could not be found in headers" header-key))))


(defun jrpc--send-400 (request message)
  (with-slots (process) request
    (ws-response-header process 400 '("Content-type" . "html/plain"))
    (process-send-string process message)))


(defun jrpc--handle-ews-request (request)
  "Handle a JSON-RPC request.

This method extracts the underlying JSON-RPC request and passes
it to the RPC layer to be executed. It then responds to the
client with the result."
  (with-slots (process headers context) request
    (condition-case err
        (progn
          (let ((content-type context))
            (unless content-type
              (signal 'jrpc-invalid-http-request (format "No `Content-Type` provided.")))
            (unless (jrpc--case-insensitive-comparison
                     (format "%s" content-type)
                     "application/json")
              (signal 'jrpc-invalid-http-request
                      (format
                       "`Content-Type` should be application/json. Was: %s"
                       content-type))))
          (let* ((content (jrpc--extract-ws-post-content request))
                 (jrpc-response (jrpc-handle content)))
            (ws-response-header process 200 '("Content-type" . "application/json"))
            (process-send-string process jrpc-response)))
      ((jrpc-malformed-http-request jrpc-invalid-http-request)
       (jrpc--send-400 request (cdr err))))))


(defun jrpc--ws-server-port (ws-server-instance)
  "Get the actual port a `ws-server' is running on.

By default, `ws-server' objects store the port which was given as
input to create the server. This may not actually be the port the
server is running on. For example, if a server was created with
dynamic port allocation, the `ws-server' object may have the port
stored as \"0\" or t - even though the network process was
allocated a specific port.

This method bypasses the flawed `ws-server' implementation and
extract the actual port from the underlying network process."
  (process-contact (oref ws-server-instance process) :service))


(defun jrpc--ews-auth-handler (&optional username password)
  "Create a request handler that requires authentication.

This handler works the same as `jrpc--handle-ews-request', but it
requires Basic Access Authentication for the request to be
processed.

`USERNAME' and `PASSWORD' are the authentication credentials to
use."
  (when (and username (not password))
    (setq password ""))
  (when (and password (not username))
    (setq username ""))
  (ws-with-authentication
   'jrpc--handle-ews-request
   ;; Build an alist with just this user
   (list (cons username password)))
  ;; TODO: Meaningful responses on failed authentication, internal errors, etc.
  )


(cl-defun jrpc-start-server (&key
                             (port "0")
                             username
                             password)
  "Start a new JSON-RPC 2.0 server.

JSON-RPC requests to the server should be sent in the body of
POST requests. They should be sent according to the JSON-RPC 2.0
specification, although the server will also tolerate JSON-RPC
1.x requests. JSON-RPC protocols >2.0 are not supported.

# Port

  The server will be allocated a random port when it is started.
  This will be printed to the message buffer. Use the keyword
  argument `:PORT' to specify a port to the server.

# Authentication

  The server optionally supports Basic Access Authentication to
  authenticate RPC requests:

    https://en.wikipedia.org/wiki/Basic_access_authentication

  By default, the server will be started with no authentication.
  If you would like to use Basic Access Authentication, specify
  the keyword arguments `:USERNAME' or `:PASSWORD'. You do not
  need to provide both (although it is obviously recommended).
  For example, if you provide only a password, the empty string
  will be used for the username.

Note that this method will fail if the server is already
running."
  (when jrpc--server
    (user-error "RPC server already running for this instance of Emacs. "
                "Please call `jrpc-stop-server' before starting another."))
  (setq jrpc--server
        (ws-start
         (if (or username password)
             (jrpc--ews-auth-handler username password)
           'jrpc--handle-ews-request)
        port))
  (message "JSON-RPC server running on port %s"
           (jrpc--ws-server-port jrpc--server)))


(defun jrpc-stop-server ()
  "Stop the active JSON-RPC 2.0 server.

This method will fail if no server is running."
  (unless jrpc--server
    (error "Server not running."))
  (ws-stop jrpc--server)
  (setq jrpc--server nil))


(provide 'json-rpc-server-ews-transport-layer)
;;; json-rpc-server-ews-transport-layer.el ends here
