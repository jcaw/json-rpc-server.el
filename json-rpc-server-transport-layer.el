(require 'elnode)


;; Elnode needs to be monkey-patched because of a small bug that makes it
;; unusable on Emacs 25.1+
(require 'jrpc-elnode-monkeypatch)


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


(cl-defstruct (jrpc-call (:include jrpc-request))
  "Object representing a remote procedure call.

This contains each component of the json-rpc protocol, as well as
information about the HTTP connection.

The member `HTTPCON' holds an Elnode `httpcon' object. This
represents the inbound connection.

All other members are inherited from `jrpc-request'."
  httpcon)


(defun jrpc-decode-request ()
  ;; TODO: Header params will actually try and extract some from the body
  (let ((header-params (elnode-http-params httpcon))
        (body-params (jrpc--parse-body httpcon))
        (all-params (jrpc--join-params header-params body-params)))
    ;; TODO: verify all params
    
    ))


(defun jrpc-handle-request (httpcon)
  "Handle a new json-rpc request."
  (jrpc--decode-request)
  )





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


(provide 'json-rpc-server-transport-layer)
;;; json-rpc-server-transport-layer.el ends here
