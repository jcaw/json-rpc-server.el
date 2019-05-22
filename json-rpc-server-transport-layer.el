(require 'elnode)


;; Elnode needs to be monkey-patched because of a small bug that makes it
;; unusable on Emacs 25.1+
(require 'jrpc-elnode-monkeypatch)



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
