(defvar jrpc--ews-test-directory
  (file-name-directory (or load-file-name
                           buffer-file-name))
 "The location of the jrpc `emacs-web-server'-based transport layer's tests.")


(push (expand-file-name  (concat jrpc--ews-test-directory "../../"))
      load-path)


(load-file (concat jrpc--ews-test-directory
                   "../../"
                   "json-rpc-server-ews-transport-layer.el"))


(defconst jrpc-transport-layer-test-dir
  (file-name-directory
   (or load-file-name buffer-file-name)))


(defconst jrpc-transport-layer-test-port 0)


(defun jrpc-stop-server-on-idle-timer ()
  "Stop the server on an idle timer.

Designed to be called by the test suite after all tests have been
run successfully."
  (run-with-idle-timer 0.01 nil (lambda ()
                                  (jrpc-stop-server)))
  t)


(defun jrpc-run-transport-layer-tests ()
  "Test the functionality of the transport layer.

This method spins up a test server and then runs a set of HTTP
requests using Python. The results will be displayed in a new
buffer.

Note that this test will reset your `json-rpc-server'
configuration."
  (interactive)
  ;; Stop the server if it's running.
  (ignore-errors (jrpc-stop-server))
  ;; Have to expose a single method: `+'.
  (setq jrpc-exposed-functions '(+))
  ;; We use dynamic port 8006 for tests. These tests won't work if port 8006 is
  ;; already bound.
  (jrpc-start-server :port jrpc-transport-layer-test-port
                     :username "test_username"
                     :password "test_password")
  (let ((default-directory jrpc-transport-layer-test-dir))
    (async-shell-command
     (format "nosetests %s" "test_transport_layer.py")
     "*transport layer nosetests*")))


;; Tests will be run automatically when this buffer is evaluated directly.
(when (not load-file-name)
 (jrpc-run-transport-layer-tests))


(provide 'test-json-rpc-ews-transport-layer)
;;; test-json-rpc-ews-transport-layer.el ends here
