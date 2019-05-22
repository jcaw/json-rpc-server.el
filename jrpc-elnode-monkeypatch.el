(require 'elnode)


;; Elnode doesn't work on Emacs 25.1+, because it sets both the `:server` and
;; `:nowait` flags. They can't both be set on Emacs 25.1+, so `:nowait` has to be
;; dropped.
;;
;; Make sure the patch is applied *after* elnode is loaded.
(with-eval-after-load 'elnode
  (defun elnode/make-service (host port service-mappings request-handler defer-mode)
    "Make an actual TCP server."
    (let ((an-buf (get-buffer-create "*elnode-webserver*")))
      (make-network-process
       :name "*elnode-webserver-proc*"
       :buffer an-buf
       :server t
       ;; This is patched in this version of the method
       :nowait nil
       :host (cond
              ((equal host "localhost") 'local)
              ((equal host "*") nil)
              (t host))
       :service port
       :coding '(raw-text-unix . raw-text-unix)
       :family 'ipv4
       :filter 'elnode--filter
       :sentinel 'elnode--sentinel
       :log 'elnode--log-fn
       :plist (list
               :elnode-service-map service-mappings
               :elnode-http-handler request-handler
               :elnode-defer-mode defer-mode)))))


(provide 'jrpc-elnode-monkeypatch)
;;; jrpc-elnode-monkeypatch.el ends here
