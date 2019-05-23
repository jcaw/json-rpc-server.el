;;; json-rpc-server-rpc-layer-test.el --- Tests for json-rpc-server


(require 'ert)

(load-file "json-rpc-server-rpc-layer.el")


;; Unit tests
(progn
  ;; TODO: Find how to organize Elisp tests hierarchically.
  (ert-deftest test-jrpc--request-from-json ()
    "Test class for `jrpc--request-from-json'.

Test whether it decodes request json properly, and raises the
correct errors for flawed json.

This test doesn't thoroughly test the validity of the json. That
is handled in the unit test for the json decoder itself.

Note that when testing for raised errors, it doesn't test error messages - it just tests the class of the signal."
    ;; Valid request, all fields filled.
    (should (jrpc--structs-equal
             (jrpc--request-from-json "{\"jsonrpc\": \"2.0\",\"method\": \"message\",\"params\": [\"This is a %s\", \"test message\"],\"id\": 12456,}")
             (make-jrpc-request :jsonrpc "2.0"
                                :method "message"
                                :params '("this is a %s" "test message")
                                :id 12456)))
    ;; Valid request, but it's jsonrpc 1.0
    (should (jrpc--structs-equal
             (jrpc--request-from-json
              "{\"method\": \"message\",\"params\": [\"This is a %s\", \"test message\"],\"id\": 12456,}")
             (make-jrpc-request :jsonrpc nil
                                :method "message"
                                :params '("this is a %s" "test message")
                                :id 12456)))
    ;; Valid request, but there's no params.
    (should (jrpc--structs-equal
             (jrpc--request-from-json "{\"jsonrpc\": \"2.0\",\"method\": \"message\",\"id\": 12456,}")
             (make-jrpc-request :jsonrpc "2.0"
                                :method "message"
                                :params '()
                                :id 12456)))

    ;; Invalid `jsonrpc' param
    (progn
      ;; jsonrpc is 3.0 - too high
      (should-error (jrpc--request-from-json "{\"jsonrpc\": \"3.0\",\"method\": \"message\",\"id\": 12456,}")
                    :type 'jrpc-invalid-request)
      ;; jsonrpc is 2 -- formatted wrong
      (should-error (jrpc--request-from-json "{\"jsonrpc\": \"2\",\"method\": \"message\",\"id\": 12456,}")
                    :type 'jrpc-invalid-request))

    ;; Invalid `method' param
    (progn
      ;; No method
      (should-error (jrpc--request-from-json "{\"jsonrpc\": \"2.0\",\"id\": 12456,}")
                    :type 'jrpc-invalid-request)
      ;; Wrong type for method
      (should-error (jrpc--request-from-json "{\"jsonrpc\": \"2.0\",\"method\": 120983,\"id\": 12456,}")
                    :type 'jrpc-invalid-request))

    ;; Invalid `params' param
    (progn
      (should-error (jrpc--request-from-json "{\"jsonrpc\": \"2.0\",\"method\": \"message\",\"params\": \"Just a string param\",\"id\": 12456,}")
                    :type 'jrpc-invalid-request))

    ;; Invalid `id' param
    (progn
      ;; No id
      (should-error (jrpc--request-from-json "{\"jsonrpc\": \"2.0\",\"method\": \"message\",}")
                    :type 'jrpc-invalid-request)
      ;; Invalid id type - in this case, a string.
      (should-error (jrpc--request-from-json "{\"jsonrpc\": \"2.0\",\"method\": \"message\",\"id\": \"12456\",}")
                    :type 'jrpc-invalid-request))))


;; Integration tests
(progn
  ;; None yet.
  )


;; TODO: Do we need to provide?
;; (provide 'json-rpc-server-rpc-layer-test)
;;; json-rpc-server-rpc-layer-test.el ends here
