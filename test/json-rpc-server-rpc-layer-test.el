;;; json-rpc-server-rpc-layer-test.el --- Tests for json-rpc-server


(require 'ert)

(load-file "json-rpc-server-rpc-layer.el")


;; Unit tests
(progn
  ;; TODO: Find how to organize Elisp tests hierarchically.
  (ert-deftest test-jrpc--request-from-json ()
    "Test function for `jrpc--request-from-json'.

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
                    :type 'jrpc-invalid-request)))

  (ert-deftest test-jrpc--decode-request-json ()
    "Test for `jrpc--decode-request-json'.

Test whether it decodes json correctly, in the way I want.

Note that this does not test the functionality of `json.el'. It
only tests the additional conditions imposed by the
`jrpc--decode-request-json' method."
    ;; List decoding
    (progn
      ;; Simple members
      (should (equal (jrpc--decode-request-json
                      "[1, 2, 3]")
                     '(1 2 3)))
      (should (equal (jrpc--decode-request-json
                      "[\"first\", \"second\", \"third\"]")
                     '("first" "second" "third"))))

    ;; Index and object decoding.
    ;;
    ;; Indexes should be symbols, and the result should be an alist.
    (should (equal (jrpc--decode-request-json
                    "{\"index1\": \"value1\", \"index2\": \"value2\"}")
                   '((index1 . "value1")
                     (index2 . "value2"))))

    ;; Malformed json should raise a specific error, so it can be caught.
    (should-error (jrpc--decode-request-json
                   ;; Some malformed JSON input.
                   "als;d'asfoasf")
                  :type 'json-readtable-error))
  )


;; Integration tests
(progn
  ;; None yet.
  )


;;; json-rpc-server-rpc-layer-test.el ends here
