;;; json-rpc-server-test.el --- Tests for json-rpc-server


(require 'ert)

(load-file "json-rpc-server.el")


;; Unit tests
(progn
  ;; TODO: Find how to organize Elisp tests hierarchically.
  (ert-deftest test-json-rpc-server--validate-request ()
    "Test for `json-rpc-server--validate-request'.

Test whether it accepts good requests, and raises the
correct errors for flawed requests.

Note that when testing for raised errors, it doesn't test error
messages - it just tests the class of the signal."
    ;; Valid request, all fields filled.
    ;;
    ;; We also use this test to ensure the request is returned.
    (let ((request '(("jsonrpc" . "2.0")
                     ("method" . "message")
                     ("params" . ("This is a %s"
                                  "test message"))
                     ("id" . 12456))))
      (should (equal (json-rpc-server--validate-request request)
                     request)))
    ;; Valid request, but it's jsonrpc 1.0
    (should (json-rpc-server--validate-request
             '(("method" . "message")
               ("params" . ("This is a %s"
                            "test message"))
               ("id" . 12456))))
    ;; Valid request, but there's no params.
    (should (json-rpc-server--validate-request
             '(("jsonrpc" . "2.0")
               ("method" . "message")
               ("id" . 12456))))

    ;; This time, the id is a string.
    (should (json-rpc-server--validate-request
             '(("jsonrpc" . "2.0")
               ("method" . "message")
               ("id" . "b48297ce-8e07-4e72-b487-4d06b45cdf52"))))

    ;; Invalid `jsonrpc' param
    (progn
      ;; jsonrpc is 3.0 - too high
      (should (catch 'json-rpc-server-respond
                (json-rpc-server--validate-request
                 '(("jsonrpc" . "3.0")
                   ("method" . "message")
                   ("id" . 12456))))
              ;; TODO: Type
              )
      ;; jsonrpc is 2 - formatted wrong
      (should (catch 'json-rpc-server-respond
                (json-rpc-server--validate-request
                 '(("jsonrpc" . "2")
                   ("method" . "message")
                   ("id" . 12456))))
              ;; TODO: Type
              ))

    ;; Invalid `method' param
    (progn
      ;; No method
      (should (catch 'json-rpc-server-respond
                (json-rpc-server--validate-request
                 '(("jsonrpc" . "2.0")
                   ("id" . 12456))))
              ;; TODO: Type
              )
      ;; Wrong type for method
      (should (catch 'json-rpc-server-respond
                (json-rpc-server--validate-request
                 '(("jsonrpc" . "2.0")
                   ("method" . 120983)
                   ("id" . 12456))))
              ;; TODO: Type
              ))

    ;; Invalid `params' param
    (progn
      (should (catch 'json-rpc-server-respond
                (json-rpc-server--validate-request
                 '(("jsonrpc" . "2.0")
                   ("method" . "message")
                   ("params" . "Just a string param")
                   ("id" . 12456))))
              ;; TODO: Type
              ))

    ;; Invalid `id' param
    (progn
      ;; No id
      (should (catch 'json-rpc-server-respond
                (json-rpc-server--validate-request
                 '(("jsonrpc" . "2.0")
                   ("method" . "message"))))
              ;; TODO: type
              )
      ;; Invalid id type - in this case, a list.
      (should (catch 'json-rpc-server-respond
                (json-rpc-server--validate-request
                 '(("jsonrpc" . "2.0")
                   ("method" . "message")
                   ("id" . ["this is a list"]))))
              ;; TODO: Type
              )
      ;; Id is null.
      (should (catch 'json-rpc-server-respond
                (json-rpc-server--validate-request
                 '(("jsonrpc" . "2.0")
                   ("method" . "message")
                   ("id" . :json-null))))
              ;; TODO: Type
              ))
    )

  (ert-deftest test-json-rpc-server--decode-request-json ()
    "Test for `json-rpc-server--decode-request-json'.

Test whether it decodes json correctly, in the way I want.

Note that this does not test the functionality of `json.el'. It
only tests the additional conditions imposed by the
`json-rpc-server--decode-request-json' method."
    ;; List decoding
    (progn
      ;; Simple members
      (should (equal (json-rpc-server--decode-request-json
                      "[1, 2, 3]")
                     '(1 2 3)))
      (should (equal (json-rpc-server--decode-request-json
                      "[\"first\", \"second\", \"third\"]")
                     '("first" "second" "third"))))

    ;; Index and object decoding.
    ;;
    ;; Indexes should be strings, not symbols, and the result should be an
    ;; alist.
    (should (equal (json-rpc-server--decode-request-json
                    "{\"index1\": \"value1\", \"index2\": \"value2\"}")
                   '(("index1" . "value1")
                     ("index2" . "value2"))))

    ;; Malformed json should raise a specific error, so it can be caught.
    (should (catch 'json-rpc-server-respond
              (json-rpc-server--decode-request-json
               ;; Some malformed JSON input.
               "als;d'asfoasf"))
            ;; TODO: Type (invalid json)
            )

    ;; Try decoding a full request
    (should (equal
             (json-rpc-server--decode-request-json "{\"jsonrpc\": \"2.0\",\"method\": \"message\",\"params\": [\"This is a %s\", \"test message\"],\"id\": 12456,}")
             '(("jsonrpc" . "2.0")
               ("method" . "message")
               ("params" . ("This is a %s"
                            "test message"))
               ("id" . 12456))))
    )

  (ert-deftest test-json-rpc-server--execute-request ()
    "Test for `json-rpc-server--execute-request'.

Note that this while this test does test a full function
execution, it does not do so thoroughly. That is done in the unit
test for the underlying function,
`json-rpc-server--call-function'.

This test is primarily designed to check that the function is
correctly parsed and sent into `json-rpc-server--call-function'."
    (defun json-rpc-server--call-function-patch (func args)
      "Patched `json-rpc-server--call-function' that just checks
the types of the arguments."
      (should (symbolp func))
      ;; Note that nil counts as a list.
      (should (listp args)))

    ;; Mock `json-rpc-server--call-function' for these methods
    (cl-letf (((symbol-function 'json-rpc-server--call-function)
               'json-rpc-server--call-function-patch))
      ;; Check it executes okay with a simple method call
      (json-rpc-server--execute-request '(("method" . "message")
                               ("params" . ("this is a %s message"
                                            "test"))
                               ("id"     . 1))
                             '(message))
      ;; Check it executes okay no arguments
      (json-rpc-server--execute-request '(("method" . "message")
                               ("id"     . 1))
                             '(message)))

    ;; Ensure it executes correctly.
    (should (= (json-rpc-server--execute-request '(("method" . "+")
                                        ("params" . (1 2 3))
                                        ("id"     . 1))
                                      '(+))
               6)))


  (ert-deftest test-json-rpc-server-internal-error-response ()
    "Test for `json-rpc-server-internal-error-response'.

Tests the correct JSON is constructed, and the correct errors raised."
    ;; Check a simple message.
    (should (cl-equalp (json-read-from-string
                        (json-rpc-server-internal-error-response "This is a test"))
                       '((jsonrpc . "2.0")
                         (error   . ((code    . -32700)
                                     (message . "This is a test")
                                     (data    . nil)))
                         (id      . nil))))

    ;; Check a request that holds an id
    (should (cl-equalp (json-read-from-string
                        (json-rpc-server-internal-error-response
                         "This is a test"
                         "{\"method\": \"message\",\"id\": 12456,}"
                         ))
                       '((jsonrpc . "2.0")
                         (error   . ((code    . -32700)
                                     (message . "This is a test")
                                     (data    . nil)))
                         (id      . 12456))))

    ;; Check wrong message types
    (progn
      (should-error (json-rpc-server-internal-error-response nil)
                    :type 'error)
      (should-error (json-rpc-server-internal-error-response 1)
                    :type 'error)
      (should-error (json-rpc-server-internal-error-response '(("an" . "alist")))
                    :type 'error))

    ;; Check wrong JSON types
    ;;
    ;; These forms should execute without issue, but they should NOT contain an
    ;; id.
    (progn
      ;; nil JSON
      (should (not
               (alist-get
                'id
                (json-read-from-string
                 (json-rpc-server-internal-error-response "This is a test" nil)))))
      ;; Wrong JSON structure
      (should (not
               (alist-get
                'id
                (json-read-from-string
                 (json-rpc-server-internal-error-response "This is a test" "12980")))))
      ;; Non-string JSON
      (should (not
               (alist-get
                'id
                (json-read-from-string
                 (json-rpc-server-internal-error-response "This is a test" 12980))))))
    )

  (ert-deftest test-json-rpc-server--decode-id ()
    "Test for `json-rpc-server--decode-id'.

Tests that it decodes the id in minimalistic JSON, and also that
it does not block with errors when it cannot decode the id."
    ;; It should extract the id even if the overall request is invalid.
    (should (eq (json-rpc-server--decode-id "{\"id\": 10249}")
                10249))
    ;; Also check strings
    (should (equal (json-rpc-server--decode-id "{\"id\": \"10249\"}")
                   "10249"))

    ;; These are all invalid JSON, so they should return nil. Nothing should
    ;; raise an error.
    (progn
      ;; Null id
      (should (eq (json-rpc-server--decode-id "{\"id\": null}")
                  nil))
      ;; Invalid id type: object (dictionary)
      (should (eq (json-rpc-server--decode-id "{\"id\": {\"nested\": \"dict\"}}")
                  nil)))
    )

  (ert-deftest test-json-rpc-server--replace-symbol-strings ()
    "Test for `test-json-rpc-server--replace-symbol-strings'.

This test throws hypothetical objects at
`test-json-rpc-server--replace-symbol-strings' and ensures it
replaces the symbols correctly."
    ;; Symbol variants - cover normal symbols, and keywords.
    (progn
      (should (eq (json-rpc-server--replace-symbol-strings "'symbol")
                  'symbol))
      (should (eq (json-rpc-server--replace-symbol-strings "'SYMBOL")
                  'SYMBOL))
      (should (eq (json-rpc-server--replace-symbol-strings ":keyword")
                  :keyword))
      (should (eq (json-rpc-server--replace-symbol-strings ":KEYWORD")
                  :KEYWORD))

      ;; Special case - quoted keywords should just parse like normal symbols
      (should (eq (json-rpc-server--replace-symbol-strings "':keyword")
                  :keyword)))

    ;; Straight strings
    (progn
      (should (equal (json-rpc-server--replace-symbol-strings "a string")
                     "a string"))
      (should (equal (json-rpc-server--replace-symbol-strings "'double quoted string'")
                     "'double quoted string'"))
      (should (equal (json-rpc-server--replace-symbol-strings ":double key string:")
                     ":double key string:")))

    ;; There's no special handling for keywords with apostrophes in.
    (should (equal (json-rpc-server--replace-symbol-strings ":mixed 'keyword")
                   :mixed\ \'keyword))

    ;; Other types
    (progn
      (should (eq (json-rpc-server--replace-symbol-strings nil)
                  nil))
      (should (eq (json-rpc-server--replace-symbol-strings 23)
                  23)))

    ;; Nested types
    (should (equal (json-rpc-server--replace-symbol-strings '(something))
                   '(something)))
    (should (equal (json-rpc-server--replace-symbol-strings '(nil))
                   '(nil)))
    (should (equal (json-rpc-server--replace-symbol-strings '("string"))
                   '("string")))
    (should (equal (json-rpc-server--replace-symbol-strings '("'symbol"))
                   '(symbol)))

    ;; Cons cells
    (should (equal (json-rpc-server--replace-symbol-strings '("'symbol" . "a string"))
                   '(symbol . "a string")))

    ;; Monster nesting test - test this thoroughly
    (should (equal (json-rpc-server--replace-symbol-strings
                    '(
                      ;; Cons cell
                      ("string1" . "'symbol1")
                      ;; Straight list
                      ("string2" "'symbol2" ":keyword2" 2)
                      ;; Individual components
                      ":keyword3"
                      "'symbol3"
                      "string3"
                      3
                      ("string4"
                       ;; Nested cons cell
                       ("'symbol4" . ":keyword4")
                       4)))
                   '(
                     ;; Cons cell
                     ("string1" . symbol1)
                     ;; Straight list
                     ("string2" symbol2 :keyword2 2)
                     ;; Individual components
                     :keyword3
                     symbol3
                     "string3"
                     3
                     ("string4"
                      ;; Nested cons cell
                      (symbol4 . :keyword4)
                      4))))
    )
  )


;; Integration tests
(progn
  ;; None yet.

  (ert-deftest test-full-procedure-call--to-+ ()
    "Test a valid procedure call to `+'.

Note that `+' is a command that doesn't change the editor's
state. Thus this checks a limited type of functionality."
    ;; Get the response first, then progressively check each part of its
    ;; contents.
    (let ((response (json-read-from-string
                     (json-rpc-server-handle
                      (json-encode
                       '(("jsonrpc" . "2.0")
                         ("method"  . "+")
                         ("params"  . [1 2 3])
                         ("id"      . 21145)))
                      '(+)))))
      ;; Check each component, *then* check the full structure. We do this to
      ;; make it easier to pinpoint why the test is failing.
      (should (equal (alist-get 'jsonrpc response)
                     "2.0"))
      (should (eq (alist-get 'result response)
                  6))
      ;; The JSON-RPC 2.0 specification indicates that, on a successful
      ;; response, the `error' parameter should not be present in the response
      ;; at all. It cannot simply be null - it should not be there.
      (should (eq (assoc 'error response)
                  nil))
      (should (eq (alist-get 'id response)
                  21145))
      ;; Since Elisp has no reliable way of comparing alists with the same
      ;; elements in different orders, this is sensitive to the *order* of the
      ;; JSON object returned. The test will fail if the order changes. Not
      ;; perfect.
      (should (cl-equalp response
                         '((jsonrpc . "2.0")
                           (result  . 6)
                           (id      . 21145)))))
    )

  (ert-deftest test-full-procedure-call--changing-internal-state ()
    "Test a valid procedure call that just changes a variable.

This test is designed to test an internal state change. It tests
relatively minimal stay changing functionality. Only a variable
is changed - things like the buffer should be unaffected."
    ;; We have to define a function to change the variable, that takes a string
    ;; name as input, since we can't transfer symbols via JSON.
    (defun json-rpc-server-custom-setq (var-name new-value)
      (set (intern var-name) new-value))
    (let (
          ;; This is the variable we will try to change
          (test-var 10298)
          )
      (json-rpc-server-handle
       (json-encode
        '(("jsonrpc" . "2.0")
          ("method"  . "json-rpc-server-custom-setq")
          ("params"  . ["test-var" "this is a test string"])
          ("id"      . 21145)))
       '(json-rpc-server-custom-setq))
      (should (string= test-var
                       "this is a test string"))))

  (ert-deftest test-full-procedure-call--changing-buffer ()
    "Test a valid procedure call to `insert', with a temp buffer.

This test is designed to test functionality that changes the
state of the buffer.

This only tests the change in the buffer - other tests are
responsible for checking the actual response of the API."
    ;; Temporarily expose `insert'
    (with-temp-buffer
      (json-rpc-server-handle
       (json-encode
        '(("jsonrpc" . "2.0")
          ("method"  . "insert")
          ("params"  . ["this is a test string"])
          ("id"      . 21145)))
       '(insert))
      (should (string= (buffer-string)
                       "this is a test string")))
    )

  (ert-deftest test-full-procedure-call--unexposed-function ()
    "Test a procedure call to a function that hasn't been exposed.

This test is designed to test two things:

  1. The error type of a function that has not been exposed. This
     should match the JSON-RPC 2.0 specification. Specifically,
     the error code needs to match.

  2. The structure of an error response.

Other integration tests will check other error codes, but they
won't check the structure of the response. It is assumed that
this test is sufficient to check that for other error codes."
    ;; Get the response first, then progressively check each part of its
    ;; contents.
    (let* ((response (json-read-from-string
                      (json-rpc-server-handle
                       (json-encode
                        '(("jsonrpc" . "2.0")
                          ("method"  . "+")
                          ("params"  . [1 2 3])
                          ("id"      . 21145)))
                       ;; Expose no functions
                       '())))
           (response-error (alist-get 'error response)))
      (should response)
      ;; Check each component, *then* check the full structure. We do this to
      ;; make it easier to pinpoint why the test is failing.
      (should (equal (alist-get 'jsonrpc response)
                     "2.0"))
      ;; The JSON-RPC 2.0 specification indicates that, when an error is
      ;; raised, the `result' parameter should not be present in the response
      ;; at all. It cannot simply be null - it should not be there.
      (should (eq (assoc 'result response)
                  nil))
      (should (eq (alist-get 'id response)
                  21145))
      (should (eq (alist-get 'code response-error)
                  ;; This error code corresponds to "method not found" in the
                  ;; JSON-RPC 2.0 specification.
                  -32601))
      (should (eq (alist-get 'data response-error)
                  nil))
      ;; We don't check the exact string
      (should (stringp (alist-get 'message response-error)))))

  (ert-deftest test-full-procedure-call--non-existant-function ()
    "Test a procedure call to a function that has been exposed, but doesn't exist.

This test is designed to trick the system up by making it think
it is calling a valid function, causing an unexpected error when
the function is invoked."
    (let* ((response (json-read-from-string
                      (json-rpc-server-handle
                       (json-encode
                        '(("method"  . "json-rpc-server-function-that-does-not-exist")
                          ("id"      . 1)))
                       '(json-rpc-server-function-that-does-not-exist))))
           (response-error (alist-get 'error response)))
      (should response)
      ;; We only check the response code
      (should (eq (alist-get 'code response-error)
                  ;; This error code corresponds to "method not found" in the
                  ;; JSON-RPC 2.0 specification.
                  -32601))))

  (ert-deftest test-full-procedure-call--empty-json ()
    "Test a procedure call with empty JSON."
    (let* ((response (json-read-from-string
                      ;; The exposed functions don't matter here. Just pass an
                      ;; empty list.
                      (json-rpc-server-handle "{}" '())))
           (response-error (alist-get 'error response)))
      (should response)
      (should (eq (alist-get 'code response-error)
                  ;; This error code corresponds to "invalid request" in the
                  ;; JSON-RPC 2.0 specification.
                  -32600))))

  (ert-deftest test-full-procedure-call--with-symbols ()
    "Test a valid procedure call, that includes symbols.

Symbols have to be passed by abusing the JSON syntax. Test a full
procedure call works using this paradigm."
    ;; Temporarily expose `insert'
    (cl-defun json-rpc-server--symbols-test-function (arg1 &key keyword)
      ;; Keyword should be passed as a keyword, arg2 should end up a symbol.
      (should (equal arg1 "string"))
      (should (eq keyword 'symbol))
      t)
    (should (json-rpc-server-handle
             (json-encode
              '(("jsonrpc" . "2.0")
                ("method"  . "json-rpc-server--symbols-test-function")
                ("params"  . ["string" ":keyword" "'symbol"])
                ("id"      . 201398)))
             '(json-rpc-server--symbols-test-function)))
    )

  (ert-deftest test-full-procedure-call--quoted-method ()
    "Test that a procedure call still works when the function is quoted.

Given the way symbols are encoded, the user may get confused.
They might pass the function name quoted, rather than as a
straight string. This should be tolerated."
    (let ((response (json-read-from-string
                     (json-rpc-server-handle
                      (json-encode
                       '(("jsonrpc" . "2.0")
                         ("method"  . "'+")
                         ("params"  . [1 2 3])
                         ("id"      . 23234)))
                      '(+)))))
      (should-not (alist-get 'error response))
      (should (eq (alist-get 'result response)
                  6))
      (should (cl-equalp response
                         '((jsonrpc . "2.0")
                           (result  . 6)
                           (id      . 23234)))))
    )
  )


;;; json-rpc-server-test.el ends here
