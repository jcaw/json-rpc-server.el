;; -*- lexical-binding: t -*-

;; Package that allows external JSON-RPC calls to Emacs.


;;; Code:


(require 'json)
(require 'cl-lib)


(define-error 'jrpc-procedural-error
  ;; This is the base class for all errors that occur during the handling of an
  ;; RPC request. The purpose of this error class is to categorise all runtime
  ;; errors that should produce an error code, per the JSON-RPC 2.0
  ;; specification.
  ;;
  ;; The program can capture errors of this class, and dispatch the specific
  ;; error to a handler which will decode its error code and construct a
  ;; response.
  ;;
  ;; Please note that subclasses of this error should not be raised directly.
  ;; They should be raised with `jrpc--raise-procedural-error'. This method
  ;; attaches data to the errors in a particular structure - the handlers for
  ;; these errors expect this structure.
  ;;
  ;; This error is an abstract error. It should never be used directly - only
  ;; use its subclasses.
  "An error was raised while processing the JSON-RPC request itself")


(define-error 'jrpc-invalid-request
  ;; Error to be raised when a request is invalid.
  "An invalid request was supplied"
  'jrpc-procedural-error)


(define-error 'jrpc-error-calling-method
  ;; Error to be raised when there was an error calling the procedure specified
  ;; by the RPC request.
  "There was an error calling the method"
  'jrpc-procedural-error)


(define-error 'jrpc-invalid-function
  ;; Error to be raised when the procedure to be invoked is not available.
  "The function was not available to call"
  'jrpc-procedural-error)


(define-error 'jrpc-invalid-request-json
  ;; Error to be raised when the JSON in a supplied request is invalid.
  "The request's JSON was invalid."
  'jrpc-procedural-error)


(define-error 'jrpc-invalid-params
  ;; Error to be raised when the parameters supplied were not valid.
  "The parameters supplied were invalid"
  'jrpc-procedural-error)


(define-error 'jrpc-type-error
  ;; Error to be raised when type mismatch is detected (usually, because a
  ;; supplied variable had the wrong type).
  "A variable had the wrong type"
  'jrpc-procedural-error)


(define-error 'jrpc-unforeseen-error
  ;; Error to be raised an unforeseen error occurs.
  "An unforeseen error occurred"
  'jrpc-procedural-error)


(defvar jrpc--error-codes
  '((jrpc-invalid-request-json . -32700)
    (jrpc-invalid-request      . -32600)
    (jrpc-invalid-function     . -32601)
    (jrpc-invalid-params       . -32602)
    (jrpc-error-calling-method . -32603)
    (jrpc-unforeseen-error     . -32603)
    (jrpc-type-error           . -32603))
  "Alist mapping procedural errors to their JSON-RPC 2.0 error codes.")


(defvar jrpc--unknown-error-code  -32603
  "Error code to be used for unknown errors.")


(cl-defstruct jrpc-error-for-response
  "Object representing a JSON-RPC response's error.

Please note this is not an Emacs error. It's a collection of
information *about* an error, as laid out in the JSON-RPC 2.0
protocol, to be included as part of a JSON-RPC 2.0 response."
  (code :type int)
  (message :type string)
  (data nil))


(cl-defstruct (jrpc-response
               (:constructor nil)
               (:constructor make-jrpc-response-error
                             (&key
                              (error :type jrpc-error-for-response)
                              id)
                             "Construct a JSON-RPC response for an error.")
               (:constructor make-jrpc-response-result
                             (&key result id)
                             "Construct a JSON-RPC response for a successful result."))
  "Object representing a JSON-RPC response.

This represents a response as laid out in the JSON-RPC 2.0
specification.

https://www.jsonrpc.org/specification

Note that a response can contain *either* the result of a
successful RPC call or an error, but not both. Thus this struct
has two constructors:

 - `make-jrpc-response-result' :: create a response representing
                                  a successful result.
 - `make-jrpc-response-error' :: create a response representing
                                 an error.

No other constructors exist. One of those two must be used.

Also note that this response will *always* be tagged as JSON-RPC
2.0, even if the client sent a JSON-RPC 1.0 request."
  (jsonrpc "2.0")
  (result nil)
  (error nil)
  id)


(defun jrpc--get-error-code (error-symbol)
  "Get the JSON-RPC 2.0 specification error code for an error.

`ERROR-SYMBOL' should be a symbol representing the error. It
should a subclass of `jrpc-procedural-error'."
  (interactive)
  (or (alist-get error-symbol jrpc--error-codes)
      jrpc--unknown-error-code))


(cl-defun jrpc--raise-procedural-error (error-symbol
                                        message
                                        &key
                                        (original-error nil))
  "Raise a subclass of `jrpc-procedural-error'.

Subclasses of `jrpc-procedual-error' should not be raised
directly. This method should be used instead. It will attach data
to the error in a particular structure. The error handlers expect
this structure."
  (when (eq error-symbol 'jrpc-procedural-error)
    (error "%s"
           (concat "`jrpc-procedural-error' is an abstract error. Don't "
                   "try to raise it directly - only raise its sub-errors.")))
  (let ((data `((message . ,message)
                (json-rpc-error-code . ,(jrpc--get-error-code error-symbol)))))
    (when original-error
      (push (cons 'original-error original-error) data))
    (signal error-symbol data)))


(defun jrpc-response-to-alist (instance)
  "Convert a jrpc-response object to an alist.

`INSTANCE' should be an instance of the `jrpc-response' object.

This is necessary to encode into JSON. cl-structs cannot be
encoded at the time of writing (json.el version 1.4)."
  (let ((response-error (jrpc-response-error instance)))
    (list (cons "jsonrpc" (jrpc-response-jsonrpc instance))
          ;; The JSON-RPC 2.0 specification requires ONLY an error OR a result.
          ;; Not both. If there's an error, only include the error field. If
          ;; not, we only include the result field.
          (if response-error
               (cons "error" (jrpc-error-for-response-to-alist
                                   response-error))
             (cons "result" (jrpc-response-result instance)))
          (cons "id" (jrpc-response-id instance)))))


(defun jrpc-error-for-response-to-alist (instance)
  "Convert a jrpc error object into an alist so it can be encoded into JSON.

If `INSTANCE' is nil, nil will be returned."
  (when instance
    (list (cons "code" (jrpc-error-for-response-code instance))
          (cons "message" (jrpc-error-for-response-message instance))
          (cons "data" (jrpc-error-for-response-data instance)))))


;; --------------------------------------------------------------------------


(defun jrpc-null-p (value)
  "Is `VALUE' either nil, or json-null?

Other falsey values, such as 0, do not count. Note that the empty
list is equivalent to nil, so the empty list counts as nil."
  (or (eq value nil)
      (eq value json-null)))


(defun jrpc--string-to-int (string)
  "Convert a `STRING' to an int. Return nil if it cannot be converted."
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


(defun jrpc-alist-get (key alist)
  "Like `alist-get', but works with string keys.

`KEY' is the key to query.
`ALIST' is the alist to search."
  (let ((pair (assoc key alist)))
    (and pair
         (eq (type-of pair) 'cons)
         (cdr pair))))


(defun jrpc--call-function (func args)
  "Execute the remote procedure call for `FUNC' with `ARGS'."
  ;; TODO: Should we allow macro calls here too?
  (condition-case-unless-debug err
      (apply func args)
    (error
     (jrpc--raise-procedural-error
      'jrpc-error-calling-method
      "There was an error calling the method."
      :original-error err))))


(defun jrpc--execute-request (request exposed-functions)
  "Execute a remote procedure call.

`REQUEST' should be an alist representing a JSON-RPC 2.0 request.

`EXPOSED-FUNCTIONS' should be a list of function symbols that are
allowed to be executed. The method will not be executed unless
it's in this list. See `jrpc-handle' for more details.

An error will be raised if the function in the request does not
exist (or has not been exposed.)"
  (let* ((method-name (jrpc-alist-get "method" request))
         ;; Because we can only transport strings via JSON, the method name has
         ;; to be encoded as a string. That means we have to manually convert it
         ;; into a symbol before invocation.
         (method-symbol
          ;; If the method-name was quoted, it will already have been converted
          ;; to a symbol.
          (if (symbolp method-name)
              method-name
            ;; I don't know what kind of strings fail to convert to symbols, but
            ;; add error handling just in case.
            (condition-case nil
                (intern method-name)
              (error
               (jrpc--raise-procedural-error
                'jrpc-invalid-request
                (concat
                 "`method` could not be converted to an Elisp symbol. It "
                 "should be a string that converts into an elisp symbol."))))))
         (args (jrpc-alist-get "params" request)))
    ;; We now check that the function is legal, and callable, before trying to
    ;; call it.
    (unless (member method-symbol exposed-functions)
      (jrpc--raise-procedural-error
       'jrpc-invalid-function
       (concat
        "Function has not been exposed (it may or may not exist). Cannot "
        "execute.")))
    (unless (functionp method-symbol)
      (jrpc--raise-procedural-error
       'jrpc-invalid-function
       "This symbol has been exposed, but it is not a function. Cannot call it."
       ))
    ;; TODO: Check if function is callable with args. Can the function signature
    ;; be checked?
    (jrpc--call-function method-symbol args)))


(defun jrpc--validate-request (request-alist)
  "Validate that a decoded request has the correct structure.

The request should be provided in the form of an alist.
`REQUEST-ALIST' is the request.

Relevant errors will be raised if the request is invalid."
  (when (jrpc-null-p request-alist)
    (jrpc--raise-procedural-error
     'jrpc-invalid-request "No request provided"))
  (unless (json-alist-p request-alist)
    (jrpc--raise-procedural-error
     'jrpc-invalid-request "The request was not a JSON \"object\""))
  (let* ((jsonrpc       (jrpc-alist-get "jsonrpc" request-alist))
         (method        (jrpc-alist-get "method"  request-alist))
         (params        (jrpc-alist-get "params"  request-alist))
         (id            (jrpc-alist-get "id"      request-alist))
         ;; If there's no `jsonrpc' parameter, we assume this is probably a
         ;; jsonrpc 1.0 request.
         (appears-to-be-jsonrpc-v1 (jrpc-null-p jsonrpc))
         ;; It's jsonrpc 2.0 iff the jsonrpc string is "2.0" *exactly*.
         (is-jsonrpc-v2.0 (and (stringp jsonrpc)
                               (string= jsonrpc "2.0"))))
    ;; The supported jsonrpc versions are 2.0 and lower. Other versions are not
    ;; supported.
    (unless (or is-jsonrpc-v2.0
                appears-to-be-jsonrpc-v1)
      (jrpc--raise-procedural-error
       'jrpc-invalid-request
       (concat "Only jsonrpc versions 1 to 2.0 are supported. jsonrpc 2.0 is "
               "preferred. If the `jsonrpc` parameter is included, it must be "
               "\"2.0\" exactly.")))
    (unless method
      (jrpc--raise-procedural-error
       'jrpc-invalid-request "`method` was not provided."))
    ;; TODO: Perhaps ensure the function is not a `json-rpc-server' function?
    ;; E.g. disallow the `jrpc-' prefix? Perhaps not. Unlikely to be reliable.
    ;; User should simply never expose those functions.
    (unless (or (stringp method)
                ;; Sometimes users may get confused and send a symbol as the
                ;; method name. That's fine. Tolerate this behavior.
                (symbolp method))
      (jrpc--raise-procedural-error
       'jrpc-invalid-request "`method` should be a string."))
    ;; `params' should be a list of arguments, but it is optional. We have to
    ;; allow a nil value.
    (unless (or (jrpc-null-p params)
                (listp params))
      ;; TODO: Should this be a jrpc-invalid-params-error?
      (jrpc--raise-procedural-error
       'jrpc-invalid-request
       (concat "`params` was provided, but it was not an array. Could "
               "not decode the parameters into a list.")))
    (unless id
      (jrpc--raise-procedural-error
       'jrpc-invalid-request "`id` not provided"))
    ;; "id" can be a string or a number. Floats are allowed, which seems odd
    ;; given rounding errors.
    (unless (or (numberp id)
                (stringp id))
      (jrpc--raise-procedural-error
       'jrpc-invalid-request "`id` should be an integer."))
    request-alist))


(defun jrpc--decode-request-json (json)
  "Decode JSON with custom rules and error handling.

Arrays will be decoded into lists, objects (dictionaries) will be
decoded into alists, and keys will be decoded into symbols.

If there is an error parsing the JSON, a
`jrpc-invalid-request-json' error will be raised. This can be
converted into a JSON-RPC response with
`jrpc--encode-error-response'."
  ;; Decode symbol-like strings into symbols. Do this as part of the JSON
  ;; parsing process.
  (jrpc--replace-symbol-strings
   ;; Set some custom options for the JSON decoder.
   (let (
         ;; Arrays should be decoded as lists, because this is the array-like
         ;; type most methods are going to expect.
         ;;
         ;; This adds a limitation to the RPC server. Some functions may expect
         ;; vectors, but only one type of list can be transferred via JSON. Those
         ;; functions will receive lists. This will have to be fixed manually by
         ;; the user with some kind of proxy function.
         (json-array-type 'list)
         ;; Hash tables are faster, but alists are more common.
         (json-object-type 'alist)
         ;; Keys should be symbols because alists keys should generally be
         ;; symbols, not strings.
         (json-key-type 'string)
         )
     (condition-case err
         (json-read-from-string json)
       (error
        ;; Catch JSON errors and raise a jrpc error that can be more easily
        ;; understood.
        (jrpc--raise-procedural-error
         'jrpc-invalid-request-json
         "There was an error decoding the request's JSON."
         :original-error err))))))


(defun jrpc--replace-symbol-strings (object)
  "Replace symbol-like strings with symbols.

This is a hack that allows symbols (most importantly, keyword
arguments) to be sent over the JSON-RPC protocol. It takes
strings prefixed with a single \"'\" or \":\", and converts them
into symbols. For example:

  \"'some-name\" -> 'some-name

  \":KEYWORD\" -> :KEYWORD

Strings with more than one quote/colon won't be converted. For
example:

  \"'a quote'\" -> \"'a quote'\"

This method replaces the symbols inline in the request structure. The structure itself will be modified.

This is a recursive function. It will call itself. Ensure
`max-lisp-eval-depth' is high enough to parse your JSON object."
  (cond ((stringp object)
         (cond ((string-match "^:[^:]+$" object)
                ;; If there's an error interning the object, just pass back the
                ;; original string.
                (condition-case nil
                    (intern object)
                  (error object)))
               ((string-match "^'[^']+$" object)
                ;; If there's an error interning the object, just pass back the
                ;; original string.
                (condition-case nil
                    ;; If it's prefixed with a quote, we have to shave off the quote
                    ;; before interning.
                    (intern (substring object 1))
                  (error object)))
               ;; It wasn't a symbol. Return the original string.
               (t object)))
        ((consp object)
         (cons (jrpc--replace-symbol-strings (car object))
               (jrpc--replace-symbol-strings (cdr object))))
        (t object)))


(defun jrpc--replace-unencodable-object (object)
  "Replace `OBJECT' if it can't be encoded into JSON.

This function is designed to sanitise complex objects before they
are encoded.

Method:

  1. This function attempts to encode `OBJECT' into JSON.

  2a. If it works, that's fine - the original `OBJECT' is
      returned, unaltered.

  2b. If it can't be encoded, that's a problem. A string is
      returned instead, indicating that the object could not be
      encoded properly.

Usage example:

  Let's say an error was raised during method execution, and
  we're trying it. This shouldn't be a problem, but an error can
  theoretically contain any kind of data. There is a (very small)
  risk that the error will contain data that can't be encoded,
  crashing the JSON serializer.

  We can call this method on the error to protect ourselves. If
  the error can't be encoded, it will simply be replaced with a
  message for the end user indicating the problem."
  (condition-case nil
      (progn
        (json-encode object)
        object)
    (error
     (concat "The object in this position could not be encoded into JSON. "
             "This string was inserted instead."))))


(defun jrpc--encode-error-response (jrpc-error &optional id)
  "Encode a `jrpc-procedural-error' into a JSON-RPC 2.0 error response.

The result will be a JSON-RPC 2.0 response string, containing
information about the error.

`JRPC-ERROR' is assumed to be the error raised during the
processing of a JSON-RPC request. It needs to be a subclass of
`jrpc-procedural-error', raised with
`jrpc--raise-procedural-error', to ensure it has the information
and structure necessary to encode it into a response.

`ID' should be the id of the original request. Extracting an id
is not always possible, so this parameter is optional."
  (let* ((original-error-data (cdr jrpc-error))
         (error-message (alist-get 'message original-error-data))
         (error-code (alist-get 'json-rpc-error-code original-error-data))
         (underlying-error (alist-get 'original-error
                                      original-error-data))
         ;; The additional data should be an alist of additional data keys to
         ;; their data.
         (additional-data nil))
    ;; Additional data should *only* have a value when additional data exists.
    ;; It should be null otherwise.
    (when underlying-error
      (setq additional-data
            (append
             additional-data
             ;; TODO: Should we handle weird error structures here? Can
             ;; we always count on the underlying error having a `car'
             ;; and a `cdr'?
             `((underlying-error . ((type . ,(car underlying-error))
                                    (data . ,(jrpc--replace-unencodable-object
                                              (cdr underlying-error)))))))))
    (json-encode
     (jrpc-response-to-alist
      (make-jrpc-response-error
       :id id
       :error (make-jrpc-error-for-response
               :code error-code
               :message error-message
               :data additional-data))))))


(defun jrpc--encode-result-response (result id)
  "Create a JSON-RPC 2.0 response with a successful result.

`RESULT' should be the raw result data returned by the procedure
invoked.

`ID' should be the id in the original JSON-RPC request, so the
response can be synchronized to it."
  ;; TODO: Handle errors encoding the result. If that happens, the response
  ;; should be a new JSON-RPC error defined by this API to indicate that the
  ;; response could not be encoded.
  (json-encode
   (jrpc-response-to-alist
    (make-jrpc-response-result :result result
                               :id id))))


(defun jrpc--extract-id (decoded-request)
  "Attempt to extract the ID from a request alist and NOTHING ELSE.

If no ID could be extracted, returns nil.

This method will not raise errors.

`DECODED-REQUEST' should be a JSON-RPC request (up to 2.0),
decoded from JSON into an alist form."
  (ignore-errors
    (let ((id (jrpc-alist-get "id" decoded-request)))
      (when (integerp id)
        id))))


(defun jrpc--decode-id (request-in-json)
  "Attempt to decode the id from a JSON request and NOTHING ELSE.

If no id could be decoded, returns nil.

This method will not raise errors.

`REQUEST-IN-JSON' should be a JSON-RPC request (up to 2.0) in
JSON form."
  (ignore-errors
    (jrpc--extract-id (jrpc--decode-request-json request-in-json))))


(defun jrpc-unknown-error-response (&optional request-in-json)
  "Construct a JSON response indicating an unknown error.

This is a utility method for the transport layer to use in a
protected form. For example, let's say the transport layer
invokes a function that triggers an error and debugging is
enabled. The error will not be caught - it will invoke the
debugger.

To adhere to the JSON-RPC 2.0 protocol, the transport layer
could (for example) implement a protected form that responds with
an unknown error before the error is raised to the debugger.

If possible, please supply this method with the original JSON-RPC
request, as a JSON string, in `REQUEST-IN-JSON'. It will be used
by this method to extract as much information as possible for the
response."
  (jrpc-internal-error-response "An unknown error occurred"
                                request-in-json))


(defun jrpc-internal-error-response (message &optional request-in-json)
  "Construct a JSON response indicating internal error `MESSAGE'.

This is a utility method for the transport layer to use to
generate a valid JSON-RPC 2.0 response related to an arbitrary
internal error. For example, if there is an internal error in the
transport layer, this could be used to wrap the error into a
valid JSON-RPC 2.0 response.

If possible, please supply this method with the original JSON-RPC
request, as a JSON string, in `REQUEST-IN-JSON'. It will be used
by this method to extract as much information as possible for the
response."
  ;; We manually construct the JSON from a string to minimise the chance of an
  ;; unexpected error, or a strange encoding of the JSON.

  ;; Ensure `MESSAGE' is a string to ensure it encodes predictably (i.e. as a
  ;; string).
  (unless (stringp message)
    (error "%s" "`message' must be a string"))
  (let ((id (or (jrpc--decode-id request-in-json)
                "null")))
    (format
     "
{
    \"jsonrpc\": \"2.0\",
    \"error\": {
        \"code\": -32700,
        \"message\": %s,
        \"data\": null
    },
    \"id\": %s
}"
     ;; We encode the message explicitly to prevent something being injected
     ;; into the JSON.
     (json-encode message)
     id)))


(defun jrpc--handle-single (decoded-request exposed-functions)
  "Handle a single JSON-RPC request.

`DECODED-REQUEST' should be a JSON-RPC (up to 2.0) request,
decoded into an alist.

`EXPOSED-FUNCTIONS' should be a list of function symbols that are
allowed to be executed. The method will not be executed unless
it's in this list. See `jrpc-handle' for more details.

Returns the JSON-RPC response, encoded in JSON."
  (let (
        ;; We attempt to decode the id using a robust method, to give us the
        ;; maximum chance of being able to include it in the response if there is
        ;; an error.
        ;;
        ;; This may still fail - that's OK. We just want to maximize the chance
        ;; of extracting it.
        (id (jrpc--extract-id decoded-request))
        )
    (condition-case err
        (jrpc--encode-result-response
         (jrpc--execute-request
          (jrpc--validate-request decoded-request)
          exposed-functions)
         id)
      (jrpc-procedural-error
       (jrpc--encode-error-response err id))
      ;; (error
      ;;  jrpc--encode-unknown-error-response err id)
      )))


(defun jrpc-handle (request-in-json exposed-functions)
  "Handle a JSON-RPC request.

Parameters
----------

`REQUEST-IN-JSON' should be a JSON-RPC (up to 2.0) request,
encoded in a JSON string.

`EXPOSED-FUNCTIONS' should be a list of function symbols that are
exposed to RPC calls. The RPC call will only be executed if its
method is in this list.

  - Each function name should be a symbol.

  - Do not include raw functions such as lambdas.

  - Do not include string names.

Returns the JSON-RPC 2.0 response, encoded in a JSON string.

Description
-----------

This is the main entry point into the RPC layer. This is the
method that decodes the RPC request and executes it. This method
is transport-agnostic - transport has to be implemented
separately.

This method can take either a single encoded request, or an
encoded list of requests, per the JSON-RPC 2.0 specification.

Please note that this implementation deviates slightly from the
JSON-RPC 2.0 specification:

  1. Notifications are not supported. All RPC requests will
     receive a response. Notifications may be implemented above
     this layer, at the transport level.

  2. Batch requests are not processed concurrently. Batch
     requests will always be processed in the order they are
     supplied. Responses will be supplied in the same order."
  (condition-case err
      ;; Per JSON-RPC 2.0 specification, requests can either be single requests
      ;; or a list of requests. We have to handle the request differently
      ;; depending on which it is, so we have to decode it here.
      (let* ((decoded-request (jrpc--decode-request-json request-in-json))
             ;; Because JSON objects (dictionaries) will be decoded into alists,
             ;; we can't assume any list is a batch requests. Single requests
             ;; will also look like lists. Instead, ensure the request is a list
             ;; *and* not a dictionary.
             ;;
             ;; If the request is an empty list (or null), we just process it
             ;; like a normal request.
             (is-batch-request (and (not (jrpc-null-p decoded-request))
                                    (listp decoded-request)
                                    (not (json-alist-p decoded-request)))))
        (if is-batch-request
            ;; Process each request in turn; Return an array of each process'
            ;; result, as a string.
            ;;
            ;; HACK: Because each request returns a JSON-encoded response, we
            ;; have to decode them before joining them together.
            (json-encode
             (mapcar 'json-read-from-string
                     (mapcar (lambda (request)
                               (jrpc--handle-single request
                                                    exposed-functions))
                             decoded-request)))
          (jrpc--handle-single decoded-request
                               exposed-functions)))
    (jrpc-procedural-error
     (jrpc--encode-error-response err nil))
    ;; TODO: Encode unknown error
    ;; (error
    ;;  (jrpc--encode-unknown-error-response err nil))
    ))


(provide 'json-rpc-server)
;;; json-rpc-server.el ends here
