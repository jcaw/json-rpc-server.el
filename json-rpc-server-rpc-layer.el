;; -*- lexical-binding: t -*-

;; Package that allows external JSON-RPC calls to Emacs.


(require 'json)


(defvar jrpc-exposed-functions '()
  "List of functions that can be executed via POST request.

This list is used by `jrpc-handler-eval-function' to determine
which functions it will accept. It's a security measure to ensure
only the functions you want are exposed. If you want to post a
function to \"/eval-function\", it must be on this list or it
will not be executed.")


(define-error 'jrpc-procedural-error
  ;; This is the base class for all errors that occur during the handling of an
  ;; RPC request. The purpose of this error class is to categorise all runtime
  ;; errors that should produce an error code, per the JSON-RPC 2.0
  ;; specification.
  ;;
  ;; The program can capture errors of this class, and dispatch the specific
  ;; error to a handler which will decode its error code and construct a
  ;; response.
  "An error was raised while processing the JSON-RPC request itself.")


(define-error 'jrpc-invalid-request
  ;; Error to be raised when a request is invalid.
  "An invalid request was supplied."
  'jrpc-procedural-error)


(define-error 'jrpc-error-calling-method
  ;; Error to be raised when there was an error calling the procedure specified
  ;; by the RPC request.
  "There was an error calling the method."
  'jrpc-procedural-error)


(define-error 'jrpc-invalid-function
  ;; Error to be raised when the procedure to be invoked is not available.
  "The function was not available to call."
  'jrpc-procedural-error)


(define-error 'jrpc-invalid-params
  ;; Error to be raised when the parameters supplied were not valid.
  "The parameters supplied were invalid."
  'jrpc-procedural-error)


(define-error 'jrpc-type-error
  ;; Error to be raised when type mismatch is detected (usually, because a
  ;; supplied variable had the wrong type).
  "A variable had the wrong type."
  'jrpc-procedural-error)


(define-error 'jrpc-unforeseen-error
  ;; Error to be raised an unforeseen error occurs.
  "An unforeseen error occurred."
  'jrpc-procedural-error)


(cl-defstruct jrpc-request
  "Object representing a JSON-RPC request.

This represents a request as laid out in the JSON-RPC 2.0
specification.

https://www.jsonrpc.org/specification"
  jsonrpc
  method
  (params '())
  id)


(cl-defstruct jrpc-response-error
  "Object representing a JSON-RPC response's error."
  (code (:type int))
  (message (:type string))
  (data nil))


(cl-defstruct (jrpc-response
               (:constructor nil)
               (:constructor make-jrpc-response-error
                             (&key
                              (error (:type jrpc-eror))
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


(defun jrpc-response-to-alist (instance)
  "Convert a jrpc response object into an alist so it can be encoded into JSON."
  (list (cons "jsonrpc" (jrpc-response-jsonrpc instance))
        (cons "result" (jrpc-response-result instance))
        (cons "error" (jrpc-response-error-to-alist (jrpc-response-error instance)))
        (cons "id" (jrpc-response-id instance))))


(defun jrpc-response-error-to-alist (instance)
  "Convert a jrpc error object into an alist so it can be encoded into JSON.

If instance is nil, nil will be returned."
  (when instance
    (list (cons "code" (jrpc-response-error-code instance))
          (cons "message" (jrpc-response-error-message instance))
          (cons "data" (jrpc-response-error-data instance)))))


;; --------------------------------------------------------------------------


(defun jrpc-null-p (value)
  "Is `VALUE' either nil, or json-null?

Other falsey values, such as 0, do not count. Note that the empty
list is equivalent to nil, so the empty list counts as nil."
  (or (eq value nil)
      (eq value json-null)))


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


(defun jrpc-alist-get (key alist)
  "List alist-get, but works with string keys."
  (let ((pair (assoc key alist)))
    (and pair
         (eq (type-of pair) 'cons)
         (cdr pair))))


(defun jrpc--call-function (func args)
  ;; Check if the function is callable is close to calling the function as
  ;; possible.
  (unless (member func jrpc-exposed-functions)
    (signal
     'jrpc-invalid-function
     (concat
      "Function has not been exposed (it may or may not exist). Cannot "
      "execute. Please expose this function with `jrpc-expose-function' "
      "if you want to call it remotely.")))
  ;; TODO: Check if function is callable with args. Can the function signature
  ;; be checked?
  (condition-case-unless-debug err
      (apply func args)
    (error
     (signal 'jrpc-error-calling-method
             `((message . "There was an error calling the method.")
               (error . ,err))))))


(defun jrpc--execute-request (request)
  "Execute a remote procedure call.

`REQUEST' should be a `jrpc-request' object."
  (unless (eq (type-of request)
              'jrpc-request)
    ;; This should never be reached from outside the package. `request' will
    ;; always be passed as a `jrpc-request' object. But include it, just in
    ;; case.
    (signal 'jrpc-type-error "`request' should be a `jrpc-request' object."))
  (let* ((method-name (jrpc-request-method request))
         ;; Because we can only transport strings via JSON, the method name has
         ;; to be encoded as a string. That means we have to manually convert it
         ;; into a symbol before invocation.
         (method-symbol
          ;; I don't know what kind of strings fail to convert to symbols, but
          ;; add error handling just in case.
          (condition-case nil
              (intern method-name)
            (error
             (signal
              'jrpc-invalid-request
              (concat
               "`method` could not be converted to an Elisp symbol. It "
               "should be a string that converts into an elisp symbol.")))))
         (args (jrpc-request-params request)))
    (jrpc--call-function method-symbol args)))


(defun jrpc--request-from-json (json)
  "Decode a request from `JSON' into a `jrpc-request'.

Relevant errors will be raised if the request is invalid."
  (let* ((request-alist (jrpc--decode-request-json json))
         (jsonrpc       (alist-get 'jsonrpc request-alist))
         (method        (alist-get 'method  request-alist))
         (params        (alist-get 'params  request-alist))
         (id            (alist-get 'id      request-alist))
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
      (signal
       'jrpc-invalid-request
       (concat "Only jsonrpc versions 1 to 2.0 are supported. jsonrpc 2.0 is "
               "preferred. If the `jsonrpc` parameter is included, it must be "
               "\"2.0\" exactly.")))
    (unless method
      (signal 'jrpc-invalid-request "`method` was not provided.'"))
    ;; TODO: Perhaps ensure the function is not a `json-rpc-server' function?
    ;; E.g. disallow the `jrpc-' prefix? Perhaps not. Unlikely to be reliable.
    ;; User should simply never expose those functions.
    (unless (stringp method)
      (signal 'jrpc-invalid-request "`method` should be a string."))
    ;; `params' should be a list of arguments, but it is optional. We have to
    ;; allow a nil value.
    (unless (or (jrpc-null-p params)
                (listp params))
      ;; TODO: Should this be a jrpc-invalid-params-error?
      (signal 'jrpc-invalid-request
              (concat "`params` was provided, but it was not an array. Could "
                      "not decode the parameters into a list.")))
    (unless id
      (signal 'jrpc-invalid-request "`id` not provided"))
    (unless (integerp id)
      (signal 'jrpc-invalid-request "`id` should be an integer."))
    (make-jrpc-request :jsonrpc jsonrpc
                       :method method
                       :params params
                       :id id)))


(defun jrpc--decode-request-json (json)
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
        (json-key-type 'symbol)
        )
    (json-read-from-string json)))


(defun jrpc--encode-error-response (error-raised)
  (error "Not Implemented"))


(defun jrpc--encode-result-response (result id)
  "Create a JSON-RPC 2.0 response with a successful result.

`RESULT' should be the raw result data returned by the procedure
invoked.

`ID' should be the id in the original JSON-RPC request, so the
response can be synchronized to it."
  (json-encode
   (jrpc-response-to-alist
    (make-jrpc-response-result :result result
                               :id id))))


(defun jrpc--decode-id (request-in-json)
  "Attempt to decode the ID and NOTHING ELSE.

If no ID could be decoded, returns nil.

This method will not raise errors."
  (ignore-errors
    (let ((id (alist-get
             'id
             (jrpc--decode-request-json request-in-json))))
    (when (integerp id)
      id))))


(defun jrpc-handle (request-in-json)
  "Handle a JSON-RPC request.

The request should be encoded in `JSON'.

Returns the JSON-RPC response, encoded in JSON, as a string.

This is the main entry point into the RPC layer. This is the
method that decodes the RPC request and executes it. This method
is transport-agnostic - transport has to be implemented
separately."
  ;; TODO: Handle batch objects too.
  (let (
        ;; We attempt to decode the id using a robust method, to give us the
        ;; maximum chance of being able to include it in the response if there is
        ;; an error. However, this may still fail.
        (id (jrpc--decode-id request-in-json))
        )
    (condition-case err
        (jrpc--encode-result-response
         (jrpc--execute-request
          (jrpc--request-from-json request-in-json))
         id)
      (jrpc-procedural-error
       (jrpc--encode-error-response err))
      (error
       ;; There are a number of ways we might wish to handle unaccounted for
       ;; errors. Segregate this, for now.
       (jrpc--handle-unkown-error err)))))


(defun jrpc-expose-function (func)
  "Expose a function to \"/eval-function\".

"
  ;; TODO: Rewrite documentation for json rpc server
  (add-to-list 'jrpc-exposed-functions func))


(defun jrpc-hide-funtion (func)
  "Hide `func' from public execution.

This reverses `jrpc-expose-function'."
  (setq jrpc-exposed-functions (remove func jrpc-exposed-functions)))


(provide 'json-rpc-server-rpc-layer)
;;; json-rpc-server-rpc-layer.el ends here
