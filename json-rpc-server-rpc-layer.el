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


(defun jrpc-handle (json)
  "Handle a JSON-RPC request.

The request should be encoded in `JSON'.

Returns the JSON-RPC response, encoded in JSON, as a string.

This is the main entry point into the RPC layer. This is the
method that decodes the RPC request and executes it. This method
is transport-agnostic - transport has to be implemented
separately."
  (jrpc--execute-request
   (jrpc--request-from-json)))


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
      (jrpc-invalid-request-error
       "If the `jsonrpc` parameter is included, it must be \"2.0\" exactly."))
    (unless method
      (jrpc-invalid-request-error
       "`method` was not provided.'"))
    (unless (stringp method)
      (jrpc-invalid-request-error
       "`method` should be a string."))
    ;; `params' should be a list of arguments, but it is optional. We have to
    ;; allow a nil value.
    (unless (or (jrpc-null-p params)
                (listp params))
      ;; TODO: Should this be a jrpc-invalid-params-error?
      (jrpc-invalid-request-error
       (concat "`params` was provided, but it was not an array. Could not "
               "decode the parameters into a list.")))
    (unless id
      (jrpc-invalid-request-error "`id` not provided"))
    (unless (integerp id)
      (jrpc-invalid-request-error "`id` should be an integer."))
    (jrpc-request :jsonrpc jsonrpc
                  :method method
                  :params params
                  :id id)))


(defun jrpc--decode-request-json (json)
  ;; TODO: We want to decode the Jason in a special that spits out the right
  ;;   output. E.g. arrays should be turned into lists, not vectors.
  (json-read-from-string json))


(cl-defstruct jrpc-request
  "Object representing a JSON-RPC request.

This represents a request as laid out in the JSON-RPC 2.0
specification.

https://www.jsonrpc.org/specification"
  jsonrpc
  method
  (params '())
  id)


(cl-defstruct jrpc-error
  "Object representing a JSON-RPC error."
  (code :type int)
  (message :type string)
  (data nil))


(cl-defstruct (jrpc-response
               (:constructor nil)
               (:constructor make-jrpc-response-error
                             (&key
                              (error :type jrpc-eror)
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
        (cons "error" (jrpc-error-to-alist (jrpc-response-error instance)))
        (cons "id" (jrpc-response-id instance))))


(defun jrpc-error-to-alist (instance)
  "Convert a jrpc error object into an alist so it can be encoded into JSON.

If instance is nil, nil will be returned."
  (when instance
    (list (cons "code" (jrpc-error-code instance))
          (cons "message" (jrpc-error-message instance))
          (cons "data" (jrpc-error-data instance)))))


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
