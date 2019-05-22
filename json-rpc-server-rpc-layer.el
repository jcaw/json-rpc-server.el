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


(defun jrpc-nil-p (value)
  "Is `VALUE' exactly nil?

Other falsey values, such as 0, do not count. Note that the empty
  list is equivalent to nil, so the empty list counts."
  (eq value nil))


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
