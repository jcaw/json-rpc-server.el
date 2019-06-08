<p align=center>
    <img src="media/logo.png" alt="json-rpc-server logo" />
</p>

<h1 align=center>JSON-RPC-Server</h1>

<p align=center>Server-side implementation of the JSON-RPC 2.0 protocol for Emacs.</p>

<p align=center>
<!-- This is an emoticon. Don't delete it if it doesn't show up in your editor. -->
🔌
</p>

---

<!-- ## What is this Package? -->

This is a full implementation of the [JSON-RPC
2.0](https://www.jsonrpc.org/specification) protocol for Emacs. You pass in a
JSON string, Emacs executes the specified function(s), and a JSON-RPC 2.0
response is returned.

This package is designed sit underneath a transport layer. <b>No transport logic
is included.</b> The transport layer is responsible for communicating with
external clients. Since JSON-RPC provides [no inbuilt
mechanism](https://groups.google.com/d/msg/json-rpc/PN462g49yL8/DdMa93870_oJ)
for authenticating requests, the transport layer should also handle
authentication.

The default transport layer is
[Porthole](http://www.github.com/jcaw/http-rpc-server.el). It uses the HTTP
protocol.


---


<!-- markdown-toc start - Don't edit this section. Run M-x markdown-toc-generate-toc

     Please note that the markdown generator doesn't work perfectly with a
     centered heading. You can't refresh this list, you have to regenerate it.
     -->

### Table of Contents

- [How it Works](#how-it-works)
  - [Examples](#examples)
    - [Example: Calling a Method](#example-calling-a-method)
    - [Example: Invalid Request](#example-invalid-request)
    - [Example: Malformed JSON](#example-malformed-json)
    - [Example: Batch Requests](#example-batch-requests)
- [Datatype Limitations](#datatype-limitations)
  - [Other Types](#other-types)
  - [Symbols](#symbols)
  - [Keyword Arguments](#keyword-arguments)
    - [Example: Symbols and Keyword Arguments](#example-symbols-and-keyword-arguments)
- [Installation](#installation)
- [List of Transport Layers](#list-of-transport-layers)
- [FAQ](#faq)

<!-- markdown-toc end -->


## How it Works

`jrpc-handle` is the main entry point into the package (functions in this
package are prefixed with `jrpc-`). `jrpc-handle` takes a JSON-RPC 2.0 [request
string](https://www.jsonrpc.org/specification#request_object), and a list of
functions that are allowed to be called remotely.

```emacs-lisp
;; This will decode a JSON-RPC 2.0 request, execute it, and return the JSON-RPC 2.0 response.
(jrpc-handle string-encoded-json-rpc-request
             list-of-legal-functions)
```

If successful, the result will be a string containing the JSON-RPC 2.0 [result
response](https://www.jsonrpc.org/specification#response_object). If an error
occurs, it will contain a JSON-RPC 2.0 [error
response](https://www.jsonrpc.org/specification#error_object). Errors will be
captured and encoded into strings - they won't be raised above the handler
(unless you are debugging).

Only functions you have specifically exposed can be called via RPC. You must
pass a list of functions to `jrpc-handle` so it knows which functions it's
allowed to execute, and which it is not.

### Examples

#### Example: Calling a Method

Encode a request according to the JSON-RPC 2.0 protocol. `method` should be the
method name, as a string.

Here's an example request:

```json
{
    "jsonrpc": "2.0",
    "method": "+",
    "params": [1, 2, 3],
    "id": 29492,
}
```

Let's encode this into a string and pass it to `jrpc-handle`:

```emacs-lisp
(jrpc-handle
 "{
    \"jsonrpc\": \"2.0\",
    \"method\": \"+\",
    \"params\": [1,2,3],
    \"id\": 29492
}"
 ;; We have to make sure the `+' function is exposed to RPC calls.
 '(+))
```

`json-rpc-server` will decode the request, then apply the function `+` to the
list `'(1 2 3)`. Here's what the result of `jrpc-handle` will be:

```emacs-lisp
"{\"jsonrpc\":\"2.0\",\"result\":6,\"id\":29492}"
```

Decoded:

```json
{
    "jsonrpc": "2.0",
    "result": 6,
    "id": 29492
}
```

This string-encoded response can now be returned to the client.

(See the [Symbols](#symbols) section for how to transfer symbols and keyword
arguments.)

#### Example: Invalid Request

This time, let's try an invalid request.

```json
{
    "params": [1, 2, 3],
    "id": 23092
}
```

This request is invalid because it has no `"method"`. The call to `jrpc-handle`:

```emacs-lisp
(jrpc-handle
 "{
    \"params\": [1, 2, 3],
    \"id\": 23092
}"
 '(+))
```

Here's what `jrpc-handle` returns:

```emacs-lisp
"{\"jsonrpc\":\"2.0\",\"error\":{\"code\":-32600,\"message\":\"`method` was not provided.'\",\"data\":null},\"id\":23092}"
```

Decoded:

```json
{
    "jsonrpc": "2.0",
    "error": {
        "code": -32600,
        "message": "`method` was not provided.",
        "data": null
    },
    "id": 23092
}
```

Note the `"id"` field. `jrpc-handle` will do its best to extract an `id` from all
requests, even invalid requests, so errors can be synced up to their respective
requests.

#### Example: Malformed JSON

If there is a problem with the request (or another error occurs), `jrpc-handle`
will encode a JSON-RPC 2.0 [error
response](https://www.jsonrpc.org/specification#error_object). Here's an
example.

Let's try some malformed JSON:

```json
{Szx. dsd}
```

The call to `jrpc-handle`:

```emacs-lisp
(jrpc-handle "{Szx. dsd}" '(+))
```

Here's what `jrpc-handle` returns:

```emacs-lisp
"{\"jsonrpc\":\"2.0\",\"error\":{\"code\":-32700,\"message\":\"There was an error decoding the request's JSON.\",\"data\":{\"underlying-error\":{\"json-string-format\":[\"doesn't start with `\\\"'!\"]}}},\"id\":null}"
```

Decoded:

```json
{
  "jsonrpc": "2.0",
  "error": {
    "code": -32700,
    "message": "There was an error decoding the request's JSON.",
    "data": {
      "underlying-error": {
        "type": "json-string-format",
        "data": ["doesn't start with `\"'!"]
      }
    }
  },
  "id": null
}
```

Note the `"data"` field. Some responses are triggered by an underlying error in
the Elisp, which may contain more meaningful information about the error. When
possible, that will be returned in the `"underlying-error"` field. If there is
no underlying error, this field will not be present.

#### Example: Batch Requests

You can also execute multiple requests at once. This is useful if the client
wants to minimize the number of requests they have to make to a slow transport
layer. Each request will be executed, and a string response containing all the
results will be returned. Unlike most JSON-RPC 2.0 protocols, batch requests are
guaranteed to be executed in the same order they were received.

Let's make two requests - one to a valid function, one to an invalid one. We'll
batch them.

```json
[
  {
    "jsonrpc": "2.0",
    "method": "+",
    "params": [1, 2, 3],
    "id": 1
  },
  {
    "jsonrpc": "2.0",
    "method": "insert",
    "params": ["Some text to insert"],
    "id": 2
  }
]
```

The call to `jrpc-handle`:

```emacs-lisp
(jrpc-handle
 "[
    {
        \"jsonrpc\": \"2.0\",
        \"method\": \"+\",
        \"params\": [1, 2, 3],
        \"id\": 1
    },
    {
        \"jsonrpc\": \"2.0\",
        \"method\": \"insert\",
        \"params\": [\"Some text to insert\"],
        \"id\": 2
    }
]"
 ;; Let's expose `+', but not `insert', to demonstrate a result and an error.
 '(+))
```

Here's what `jrpc-handle` returns:

```emacs-lisp
"[{\"jsonrpc\":\"2.0\",\"result\":6,\"id\":1},{\"jsonrpc\":\"2.0\",\"error\":{\"code\":-32601,\"message\":\"Function has not been exposed (it may or may not exist). Cannot execute.\",\"data\":null},\"id\":2}]"
```

Decoded:

```json
[
  {
    "jsonrpc": "2.0",
    "result": 6,
    "id": 1
  },
  {
    "jsonrpc": "2.0",
    "error": {
      "code": -32601,
      "message": "Function has not been exposed (it may or may not exist). Cannot execute.",
      "data": null
    },
    "id": 2
  }
]
```

As you can see, one of the function calls executed successfully, another caused
an error. The responses should be returned in the same order their requests were
submitted, but they can also be synchronized based on their `"id"`.

`jrpc-handle` assumes that requests are atomic until proven otherwise. If your
batch request is *malformed*, `jrpc-handle` will probably not return a batch in
response - it will respond with a single "malformed json" error.

## Datatype Limitations

The structure of JSON limits the types of variables that can be transferred.
JSON only contains six datatypes. Thus, functions exposed by this protocol
<b>must expect certain datatypes.</b> 

The datatypes are mapped as follows:

| JSON Datatype             | Decodeded Elisp Datatype | In JSON                   | In Elisp                |
| ---                       | ---                      | ---                       | ---                     |
| string                    | string                   | `"string"`                | `"string"`              |
| [quoted string](#symbols) | symbol                   | `"'symbol"`, `":keyword"` | `'symbol`, `:keyword`   |
| number                    | integer or float         | `21`, `3.14`              | `21`, `3.14`            |
| boolean                   | `t` or `:json-false`     | `true`, `false`           | `t`, `:json-false`      |
| null                      | `nil`                    | `null`                    | `nil`                   |
| object                    | alist                    | `{"Key": "Value"}`        | `'(("Key" . "Value"))'` |
| array                     | list                     | `[1, 2, 4, 7]`            | `'(1 2 4 7)`            |

Note that alist keys are decoded as strings.

You may notice that "quoted strings" are decoded differently to normal strings.
See the [Symbols](#symbols) section for a full explanation.

### Other Types

Because of these type limitations, you cannot transfer vectors, plists, hash tables,
cl-structs, etc.

There is no easy way around this. JSON-RPC provides simplicity, at the cost of
flexibility. If you want to call a function that expects a different type, you
must write an intermediary function that translates from the available ones and
publish your intermediary instead.

### Symbols

Symbols are important in Elisp. Luckily, by abusing the JSON-RPC syntax we can
transfer symbols. Strings beginning with a single quote will be decoded into
symbols. Strings that start with a single colon will be decoded into keywords.

For example:

- The string `"'a-symbol"` becomes the symbol `'a-symbol`.
- The string `":a-keyword"` becomes the symbol `:a-keyword`.
- `"'wrapped-string'"` does not change, because it contains multiple quotes. It
  will stay a string.

Let's send a list:

```json
["a string", "'a-symbol", ":a-keyword"]
```

That list will be decoded into:

```emacs-lisp
'("a string" a-symbol :a-keyword)
```

### Keyword Arguments

By default, JSON-RPC 2.0
[requires](https://www.jsonrpc.org/specification#parameter_structures) that
keyword arguments be passed as "objects" (you might know them as dictionaries -
`{"keyword": "value"}`). This is not supported. In Elisp, you cannot reference
positional arguments by name and they may be mixed with keyword arguments.
Objects aren't compatible with that structure.

If you want to pass keyword arguments, you must encode them as a list:

```json
{
    "params": ["positional-arg",
               ":keyword1", "value1", 
               ":keyword2", "value2"]
}
``` 

#### Example: Symbols and Keyword Arguments

Here's an example of a request containing symbols and a keyword argument. Let's
say we want Emacs to [flash the line](https://github.com/rolandwalker/nav-flash)
after we scroll up, so we can keep track of the cursor.

In Elisp, we could do something like this:

```emacs-lisp
;; Advise the `scroll-up' function to call `nav-flash-show' afterwards.
(advice-add 'scroll-up :after 'nav-flash-show)
```

Here's how to encode that in a JSON-RPC call:

```json
{
    "jsonrpc": "2.0",
    "method": "advice-add",
    "params": ["'scroll-up" ":after" "'nav-flash-show"],
    "id": 29492,
}
```

This would be encoded into a string and passed to `jrpc-handle`. It will decode a function call similar to the following:

```emacs-lisp
(apply
 'advice-add
 '(switch-to-buffer
   :after
   save-current-buffer))
```

Expressed another way, this is equivalent to:

```emacs-lisp
(advice-add 'switch-to-buffer :after 'save-current-buffer)
```

(Please note that this would be terrible way to flash the line in actual Emacs.
Don't use it. You'd have to wait between each scroll press.)

## Installation

The package itself is named `json-rpc-server`. The intention is to publish this
on MELPA - hold out until they've accepted it. Once that happens:

```text
M-x package-install RET json-rpc-server RET
```

Once installed, require it with: 

```emacs-lisp
(require 'json-rpc-server)
```

## List of Transport Layers

If you want to actually make RPC calls to Emacs, you need to use a transport
layer. Here's a list:

| Project                                           | Protocol |
| -------                                           | -------- |
| [`Porthole`](http://www.github.com/jcaw/porthole) | HTTP     |

Have you written one? Open a pull request and I'll add it.

## FAQ

- <b>Is it compatible with older versions of JSON-RPC?</b>

  Yes. It should work fine with older JSON-RPC requests. However, they aren't
  officially supported and the response will still be JSON-RPC 2.0.

- <b>Does it support keyword arguments?</b>

  Yes, but not in the standard format. You may not pass them as objects. Pass
  them [as lists](#keyword-arguments), just like in Elisp.

- <b>How can I send a [vector, hash table, etc]?</b>

  [You can't](#other-types). You have to write an intermediate function that constructs these
  types from alists, strings, etc.

- <b>Does it support notifications?</b>

  No. All requests block until a value is returned (or an error occurs). This
  could be implemented at the transport level, if desired.

- <b>Does it support batch requests?</b>

  Yes. See the [batch requests](#example-batch-requests) example.

- <b>Can I run multiple servers at once?</b>

  `json-rpc-server` has a somewhat misleading name. It's not a server, it's a
  server-side implementation of the protocol. The transport layer can run as
  many servers as it likes.

- <b>Are you open to pull requests?</b>

  Yes! Please pull against the
  [develop](https://github.com/jcaw/json-rpc-server.el/tree/develop) branch.
