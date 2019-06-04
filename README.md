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
is included.</b> A separate transport layer needs to be used to communicate with
external clients. Since JSON-RPC provides [no inbuilt
mechanism](https://groups.google.com/d/msg/json-rpc/PN462g49yL8/DdMa93870_oJ) for
authenticating requests, the transport layer should also handle authentication.

The default transport layer uses the http protocol, and is available
[here](http://www.github.com/jcaw/http-rpc-server.el).


---


<!-- markdown-toc start - Don't edit this section. Run M-x
     markdown-toc-refresh-toc Please note that the markdown generator doesn't
     work perfectly with a centered heading, as above. It will need manual
     tweaking -->

### Table of Contents

- [How it Works](#how-it-works)
  - [Examples](#examples)
    - [Example: Calling a Method](#example-calling-a-method)
    - [Example: Invalid Request](#example-invalid-request)
    - [Example: Malformed JSON](#example-malformed-json)
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

`jrpc-handle` is the main entry point into the package. Functions in this
package are prefixed with `jrpc-`.

```emacs-lisp
;; This will decode a JSON-RPC 2.0 request, execute it, and return the JSON-RPC 2.0 response.
(jrpc-handle string-encoded-json-rpc-request)
```

If an error occurs, the response will be a string containing a JSON-RPC 2.0
error response.

Only functions you have specifically exposed can be called via RPC. To expose a
function, call `jrpc-expose function`. For example:

```emacs-lisp
;; This will allow the `+' function to be called via JSON-RPC.
(jrpc-expose-function '+)
```

You can also expose functions manually by adding them to `jrpc-exposed-functions`.

### Examples

#### Example: Calling a Method

Encode a request according to the JSON-RPC 2.0 protocol. `method` should be the method name, as a string.

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
}")
```

`json-rpc-server` will decode the request, then apply the function `+` to the
list `'(1, 2, 3)`. Here's what the result of `jrpc-handle` will be:

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
}")
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

If there is a problem with the request (or another error occurs), a
`jrpc-handle` will encode a JSON-RPC 2.0 error response. Here's an example.

Let's try some malformed JSON:

```json
{Szx. dsd}
```

The call to `jrpc-handle`:

```emacs-lisp
(jrpc-handle "{Szx. dsd}")
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
                "json-string-format": ["doesn't start with `\"'!"]
            }
        }
    },
    "id": null
}
```

Note the `"data"` field. Some responses are triggered by an underlying error in
the Elisp, which may contain more meaningful information about the error. When
possible, the contents of that error will be returned in the
`"underlying-error"` field. If there is no underlying error, this field will not
be present.

## Datatype Limitations

The structure of JSON limits the types of variables that can be transferred.
JSON only contains six datatypes. Thus, functions exposed by this protocol
<b>must expect certain datatypes.</b> 

The datatypes are mapped as follows:

| JSON Datatype             | JSON Examples             | Decodeded Elisp Datatype | Elisp Examples          |
| ---                       | ---                       | ---                      | ---                     |
| string                    | `"string"`                | string                   | `"string"`              |
| [quoted string](#symbols) | `"'symbol"`, `":keyword"` | symbol                   | `'symbol`, `:keyword`   |
| number                    | `21`, `3.14`              | integer or float         | `21`, `3.14`            |
| boolean                   | `true`, `false`           | `t` or `:json-false`     | `t`, `:json-false`      |
| null                      | `null`                    | `nil`                    | `nil`                   |
| object                    | `{"Key": "Value"}`        | alist                    | `'(("Key" . "Value"))'` |
| array                     | `[1, 2, 4, 7]`            | list                     | `'(1 2 4 7)`            |

You may notice that "quoted strings" are decoded differently to normal strings.
See the [Symbols](#symbols) section for a full explanation.

### Other Types

Because of these type limitations, you cannot transfer vectors, plists, hash tables,
cl-structs, etc.

There is no easy way around this. JSON-RPC provides simplicity, at the cost of
flexibility. If you want to call a function that expects a different type, you
must write an intermediary function that translates from the available ones and
publish that instead.

### Symbols

Symbols are important in Elisp. Luckily, by abusing the JSON-RPC syntax we can
transfer symbols. Strings beginning with a single quote will be decoded into
symbols. Strings that start with a single colon will be decoded into keywords.

For example:

- The string `"'a-symbol"` becomes the symbol `'a-symbol`.
- The string `":a-keyword"` becomes the symbol `:a-keyword`.
- `"'wrapped-string'"` does not change. It will stay a string.

Let's send a list:

```json
["a string", "'a-symbol", ":a-keyword"]
```

That list will be decoded into:

```emacs-lisp
'("a string" 'a-symbol :a-keyword)
```

### Keyword Arguments

By default, JSON-RPC requires that keyword arguments be passed as objects
(`{"keyword": "value"}`). This is not supported. Elisp takes a mixture of named
and keyword arguments, and arguments are lists. Thus `"params"` should always be
a list - never an object.

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
    "params": [ ":after" "'nav-flash-show"],
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

The result of this function call will be returned in a JSON-RPC 2.0 object
(encoded into a string).

## Installation

The package itself is named `json-rpc-server`. The intention is to publish this
on MELPA - hold out until they've accepted it. Once that happens:

```text
M-x pacakge-install RET json-rpc-server RET
```

Once installed, require it with: 

```emacs-lisp
(require 'json-rpc-server)
```

## List of Transport Layers

If you want to actually make RPC calls to Emacs, you need to use a transport
layer. Here's a list:

| Project                                                            | Protocol |
| -------                                                            | -------- |
| [`http-rpc-server`](http://www.github.com/jcaw/http-rpc-server.el) | HTTP     |

Have you written one? Open a pull request and I'll add it.

## FAQ

- <b>Does it support batch requests?</b> Yes. Pass in an encoded list of
  requests to execute each in turn.
- <b>Is it compatible with older versions of JSON-RPC?</b> Yes. It should accept
  and work fine with older JSON-RPC requests. However, they aren't officially
  supported and the response will still be JSON-RPC 2.0.
- <b>Does it support keyword arguments</b> Not currently, no. Support will be
  added for this in the future.
- <b>How can I send a [vector, hash table, etc]?</b> You can't. You have to
  write an intermediate function that constructs these types from alists,
  strings, etc.
- <b>Does it support notifications?</b> No. All requests block until a value is
  returned (or an error occurs). This could be implemented at the transport
  level, if desired.
- <b>Can I run multiple servers at once?</b> No. One server per session. This
  could be added in the future if it's a feature people really want.
- <b>Are you open to pull requests?</b> Yes!
