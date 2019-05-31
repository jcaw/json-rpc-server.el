<p align=center>
    <img src="media/logo.png" alt="json-rpc-server logo" />
</p>

<h1 align=center>JSON-RPC-Server</h1>

<p align=center>Server-side implementation of the JSON-RPC 2.0 protocol for Emacs.</p>

---

## What is this Package?

This is a full implementation of the JSON-RPC 2.0 protocol for Emacs. This
package is designed to be independent of the specific transport mechanism for
RPC calls. No transport logic is included. You pass in a JSON string, functions
are executed and a JSON-RPC 2.0 response string is returned.

This package is intended to sit underneath a transport layer, which is
responsible for exposing the protocol to the outside world. JSON-RPC provides no
mechanism for authentication; Because of this, the transport layer should handle
authentication too.

The default transport layer uses the http protocol, and is available
[here](http://www.github.com/jcaw/http-rpc-server.el).

## Installation

It will be installable from MELPA once I persuade them to add it.

## List of Transport Layers

If you want to actually make RPC calls to Emacs, you need to use a transport
layer. Here's a list:

| Project         | Protocol | Link                                          |
| -------         | -------- | ----                                          |
| http-rpc-server | HTTP     | http://www.github.com/jcaw/http-rpc-server.el |

Have you written one? Open a pull request and I'll add it.
