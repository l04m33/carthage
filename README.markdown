Carthage
========

[![Build Status](https://travis-ci.org/l04m33/carthage.svg?branch=master)](https://travis-ci.org/l04m33/carthage)

What is it?
-----------

It's a TCP server framework written in Erlang. It mimics some web frameworks,
featuring middleware etc.

What's so cool about it?
------------------------

1. TCP streams are packed into messages for you. You can focus on processing
   network messages, and leave the receive & send & other messy socket stuff
   to the framework.

2. It works in a one-erlang-process-per-client style, maximizing throughput
   and concurrency, yet communication between processes is still easy.

3. It elaboratively supports a login stage for every connection. And the
   login stage is cleanly separated with normal network operations.

4. It also has middleware support. You can easily transform your network
   messages, or serialize model objects, however you like, in the
   middleware, and keep your business code clean.

5. The framework itself is dead simple.

Usage
-----

In your Erlang node, make sure the application `carthage` is started, and
then:

```erlang
    carthage:start({LoginHandler, LoginOpts},
                   {ClientHandler, ClientOpts},
                   {Middlewares, Env},
                   ListenPort, NumAcceptors)
```

* `LoginHandler` and `ClientHandler`: Your handler modules for login stage
                                      and normal operations, accordingly.

* `LoginOpts` and `ClientOpts`:       Configurations for your handler modules.
                                      Must be [proplists][1].

* `Middlewares`:                      List of middleware modules.

* `Env`:                              Environment settings. Can be accessed by all handlers and
                                      middlewares. Must be proplists.

* `ListenPort`:                       TCP port to listen on.

* `NumAcceptors`:                     Size of the acceptor pool.

If the `start` function returned successfully, you'll have a running
carthage instance.

For examples, see `demo/*.erl`.

[1]: http://www.erlang.org/doc/man/proplists.html

License
-------

The MIT License: http://l04m33.mit-license.org/
