# javascript-bridge [![Build Status](https://img.shields.io/travis/ku-fpg/javascript-bridge.svg?style=flat)](https://travis-ci.org/ku-fpg/javascript-bridge)

**javascript-bridge** is an easy way of calling JavaScript from
Haskell, using web-sockets as the underlying transport
mechanism. Conceptually, javascript-bridge works be giving
Haskellacccess to the JavaScript `eval` function.  However, using a
remote monad, we support evaluation of JavaScript fragments, calling
and returning values from JavaScript functions, constructing
and using remote objects, and sending events from JavaScript
to Haskell.

# High-level overview of API

**javascript-bridge** remotely executes JavaScript *fragments*.
The basic Haskell idiom is.
```Haskell
  send eng $ command "console.log('Hello!')"
```
where `send` is an `IO` function that sends a commands for remote exection,
`eng` is a handle into a specific JavaScript engine,
and `command` is a command builder.

There are also ways synchronously sending a `procedure`,
that is a JavaScript expression that constructs a value,
then returns the resulting value.

```Haskell
  do xs :: String <- send eng $ procedure "new Date().toLocaleTimeString()"
     print xs
```

There are ways of creating remote values, for future use,
where Haskell has a handle for this remote value.

```Haskell
data Date = Date -- phantom

  do t :: RemoteValue Date <- send eng $ constructor "new Date()"
     send eng $ procedure $ "console.log(" <> var t <> ".toLocaleTimeString())"
```

Finally, there is a way of sending events from JavaScript,
then listening for the event in Haskell.

```Haskell
  do send eng $ command "jsb.event('Hello!')"
     e :: String <- listen eng
     print e
```

# Bootstrapping

Bootstrapping the connection is straightforward.
First, use a `middleware` to setup the (Haskell) server.

```Haskell
import qualified Network.JavaScript as JS

        ...
        scotty 3000 $ do
          middleware $ JS.start app
	  ...

app :: Engine -> IO ()
app = send eng $ command "console.log('Hello!')"
```

Next, include the following fragment in your HTML code,
replacing *localhost* with your web address.

```HTML
    <script>
        jsb = new WebSocket('ws://localhost:3000/');
        jsb.onmessage = function(evt){ eval(evt.data);};
    </script>
```

That's it!
