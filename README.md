# javascript-bridge [![Build Status](https://github.com/ku-fpg/javascript-bridge/workflows/Haskell-CI/badge.svg)](https://github.com/ku-fpg/javascript-bridge/actions?query=workflow%3AHaskell-CI)

**javascript-bridge** is a straightforward way of calling JavaScript
from Haskell, using web-sockets as the underlying transport
mechanism. Conceptually, javascript-bridge gives Haskell acccess to
the JavaScript `eval` function.  However, we also support calling and
returning values from JavaScript functions, constructing and using
remote objects, and sending events from JavaScript to Haskell, all
using a remote monad.

# Overview of API

**javascript-bridge** remotely executes JavaScript *fragments*.
The basic Haskell idiom is.
```Haskell
     send eng $ command "console.log('Hello!')"
```
where `send` is an `IO` function that sends a commands for remote execution,
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
  do -- Have JavaScript send an event to Haskell
     send eng $ command $ event ('Hello!'::String)"
     -- Have Haskell wait for the event, which is an Aeson 'Value'.
     e :: Value <- listen eng
     print e
```

# Bootstrapping

Bootstrapping the connection is straightforward.
First, use a `middleware` to setup the (Haskell) server.

```Haskell
import Network.JavaScript

        ...
        scotty 3000 $ do
          middleware $ start app
          ...

app :: Engine -> IO ()
app eng = send eng $ command "console.log('Hello!')"
```

Next, include the following fragment in your HTML code.

```HTML
    <script>
        window.jsb = {ws: new WebSocket('ws://' + location.host)};
        jsb.ws.onmessage = (evt) => eval(evt.data);
    </script>
```

That's it!
