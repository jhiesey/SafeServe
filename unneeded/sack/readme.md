Hack2: a Haskell Webserver Interface (V2)
========================================

UPDATE THIS: TOTALLY WRONG!
-------

Hack2 is a port of Ruby's [Rack](http://rack.rubyforge.org/) webserver interface.

Version
-------

> 2011.6.19

Introduction
------------

### Idea

Separation of concerns:

* hack2: interface spec
* hack2-middleware: building blocks
* hack2-handler: server backends

### Design

    type Application = Env -> IO Response

### Demo

    {-# LANGUAGE OverloadedStrings #-}

    import Hack2
    import Hack2.Contrib.Response (set_body_bytestring)
    import Hack2.Handler.SnapServer
    import Data.Default (def)

    app :: Application
    app = \env -> 
      return $ 
        set_body_bytestring "Hello World" $
          Response 200 [ ("Content-Type", "text/plain") ] def

    main = run app
    

Spec
----

### The Environment

* __requestMethod__: The HTTP request method, e.g. `GET`, `POST`.
* __scriptName__: The initial portion of the request URL‘s “path” that corresponds to the application object, so that the application knows its virtual “location”. This may be an empty string, if the application corresponds to the “root” of the server.
* __pathInfo__: The remainder of the request URL‘s “path”, designating the virtual “location” of the request‘s target within the application. This may be an empty string, if the request URL targets the application root and does not have a trailing slash. This value may be percent-encoded when I originating from a URL.
* __queryString__: The portion of the request URL that follows the ?, if any. May be empty.
* __serverName, serverPort__: When combined with scriptName and pathInfo, these variables can be used to complete the URL. Note, however, that `Host` in http field, if present, should be used in preference to serverName for reconstructing the request URL. serverName and serverPort can never be empty, and so are always required.
* __httpHeaders__: Variables corresponding to the client-supplied HTTP request headers (e.g. "Accept"). The presence or absence of these variables should correspond with the presence or absence of the appropriate HTTP header in the request. 
* __hackVersion__: The list of `Int`, representing this version of Hack
* __hackUrlScheme__: `HTTP` or `HTTPS`, depending on the request URL. 
* __hackInput__: The body of the request, enumerator style.
* __hackErrors__: The error stream.
* __hackHeaders__: None standard http headers.


### The Response

* __status__: This is an HTTP status. It must be greater than or equal to 100. 
* __headers__: The header must not contain a Status key, contain keys with : or newlines in their name, contain keys names that end in - or _, but only contain keys that consist of letters, digits, _ or - and start with a letter. The values of the header must be Strings, consisting of lines (for multiple header values) separated by “\n”. The lines must not contain characters below 037.
* __body__: The body of the response, enumerator style.

### Properties

* The __scriptName__, if non-empty, must start with /
* The __pathInfo__, if non-empty, must start with /
* One of __scriptName__ or __pathInfo__ must be set. __pathInfo__ should be / if __scriptName__ is empty. __scriptName__ never should be /, but instead be empty.


1 minute tutorial
-----------------

### update cabal

    cabal update
    
### install hack

    cabal install hack2

### install some hack helpers
  
    cabal install hack2-contrib

### pick a backend

    cabal install hack2-handler-snap-server

### Create a Hack app

put the following code in `Main.hs`

    {-# LANGUAGE OverloadedStrings #-}

    import Hack2
    import Hack2.Contrib.Response (set_body_bytestring)
    import Hack2.Handler.SnapServer
    import Data.Default (def)

    app :: Application
    app = \env -> 
      return $ 
        set_body_bytestring "Hello World 2" $ 
          def { headers = [ ("Content-Type", "text/plain") ] }

    main = run app


### run

    runghc Main.hs

It should be running on [http://127.0.0.1:3000](http://127.0.0.1:3000) now.

Middleware
-----------

### demo usage of middleware

put the following in `Main.hs`. This code uses the `URLMap` middleware to route both `/hello` and `/there` to the `say` application.

    {-# LANGUAGE OverloadedStrings #-}

    import Hack2
    import Hack2.Contrib.Response (set_body_bytestring)
    import Hack2.Handler.SnapServer
    import Data.Default (def)

    import Data.ByteString.Lazy.Char8 (pack)
    import Hack2.Contrib.Utils (empty_app)
    import Hack2.Contrib.Middleware.URLMap


    say :: Application
    say = \env -> return $ set_body_bytestring (pack $ show env) def

    app :: Application
    app = url_map [("/hello", say), ("/there", say)] empty_app

    main = run app

### create a middleware

inside Hack2.hs:

    type Middleware = Application -> Application

since Haskell has curry, middleware api can be of type

    Anything -> Application -> Application

just pass an applied middleware into a chain.

finally the source code of `Config.hs`:

    module Hack2.Contrib.Middleware.Config (config) where

    import Hack2

    config :: (Env -> Env) -> Middleware
    config alter app = \env -> app (alter env)


### Use the middleware stack

From `Hack2.Contrib.Utils`:

    -- usage: use [content_type, cache] app
    use :: [Middleware] -> Middleware
    use = reduce (<<<)

Handlers
--------

Once an application is written using Hack2, it should work on any web server that provides a Hack2 handler.

A handler should expose at least one function of type:

    run :: Application -> IO ()

Upgrade
-------

With every new release, any library links to Hack2 should be recompiled against the new version, usually as simple as:

    cabal install linked_lib --reinstall

Links
-----

### [Discuss:Web programming with Haskell](http://www.haskell.org/mailman/listinfo/web-devel)


