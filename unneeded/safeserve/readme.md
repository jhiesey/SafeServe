# miku

A tiny web dev DSL

## Example

    {-# LANGUAGE OverloadedStrings #-}
    
    import Network.Miku
    import Hack2.Handler.SnapServer
    
    main = run . miku $ get "/" (text "miku power")


## Installation

    cabal update
    cabal install miku
    cabal install hack2-handler-snap-server
    
    -- copy and paste the above example to myapp.hs
    
    runghc myapp.hs

check: <http://localhost:3000>

## Quick reference

<https://github.com/nfjinjing/miku/blob/master/src/Test/Test.hs>


## Routes

### Verbs
    
    {-# LANGUAGE OverloadedStrings #-}
    
    -- use - instead of $ for clarity
    import Air.Light ((-))
    import Prelude hiding ((-))
    
    import Network.Miku
    import Hack2.Handler.SnapServer
    
    main = run . miku - do

      get "/" - do
        -- something for a get request

      post "/" - do
        -- for a post request
    
      put "/" - do
        -- put ..
    
      delete "/" - do
        -- ..

### Captures

    get "/say/:user/:message" - do
      text . show =<< captures

    -- /say/miku/hello will output
    -- [("user","miku"),("message","hello")]


## Static

    -- public serve, only allows `./src`
    public (Just ".") ["/src"]

## Mime types

    -- treat .hs extension as text/plain
    mime "hs" "text/plain"

## Filters

    -- before takes a function of type (Env -> IO Env)
    before - \e -> do
      putStrLn "before called"
      return e
    
    -- after takes that of type (Response -> IO Response)
    after return

## Hack2 integration

### Use hack2 middleware

    import Hack2.Contrib.Middleware.SimpleAccessLogger
    
    middleware - simple_access_logger Nothing

### Convert miku into a hack2 application

    -- in Network.Miku.Engine
    
    miku :: MikuMonad -> Application


## Hints

* It's recommended to use your own html combinator / template engine. Try DIY with, e.g. [moe](http://github.com/nfjinjing/moe).
* [Example view using custom html combinator (moe in this case)](http://github.com/nfjinjing/miku/blob/master/src/Test/Moe.hs)
* When inspecting the request, use `ask` defined in `ReaderT` monad to get the `Hack2.Environment`, then use helper method defined in `Hack2.Contrib.Request` to query it.
* `Response` is in `StateT`, `html` and `text` are simply helper methods that update the state, i.e. setting the response body, content-type, etc.
* You do need to understand monad transformers to reach the full power of `miku`.
    
## Reference

* miku is inspired by [Rack](http://rack.rubyforge.org), [Rails](http://rubyonrails.org), [Ramaze](http://ramaze.net), [Happstack](http://happstack.com/) and [Sinatra](http://www.sinatrarb.com/).


<br/>

<p>
<a href="http://en.wikipedia.org/wiki/Hatsune_Miku"><img src="https://github.com/nfjinjing/miku/raw/master/ita.jpg"/></a>
</p>