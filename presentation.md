% SafeServe: User-Editable Websites in Haskell

# The Goal

Make it possible to edit a website from within a web browser

* WordPress (and other CMSs) give you this, but they aren't flexible

* Allowing anyone to edit sites in Python, PHP, etc. is problematic

* Why not use SafeHaskell to make this work better?


# How?

* Start with a simple web framework: [Miku](https://github.com/nfjinjing/miku)

* Provide an [editor](http://localhost:3000/edit/Newsite)

* Dynamically compile and link the code into the running server process using the [plugins package](http://hackage.haskell.org/package/plugins-1.5.1.4) and SafeHaskell


# Dynamic Compilation and Loading

~~~~ {.haskell}
make :: FilePath -> [Arg] -> IO MakeStatus

data LoadStatus a
        = LoadSuccess Module a
        | LoadFailure Errors

pdynload_ :: FilePath -> [FilePath] -> [PackageConf] -> [Arg] -> Type -> Symbol -> IO (LoadStatus a)
~~~~

* Type safety?

* Does that break SafeHaskell's safety?

# Ok, so what does the interface look like?

[See here](http://localhost:3000/edit/Newsite)


# RIO Monad
As in lecture, except for relative pathnames for file access

~~~~ {.haskell}
newtype RIO a = UnsafeRIO { runRIO :: String -> IO a }

instance Monad RIO where
  return x = UnsafeRIO (\_ -> return x)
  m >>= k = UnsafeRIO $ \path -> (runRIO m path >>= \x -> runRIO (k x) path)
  
writeFileRIO :: String -> String -> RIO ()
writeFileRIO name contents = UnsafeRIO $ \path -> do
cwd <- getCurrentDirectory
let absPath = normalise $ cwd ++ "/plugins/" ++ path ++ "/" ++ name
writeFile absPath contents
  
~~~~

# Experiences with SafeHaskell

* Some packages export a few unsafe symbols, e.g. Data.ByteString.Char8
	Wrap these

* Others, such as Data.Default, are essentially safe but can't be compiled with -XSafe due to their imports
	Recompile these with -XTrustworthy
	
	
# What's Next?

* Flesh out the API

* Make the RIO monad more powerful

* Database access (it's just flat files at this point)

* Allow sites to import each other

* Client-side security

* Support full packages