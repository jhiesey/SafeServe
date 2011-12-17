SafeServe
========================================

# To Install

This is a bit hacky.  I know I should be updating versions/submit these patches/etc.  The cabal file isn't really set up right either, since I'm a bit short on time, so here it is in English:

* Install ghc 7.2.2

* Download data-default (http://hackage.haskell.org/package/data-default) and apply the patch ../data-default.diff and install.  This enables -XTrustworthy. This looks to be safe to me.

* Download hs-plugins (http://hackage.haskell.org/package/plugins-1.5.1.4) and apply the patch ../plugins.diff and install.  This makes it compile with ghc 7.2.2

* Hopefully the default versions of everything else work.  I haven't had much of a chance to test this; I just know it works on my Mac (32 bit).

* Go to safeblaze dir and run cabal install

* Go to safebase dir and run cabal install

* Run cabal install here

NOTE ALSO ALTERNATIVE AT THE BOTTOM!

# To Run

run the command "safeserve"

Go to http://localhost:3000/edit/Default to get started




# ALTERNATIVELY:

After installing the prereq packages,

just run

runghc -iapi SafeServe.hs

in this directory.  Should work!