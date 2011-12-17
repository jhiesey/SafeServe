SafeServe
========================================

# To Install

This is a bit hacky.  I know I should be updating versions/submit these patches/etc.  The cabal file isn't really set up right either, since I'm a bit short on time, so here it is in English:

* Install ghc 7.2.2

* NOTE: for patches, go into the dir, then run

patch -Np1 < patchfile

* Download data-default (http://hackage.haskell.org/package/data-default) and apply the patch ../data-default.diff and install.  This enables -XTrustworthy. This looks to be safe to me.

* Download hs-plugins (http://hackage.haskell.org/package/plugins-1.5.1.4) and apply the patch ../plugins.diff and install.  This makes it compile with ghc 7.2.2

* Hopefully the default versions of everything else work.  I haven't had much of a chance to test this; I just know it works on my Mac (32 bit).

* Go to safeblaze dir and run cabal install

* Go to safebase dir and run cabal install

* Run cabal build (or cabal install) here

# THEN:

After building and installing,

just run

runghc -iapi SafeServe.hs

in this directory.  Should work!

Somehow the cabal install doesn't quite get things right on Ubuntu.  Let me know if I need to fix this.  I suspect it's some nastiness with getting linker paths right.
