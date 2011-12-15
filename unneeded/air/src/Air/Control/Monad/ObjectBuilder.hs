module Air.Control.Monad.ObjectBuilder where


import Control.Monad.State

import Air.Data.Default

type ObjectBuilder a = State a ()

execObjectBuilder :: s -> ObjectBuilder s -> s
execObjectBuilder s m = execState m s

object :: (Default s) => ObjectBuilder s -> s
object = execObjectBuilder def
