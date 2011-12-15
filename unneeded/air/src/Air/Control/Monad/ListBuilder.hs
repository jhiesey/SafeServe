module Air.Control.Monad.ListBuilder where


import Control.Monad.Writer
import Data.DList (DList, toList, singleton, fromList)

type ListBuilder a b = Writer (DList a) b

one :: a -> ListBuilder a ()
one = tell . singleton

many :: [a] -> ListBuilder a ()
many = tell . fromList

execListBuilder :: ListBuilder a b -> [a]
execListBuilder = toList . execWriter

list :: ListBuilder a b -> [a]
list = execListBuilder

runListBuilder :: ListBuilder a b -> (b, [a])
runListBuilder x = let (b, a) = runWriter x in (b, toList a)
