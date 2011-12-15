module Air.SimpleMath where


import Prelude ((+))
import Air.Env hiding ((+))
import qualified Data.Array as A
import qualified Data.List as L
import Control.Arrow ((&&&))
import Data.Char (digitToInt)

powerslice :: [a] -> [[a]]
powerslice xs = [ xs.slice j (j+i) |
  i <- l.downto 1,
  j <- [0..l <-> i]
  ]
  where l = xs.length


encode :: (Eq a) => [a] -> [(Int, a)]
encode xs = xs.L.group.map (length &&& head)

decode :: [(Int, a)] -> [a]
decode xs = xs.map(\(l,x) -> l.times x).join'

hist :: (Num e, A.Ix i) =>  (i, i) -> [i] -> A.Array i e
hist bnds ns = A.accumArray (+) 0 bnds [(n, 1) | n <- ns, A.inRange bnds n]


explode :: (Show a) => a -> [Int]
explode n = n.show.map digitToInt