module Air.Data.Record.SimpleLabel.TH (mkLabels, mkLabel) where

import Control.Monad
import Data.Char
import Language.Haskell.TH.Syntax

-- | Derive labels for all the record selector in a datatype.
mkLabels :: [Name] -> Q [Dec]
mkLabels = liftM concat . mapM mkLabel

mkLabel :: Name -> Q [Dec]
mkLabel n = do
    i <- reify n
    let -- only process data and newtype declarations
        cs' = case i of
                TyConI (DataD _ _ _ cs _)   -> cs
                TyConI (NewtypeD _ _ _ c _) -> [c]
                _ -> []
        -- we're only interested in labels of record constructors
        ls' = [ l | RecC _ ls <- cs', l <- ls ]
    return (map mkLabel1 ls')

mkLabel1 :: VarStrictType -> Dec
mkLabel1 (name, _, _) =
    -- Generate a name for the label:
    -- in this fork: label names are "__" + accesser name, e.g. if data Square = Square {length :: Double}, then label is __length
    let n = mkName $ "__" ++ nameBase name
    in FunD n [Clause [] (NormalB (
           AppE (AppE (VarE (mkName "label")) (VarE name)) -- getter
                (LamE [VarP (mkName "b"), VarP (mkName "a")] -- setter
                      (RecUpdE (VarE (mkName "a")) [(name, VarE (mkName "b"))]))
                                   )) []]

