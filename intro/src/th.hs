module Kata.TupleMaker (tuple) where 
{-# LANGUAGE TemplateHaskell #-}
import Language.Haskell.TH
 
tuple :: Int -> Q Exp
tuple 0 = return $ TupE []
tuple 1 = do { a <- newName "a"; return $ LamE [VarP a] (VarE a) }
tuple n = do
    ns <- mapM newName (("a"++).show <$> [1..n])
    return $ LamE (VarP <$> ns) (TupE (VarE <$> ns))