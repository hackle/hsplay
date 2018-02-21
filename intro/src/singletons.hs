{-# LANGUAGE TemplateHaskell, QuasiQuotes, PolyKinds, DataKinds, TypeFamilies #-}

import Data.Singletons
import Data.Singletons.TH

-- $(singletons [d|
--     data Nat = Zero | Succ Nat
--     pred :: Nat -> Nat
--     pred Zero = Zero
--     pred (Succ n) = n
--     |])