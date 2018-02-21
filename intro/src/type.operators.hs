{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE OverloadedStrings #-}

import Data.Text

data UserVar = UserName | UserEmail deriving Show
data I a = I { unI :: a } deriving Show
data Var a x = Var { unK :: a } deriving Show

infixr 8 +
data (f + g) a = InL (f a) | InR (g a) deriving Show

email :: [ (Var UserVar + I) Text ]
email = [
    InR (I "Dear ")
    , InL (Var UserName)
    , InR (I ", Thank you for your recent email to Santa & Santa Inc.")
    ]
