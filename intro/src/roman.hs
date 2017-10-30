import Data.List

solution :: Integer -> String
solution n = intercalate "" (reverse (encode <$> (reversed `zip` codes))) where
    reversed :: [Char]
    reversed = reverse $ show n
    codes :: [ (Char -> String) ]
    codes = encode1 <$> [ "IVX", "XLC", "CDM", "M-*" ]
    encode :: (Char, Char -> String) -> String
    encode (digit, romans) = romans digit
    encode1 :: String -> (Char -> String)
    encode1 [one, five, ten] = 
        \d -> case d of
            '0' -> []
            '1' -> [ one ]
            '2' -> [ one, one ]
            '3' -> [ one, one, one]
            '4' -> [ one, five ]
            '5' -> [ five ]
            '6' -> [ five, one ]
            '7' -> [ five, one, one ]
            '8' -> [ five, one, one, one ]
            '9' -> [ one, ten ]
