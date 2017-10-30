import Data.Char (toLower, toUpper)
import Data.List.Split (splitOn)
import Data.List (intercalate)

titleCase :: String -> String -> String
titleCase _ "" = ""
titleCase minor title = intercalate " " $ cap h : (cap' <$> t) where
    (h:t) = splitOn " " title
    lower = (toLower <$>)
    minors = lower <$> (splitOn " " minor)
    cap (x:xs) = toUpper x : (lower xs)
    cap' xs = let l = lower xs in if elem l minors then l else cap xs

-- titleCase "a an the of" "a clash of KINGS" == "A Clash of Kings"
-- titleCase "The In" "THE WIND IN THE WILLOWS" == "The Wind in the Willows"
-- titleCase "" "the quick brown fox" == "The Quick Brown Fox"