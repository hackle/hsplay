build :: String -> Int -> String -> String
build pre len suf = pre ++ (show len) ++ suf

result :: Maybe String
result = fmap length (Just [1..5]) >>= \len -> return (build "foo" len "bar")

result1 :: Maybe String
result1 = (flip (build "foo") "bar") . length <$> Just [1..5]