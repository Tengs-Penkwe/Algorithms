import Data.List (maximumBy, filter, elemIndex, isPrefixOf)
import Data.Ord (comparing)
import Data.Maybe (fromMaybe, fromJust)

-- | Lempel Ziv Compression Algorithm
--
-- >>> lempelZiv "AAABABBBBAABBBB"
-- [(0,'A'),(1,'A'),(0,'B'),(1,'B'),(3,'B'),(3,'A'),(4,'B'),(5,'\NUL')]
-- >>> lempelZiv "A"
-- [(0,'A')]
lempelZiv :: String -> [(Integer, Char)]
lempelZiv str = lempelZiv' str [] [[]]

-- Message, Compressed Message, Dictionary, Return Compressed Message
lempelZiv' :: String -> [(Integer, Char)] -> [String] -> [(Integer, Char)]
lempelZiv' [] compressed _ = compressed
lempelZiv' msg compressed dict =
    case drop (length prefix) msg of 
        [] -> compressed ++ [(index, '\0')]
        (ch:tail) -> lempelZiv' tail (compressed ++ [(index, ch)]) (dict ++ [prefix ++ [ch]])
    where
        prefix = fromMaybe [] (findLongestMatch msg dict)
        index = fromIntegral $ fromJust $ elemIndex prefix dict

findLongestMatch :: String -> [String] -> Maybe String
findLongestMatch _ [] = Nothing
findLongestMatch a xs = 
    let prefixes = filter (`isPrefixOf` a) xs
    in if null prefixes
        then Nothing
        else Just $ maximumBy (comparing length) prefixes

