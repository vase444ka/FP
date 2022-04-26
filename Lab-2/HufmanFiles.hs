{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use first" #-}
module HufmanFiles where


eof :: Char
eof = '\0'

eol :: Char
eol = '\n'

splitEveryLimit :: Int -> [a] -> Int -> [[a]]
splitEveryLimit _ [] m = []
splitEveryLimit _ xs 1 = [xs]
splitEveryLimit n list m = first : splitEveryLimit n rest (m-1)
  where
    (first,rest) = splitAt n list

subdivideArray :: Int -> [a1] -> [[a1]]
subdivideArray m arr = let n = length arr; p = div n m in
        splitEveryLimit p arr m

createMultipleStreams :: Int -> [[String]] -> [String]
createMultipleStreams n lst = subdivideArray n (preprocess' "" lst) where
    preprocess' res [] = reverse res
    preprocess' res ([]:files) = preprocess' (eof:res) files
    preprocess' res (([]:file):files) = preprocess' (eol : res) (file:files)
    preprocess' res (((x:line):file):files) = preprocess' (x : res) ((line:file):files)

postProcessFiles :: String -> [[String]]
postProcessFiles = post [] [] [] where
    post files file line [] = reverse files
    post files file line (x:xs) | x == eol = post files (reverse line : file) [] xs
                                | x == eof = post (reverse file : files) [] [] xs
                                | otherwise = post files file (x:line) xs