module Main where

import Hufman (HufTree, letterEncode, letterDecode, genCodes)
import HufmanFiles (createMultipleStreams, postProcessFiles)
import Data.List (isPrefixOf)
import GHC.Conc


files :: [[String]]
files = [["Hello", "Mama"], ["world"]]

encodeHufman :: Int -> [[String]] -> (String, HufTree Char)
encodeHufman n fs = let streams = createMultipleStreams n fs; 
                        codes = genCodes fs 
                    in (encode codes streams, codes)
    

encode :: Ord a => HufTree a -> [[a]] -> [Char]
encode _ [] = []
encode codes (x:xs) = part `par` main `pseq` (part ++ main) where 
    part = encodeThread "" codes x
    main = encode codes xs

    encodeThread res codes [] = res
    encodeThread res codes (x:str) = encodeThread (res ++ letterEncode codes x) codes str

decodeHufman :: HufTree Char -> String -> [[String]]
decodeHufman tree lst = postProcessFiles $ decode tree lst  
    where
    decode tree [] = []
    decode tree str = let (oneLetter, letterCode) = letterDecode tree str  in 
                            oneLetter : decode tree (drop (length letterCode) str)

main :: IO()
main = do
    let n = 4
    print "Hufman encoded: \n"
    let h = fst $ encodeHufman n files
    print h 

    print "Hufman efficiency: \n"
    let doubleLen = fromIntegral . length
    print $ (8 * doubleLen (concat (createMultipleStreams 1 files))) / doubleLen h
    
    print "Hufman decoded: \n"
    print $ uncurry (flip decodeHufman) $ encodeHufman n files