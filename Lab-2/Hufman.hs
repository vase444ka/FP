{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use first" #-}
module Hufman where

import Data.List (sortBy, isPrefixOf, insertBy, intersperse)
import qualified Data.Bifunctor (first)
import HufmanFiles (createMultipleStreams)


data HufTree a = HufLeaf a | HufBranch [a] (HufTree a) (HufTree a)  deriving (Eq, Ord, Show)


letterEncode :: Ord a => HufTree a -> a -> String
letterEncode tree e = fst $ helper "" tree e 
    where 
    helper pref (HufLeaf x) y = if x == y then (reverse pref, True)
                                          else (pref, False)
    helper pref (HufBranch arr left right) x =
                 if x `notElem` arr then (pref, False)
                                    else case helper ('0':pref) left x of 
                                            (pref, True) -> (pref, True)
                                            (_, False)   -> helper ('1':pref) right x


letterDecode :: Ord a => HufTree a -> String -> (a, String) 
letterDecode = decode' "" 
    where 
    decode' pref (HufLeaf a) _ = (a, pref) 
    decode' pref (HufBranch _ l r) [] = decode' pref l [] 
    decode' pref (HufBranch _ l r) (x:xs) = if x == '0' then decode' (x:pref) l xs
                                                        else decode' (x:pref) r xs

    


-- imported END ---


buildTreeFromFreq :: Ord a => [(a, Int)] -> HufTree a
buildTreeFromFreq dict  = combine $ map (\(x,y) -> (HufLeaf x, y))  $ sortBy cmpSnd dict where
            combine [] = error "Nothing to build ufman code from"
            combine [(x,_)] = x
            combine ((x, xn):(y, yn):xs) = combine $ insertBy cmpSnd (makeBranch x y, xn+yn) xs
            
            makeBranch l@(HufLeaf a) r@(HufLeaf b) = HufBranch (mergeArrays [a] [b]) l r
            makeBranch l@(HufBranch arr _ _) r@(HufLeaf b) = HufBranch (mergeArrays arr [b]) l r
            makeBranch l@(HufLeaf b) r@(HufBranch arr _ _) = HufBranch (mergeArrays arr [b]) l r
            makeBranch l@(HufBranch arr _ _) r@(HufBranch brr _ _) = HufBranch (mergeArrays arr brr) l r

            mergeArrays [] xs = xs
            mergeArrays xs [] = xs
            mergeArrays (x:xs) (y:ys) = if x <= y then x : mergeArrays xs (y:ys)
                                                  else y : mergeArrays ys (x:xs)
            
            cmpSnd = \(_, x) (_, y) -> compare x y



buildFrequncyDict :: Ord a => [a] -> [(a, Int)]
buildFrequncyDict = go [] where
    go dict [] = dict
    go dict (x:files) = go (addLetter dict x) files

    addLetter [] a          = [(a,1)]
    addLetter ((a,n):xs) b
                | a < b     = (a,n) : addLetter xs b
                | a == b    = (a,n+1) : xs
                | otherwise = (b,1) : (a,n) : xs

genCodes ::  [[String]] -> HufTree Char
genCodes files = buildTreeFromFreq . buildFrequncyDict . head $ createMultipleStreams 1 files