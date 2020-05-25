#! /usr/bin/env stack
import System.Random
import Control.Monad

randIO :: Int -> Int -> IO Int
randIO x y = randomRIO (x, y) :: IO Int

{- 死骸、いつかのために残す
select :: Int -> Int -> [Int] -> IO [Int]
select range randrange selects = do
    rand <- randIO 0 randrange
    case range of
        0 -> return selects
        otherwise -> select (range - 1) randrange (rand:selects)
-}

--母音表
vowels :: Int -> [String] -> [String] -> IO [String]
vowels kinds vows void = do --母音の種類と母音表と空のリスト
    select <- randIO 1 (length vows)
    select2 <- randIO 1 (length vows)
    vowslength <- randIO 1 2 --二重母音までにしました。疲れたので三重母音とか言わないで、てか日本語話者は使えないよ
    if kinds == length void
        then return void
        else if kinds > length void && vowslength == 1 && notElem (vows!!(select-1)) void
            then vowels kinds vows ((vows!!(select-1)):void)
        else if kinds > length void && vowslength == 2 && notElem ((vows!!(select-1))++(vows!!(select2-1))) void && (vows!!(select-1)) /= (vows!!(select2-1))
            then vowels kinds vows (((vows!!(select-1))++(vows!!(select2-1))):void)
        else vowels kinds vows void

--子音表
consonants :: Int -> [String] -> [String] -> IO [String]
consonants kinds con void = do
    select <- randIO 1 (length con)
    if kinds == length void
        then return void
        else if kinds > length void && notElem (con!!(select-1)) void
           then consonants kinds con ((con!!(select-1)):void)
        else consonants kinds con void

--音節構造自動生成器(ランダム)
sylgen :: [String] -> [String] -> IO [String] 
sylgen symbol void = do
    ini <- randIO 1 (length symbol)
    case length void of
        0 -> sylgen symbol ((symbol!!ini):void)
        _ -> syl1 void symbol

syl1 :: [String] -> [String] -> IO [String]
syl1 input symbol = do
    maxrand <- randIO 1 6
    if length (input!!(length input - 1)) == maxrand then syl3 input symbol
    else do
        select <- randIO 0 1
        case select of
            0 -> sylgen symbol (((symbol!!0) ++ (input!!(length input - 1))):input)
            1 -> sylgen symbol (((symbol!!1) ++ (input!!(length input - 1))):input)

syl3 :: [String] -> [String] -> IO [String]
syl3 input symbol = if 'V' `elem` (input!!(length input - 1)) then return input else sylgen (init input) symbol


main = do
    print =<< vowels 6 ["a", "i", "u", "e", "o"] []
    print =<< consonants 4 ["b", "c", "d", "f", "g", "h", "j", "k", "l", "m", "n", "p", "q", "r", "s", "t", "v", "w", "x", "y", "z"] []
    print =<< sylgen ["C", "V"] []