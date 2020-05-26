#! /usr/bin/env stack
import System.Random
import Control.Monad

randIO :: Int -> Int -> IO Int
randIO x y = randomRIO (x, y) :: IO Int

fact 0 = 1
fact n = fact(n-1) * n

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
sylgen :: Int -> [String] -> IO [String]
sylgen maxsyllable void = do
    let symbol = ["C", "V"]
    select <- randIO 0 1
    if length void == 0 
        then do
            max <- randIO 1 maxsyllable --音節構造の大きさの範囲を指定するとこ
            sylgen max ((symbol!!select):void)
        else if (length (head void)) == maxsyllable then syl maxsyllable void
            else if (head void)!!0 == 'V' then sylgen maxsyllable ((init (head (((symbol!!0) ++ head void):(init void)))):(init void))
            else case select of
                    0 -> sylgen maxsyllable (((symbol!!0) ++ head void):(init void))
                    1 -> sylgen maxsyllable (((symbol!!1) ++ head void):(init void))

syl :: Int -> [String] -> IO [String]
syl maxsyllable input = if (elem 'V' (input!!(length input - 1)) && elem 'C' (input!!(length input - 1))) || elem 'V' (input!!(length input -1)) then return input else sylgen maxsyllable []

sylsets :: Int -> Int -> [String] -> IO [String]
sylsets kinds maxsyllable void = if kinds > (fact maxsyllable)^2 then sylsets kinds (maxsyllable+1) void --maxsyllableの値が音節構造の組み合わせ未満のとき
    else do
        sylgen <- sylgen maxsyllable []
        if length void == kinds then return void
            else if head sylgen `notElem` void then sylsets kinds maxsyllable (sylgen ++ void)
            else sylsets kinds maxsyllable void

--単語の雛形

prewordgen :: Int -> [String] -> [String] -> IO [String]
prewordgen syllableculster syllable void = do
    select <- randIO 1 (length syllable)
    if length void == 0 then prewordgen syllableculster syllable (syllable!!(select-1):void)
        else if length void == syllableculster then preword void []
        else prewordgen syllableculster syllable (syllable!!(select-1):void)


preword :: [String] -> [String] -> IO [String]
preword input output = if length input == 0 then return output else if length output == 0 then preword (tail input) ((head input):output) else preword (tail input) ((head output ++ head input):(init output))

prewordsets :: Int -> Int -> [String] -> [String] -> IO [String]
prewordsets kinds syllablerange syllable void = do
    syllableculster <- randIO 1 syllablerange
    prewordgen <- prewordgen syllableculster syllable []
    if length void == kinds then return void
        else prewordsets kinds syllablerange syllable (prewordgen ++ void)

--単語生成
{-
wordgen :: [String] -> [String] -> [String] -> [String] -> IO [String]
wordgen syllable vowel consonat void = do
-}



main :: IO()
main = do
    putStrLn "母音一覧"
    print =<< vowels 8 ["a", "i", "u", "e", "o"] []
    putStrLn "子音一覧"
    print =<< consonants 12 ["b", "c", "d", "f", "g", "h", "j", "k", "l", "m", "n", "p", "q", "r", "s", "t", "v", "w", "x", "y", "z"] []
    putStrLn "音節構造一覧"
    sylsets <- sylsets 10 4 []
    print $ sylsets
    putStrLn "単語の雛形(語の音節構造)"
    print =<< prewordgen 3 sylsets []
    putStrLn "単語の雛形(語の音節構造)一覧"
    prewordsets <- prewordsets 30 5 sylsets []
    print $ prewordsets