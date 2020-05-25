#! /usr/bin/env stack
import System.Random

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

vowels :: Int -> [String] -> [String] -> IO [String]
vowels kinds vows void = do --母音の種類と母音表と空のリスト
    select <- randIO 0 (length vows)
    select2 <- randIO 0 (length vows)
    vowslength <- randIO 1 2 --二重母音までにしました。疲れたので三重母音とか言わないで、てか日本語話者は使えないよ
    if kinds == length void
        then return void
        else if kinds > length void && vowslength == 1
            then vowels kinds vows ((vows!!select):void)
        else if kinds > length void && vowslength == 2
            then vowels kinds vows (((vows!!select)++(vows!!select2)):void)
        else return []

main = do
    print =<< vowels 6 ["a", "i", "u", "e", "o"] []