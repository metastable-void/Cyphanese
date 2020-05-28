#! /usr/bin/env stack
import System.Random
import qualified Data.List as L
import qualified Data.Map as Map

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
sylsets kinds maxsyllable void = do
        sylgen <- sylgen maxsyllable []
        if length void == kinds then return void
            else if notElem (head sylgen) void then sylsets kinds maxsyllable (sylgen ++ void)
            else sylsets kinds (maxsyllable+1) void

--単語の雛形

prewordgen :: Int -> [String] -> [String] -> IO [String]
prewordgen syllableculster syllable void = do
    select <- randIO 1 (length syllable)
    if length void == 0 then prewordgen syllableculster syllable (syllable!!(select-1):void)
        else if length void == syllableculster then preword void []
        else prewordgen syllableculster syllable (syllable!!(select-1):void)

preword :: [String] -> [String] -> IO [String]
preword input output = if length input == 0 then return output
                    else if length output == 0 then preword (tail input) ((head input):output) else preword (tail input) ((head output ++ head input):(init output))

prewordsets :: Int -> Int -> [String] -> [String] -> IO [String]
prewordsets kinds syllablerange syllable void = do
    syllableculster <- randIO 1 syllablerange
    prewordgen <- prewordgen syllableculster syllable []
    if length void == kinds then return void
        else prewordsets kinds syllablerange syllable (prewordgen ++ void)

--単語生成

wordgen :: [String] -> [String] -> [String] -> [String] -> IO [String]
wordgen preword vowel consonant void = do
    selectcon <- randIO 0 (length consonant-1)
    selectvow <- randIO 0 (length vowel-1)
    let con = (consonant!!selectcon)
    let vow = (vowel!!selectvow)
    if length void == length (head preword) then word void []
    else case length void of
            0 -> case (head preword)!!0 of
                    'V' -> wordgen preword vowel consonant (vow:void)
                    'C' -> wordgen preword vowel consonant (con:void)
            _ -> case (head preword)!!(length void) of
                    'V' -> wordgen preword vowel consonant (void ++ (vow:[]))
                    'C' -> if (last void) == con then wordgen preword vowel consonant void else wordgen preword vowel consonant (void ++ (con:[]))

word :: [String] -> [String] -> IO [String]
word input output = if length input == 0 then return output
                    else if  length output == 0 then word (tail input) ((head input):output) else word (tail input) ((head output ++ head input):(init output))

wordsets :: [String] -> [String] -> [String] -> [String] -> IO [String]
wordsets prewords vowel consonant void =
    if length void == length prewords then return void
        else do
            let preword = (prewords!!(length void)):[]
            wordgen <- wordgen preword vowel consonant []
            wordsets prewords vowel consonant (void ++ wordgen)

--文の生成
sentgen :: [String] -> [String] -> String
sentgen wordlist void
        | length wordlist == 0 = head void
        | length void == 0 = sentgen (tail wordlist) ((head wordlist):void)
        | otherwise = sentgen (tail wordlist) ((head wordlist ++ " " ++ head void):init void)

--ユーフォニー指数
euphony :: Double -> Double -> Double -> Double -> Int -> Double
euphony alpha beta gamma delta epsilon
        | epsilon == 2 = -0.04*(e0**2) + 1.4*e0
        | otherwise = e0
    where
        e0 = euphony0 alpha beta gamma delta epsilon

euphony0 :: Double -> Double -> Double -> Double -> Int -> Double
euphony0 alpha beta gamma delta epsilon = 0.5 * (1 + (1 / (1 + exp (0.5*alpha - 7)))) * (100 / (1 + exp (-2.26*alpha - 0.0693*beta + 0.0112*gamma + 0.388*delta - 11.9)))

wordsize :: [String] -> [Int] -> [Int]
wordsize wordlist void
    | length wordlist == 0 = void
    | otherwise = wordsize (tail wordlist) (void ++ (length (head wordlist)):[])

mean :: [Double] -> Double --算術平均
mean ns = (sum ns) / (fromIntegral $ length ns)

mode :: [Int] -> [Int] --最頻値
mode [] = []
mode ns =
    let l = Map.fromListWith (\n m -> n + m) $ map (\x -> (x, (1::Double))) ns
        a = foldr1 (\x acc -> if x > acc then x else acc) $ Map.elems l
    in Map.keys $ Map.filter (==a) l

alpha :: [String] -> Double
alpha wordlist = mean (map fromIntegral $ wordsize wordlist [])

beta0 :: [String] -> [String] -> [String] -> Int
beta0 wordlist consonants void
    | length wordlist == 0 = length void
    | elem (((head wordlist)!!0):[]) consonants && elem (((head wordlist)!!1):[]) consonants = beta0 (tail wordlist) consonants ((head wordlist):void)
    | otherwise = beta0 (tail wordlist) consonants void

beta :: [String] -> [String] -> Double
beta wordlist consonants = (fromIntegral $ beta0 wordlist consonants []) / (fromIntegral $ length wordlist)

gamma0 :: [String] -> [String] -> [String] -> Int -> Int -> Int
gamma0 wordlist consonants void1 void2 void3
    | length wordlist == 0 = length void1
    | length (head wordlist) < 3 = gamma0 (tail wordlist) consonants void1 void2 void3
    | elem (head wordlist) ("s":[]) == False = gamma0 (tail wordlist) consonants (void1 ++ ((head wordlist):[])) void2 void3
    | otherwise = if ((head wordlist)!!void2) == 's'
                    then
                        if ((head wordlist)!!(void2+1)) /= (head (consonants!!void3)) then gamma0 (tail wordlist) consonants void1 void2 (void3+1)
                           else if ((head wordlist)!!(void2+2)) == 'r' || ((head wordlist)!!(void2+2)) == 'l' || ((head wordlist)!!(void2+2)) == 'h' then gamma0 (tail wordlist) consonants void1 0 0
                                else gamma0 (tail wordlist) consonants  (void1 ++ (head wordlist):[]) 0 0
                    else gamma0 wordlist consonants void1 (void2+1) void3

gamma :: [String] -> [String] -> Double
gamma wordlist consonants = (((fromIntegral (gamma0 wordlist consonants [] 0 0)) / (fromIntegral $ length wordlist)) * (beta wordlist consonants))

delta0 :: [String] -> [String] -> String
delta0 wordlist void
        | length wordlist == 0 = head void
        | length void == 0 = delta0 (tail wordlist) ((head wordlist):void)
        | otherwise = delta0 (tail wordlist) ((head wordlist ++ head void):init void)

delta1 :: String -> Int -> Int
delta1 word void
    | length word == 0 = void
    | head word == 'C' = delta1 (tail word) void+1
    | otherwise = delta1 (tail word) void

delta :: [String] -> Double
delta syllablelist = (fromIntegral (delta1 (delta0 syllablelist []) 0)) / (fromIntegral (length (delta0 syllablelist [])))

epsilon0 :: String -> Int -> [Int] -> Int
epsilon0 word void1 void2
    | length word == 0 = head (mode void2)
    | head word == 'C' = epsilon0 (tail word) (void1+1) void2
    | otherwise = epsilon0 (tail word) 0 (void1:void2)

epsilon :: [String] -> Int
epsilon word = epsilon0 (delta0 word []) 0 []

--自然言語処理
natural :: String -> [String] -> [String] -> String -> String
natural sentence vowels consonants void
    | length sentence == 0 = void
    | elem ((head sentence):[]) vowels == True = natural (tail sentence) vowels consonants (void ++ "V")
    | elem ((head sentence):[]) consonants == True = natural (tail sentence) vowels consonants (void ++ "C")
    | otherwise = natural (tail sentence) vowels consonants (void ++ " ")

naturallist :: String -> [String] -> [String] -> String -> [String] -> [String]
naturallist sentence vowels consonants void1 void2 
    | length sentence == 0 = (void2 ++ (void1:[]))
    | elem ((head sentence):[]) vowels == True = naturallist (tail sentence) vowels consonants (void1 ++ ((head sentence):[])) void2
    | elem ((head sentence):[]) consonants == True = naturallist (tail sentence) vowels consonants (void1 ++ ((head sentence):[])) void2
    | otherwise = naturallist (tail sentence) vowels consonants "" (void2 ++ (void1:[]))

{-
prenatural :: [String] -> [String] -> [String] -> String -> [String] -> [String]
prenatural sentence vowels consonants void1 void2 
    | length (head sentence) == 0 = (void2 ++ (void1:[]))
    | elem ((head (head sentence)):[]) vowels == True = prenatural ((tail (head sentence)):(tail sentence)) vowels consonants (void1 ++ (head vowels)) void2
    | elem ((head (head sentence)):[]) consonants == True = prenatural ((tail (head sentence)):(tail sentence)) vowels consonants (void1 ++ (head consonants)) void2
    | otherwise = prenatural ((tail (head sentence)):(tail sentence)) vowels consonants "" (void2 ++ (void1:[]))
-}

prenatural :: String -> [String] -> [String]
prenatural prewords void
    | length prewords == 0 = void
    | length void == 0 = case head prewords of
                            'C' -> prenatural (tail prewords) ("C":void)
                            'V' -> prenatural (tail prewords) ("C":void)
    | head prewords == 'C' = prenatural (tail prewords) (void ++ (((head void) ++ "C"):[]))
    | head prewords == 'V' = prenatural (tail prewords) (void ++ (((head void) ++ "V"):[]))
    | otherwise = prenatural (tail prewords) void

euphonyindexgenerator :: String -> Int -> IO()
euphonyindexgenerator x y =
    if y /= 0
        then do
            let vowel = ["a", "i", "u", "e", "o"]
            vows <- randIO 1 (length vowel)
            vowels <- vowels vows vowel []
            let consonant = ["b", "c", "d", "f", "g", "h", "j", "k", "l", "m", "n", "p", "q", "r", "s", "t", "v", "w", "x", "y", "z"]
            cons <- randIO 2 (length consonant)
            consonants <- consonants cons consonant []
            kind <- randIO 1 50
            max <- randIO 1 3
            sylsets <- sylsets kind max []
            prewordgen <- prewordgen 3 sylsets []
            syllablerange <- randIO 1 6
            prewordsets <- prewordsets 30 syllablerange  sylsets []
            wordgen <- wordgen prewordgen vowels consonants []
            wordsets <- wordsets prewordsets vowels consonants []
            let sentence = sentgen wordsets []
            let alph = alpha wordsets
            let bet = 100 * (beta wordsets consonants)
            let gam = 100 * (gamma wordsets consonants)
            let del = 100 * (delta prewordsets)
            let ep = epsilon prewordsets
            let euphonyindex = euphony alph bet gam del ep
            euphonyindexgenerator (x ++ (sentence ++ "\t" ++ (show euphonyindex) ++ "\t" ++ (show alph) ++ "\t" ++ (show bet) ++ "\t" ++ (show gam) ++ "\t" ++ (show del) ++ "\t" ++ (show ep) ++ "\n")) (y-1)
    else writeFile "EuphonyIndex-Generator/Projects/output3.txt" x

main :: IO()
main = do
    putStrLn "入力："
    input <- getLine
    let vowel = ["a", "i", "u", "e", "o"]
    let consonant = ["b", "c", "d", "f", "g", "h", "j", "k", "l", "m", "n", "p", "q", "r", "s", "t", "v", "w", "x", "y", "z"]
    let nat = natural input vowel consonant ""
    let natwordset = naturallist input vowel consonant "" []
    let pre = L.words nat 
    print nat
    print natwordset
    print pre
    putStrLn "母音表"
    print vowel
    putStrLn "子音表"
    print $ consonant
    putStrLn "ユーフォニー指数(Euphony Index;)"
    putStr "alpha:"
    let alph = alpha natwordset
    print $ alph
    putStr "beta:"
    let bet = 100 * (beta natwordset consonant)
    print $ bet
    putStr "gamma:"
    let gam = 100 * (gamma natwordset consonant)
    print $ gam
    putStr "delta:"
    let del = 100 * (delta pre)
    print $ del
    putStr "epsilon:"
    let ep = epsilon pre
    print $ ep
    putStr "E = "
    let euphonyindex = euphony alph bet gam del ep
    print $ euphonyindex 


    {-
    putStrLn "母音一覧"
    let vowel = ["a", "i", "u", "e", "o"]
    vows <- randIO 1 (length vowel)
    vowels <- vowels vows vowel []
    print $ vowels
    putStrLn "子音一覧"
    let consonant = ["b", "c", "d", "f", "g", "h", "j", "k", "l", "m", "n", "p", "q", "r", "s", "t", "v", "w", "x", "y", "z"]
    cons <- randIO 2 (length consonant)
    consonants <- consonants cons consonant []
    print $ consonants
    putStrLn "音節構造一覧"
    kind <- randIO 1 50
    max <- randIO 1 3
    sylsets <- sylsets kind max []
    print $ sylsets
    putStrLn "単語の雛形(語の音節構造)"
    prewordgen <- prewordgen 3 sylsets []
    print $ prewordgen
    putStrLn "単語の雛形(語の音節構造)一覧"
    syllablerange <- randIO 1 6
    prewordsets <- prewordsets 30 syllablerange  sylsets []
    print $ prewordsets
    putStrLn "単語"
    wordgen <- wordgen prewordgen vowels consonants []
    print $ wordgen
    putStrLn "単語一覧"
    wordsets <- wordsets prewordsets vowels consonants []
    print $ wordsets
    putStrLn "文の生成"
    let sentence = sentgen wordsets []
    print $ sentence
    putStrLn "ユーフォニー指数(Euphony Index;)"
    putStr "alpha:"
    let alph = alpha wordsets
    print $ alph
    putStr "beta:"
    let bet = 100 * (beta wordsets consonants)
    print $ bet
    putStr "gamma:"
    let gam = 100 * (gamma wordsets consonants)
    print $ gam
    putStr "delta:"
    let del = 100 * (delta prewordsets)
    print $ del
    putStr "epsilon:"
    let ep = epsilon prewordsets
    print $ ep
    putStr "E = "
    let euphonyindex = euphony alph bet gam del ep
    print $ euphonyindex 
    -}
    --euphonyindexgenerator "sentence \t E \t alpha \t beta \t gamma \t delta \t epsilon \n" 1000