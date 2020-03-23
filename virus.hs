import System.Environment
import System.Exit
import Text.Printf
import Data.Char

-- This programme calculates a few simple virus spread models.
-- It is meant as a toy problem to illustrate exponential growth
-- and limits to growth in a population that become immune.

-- MIT License
-- Copyright (c) 2020 gerald schenk

-- simple exponential growth, each step n is one incubation period
-- simple :: Double -> Int -> Int
-- simple :: (RealFrac a, Floating a, Integral b) => a -> b -> b -> b
simple :: Double -> Int -> Int -> Int
simple r i n = round (r**a)
   where a = fromIntegral n + (log (fromIntegral i)/log r)


-- simple accumulated
simpleAcc :: Double -> Int -> Int -> Int -> Int
simpleAcc r i p = (+p) . (foldl (+) 0) . map(simple r i) . l
   where l n = [0..n]


-- population limited accumulated growth
-- where r0 is initial reproduction rate
-- and n0 is total population
-- assuming no re-infections

-- get difference of last two list elements
tailDiff :: Num a => [a] -> a
tailDiff [] = 0
tailDiff (n:[]) = n
tailDiff [m,n] = n-m
tailDiff (_:ns) = tailDiff ns

-- accumulated new infected from list of infected
accFromList :: (RealFrac a, Integral b) => a -> b -> [b] -> b
accFromList _ _ [] = 0
accFromList _ 0 is = 0
accFromList 0 _ is = last is
accFromList r0 n0 is =  f n
   where i = tailDiff is -- number infectious cases
         r = (*r0).(+1).(/ (fromIntegral n0)).negate.fromIntegral.last $ is -- effective replication number
         n = (+ last is) . round . (* r) . fromIntegral $ i -- newly infected
         f n
             | n > n0 = n0
             | otherwise = n

-- accumulated cases new, and old, with pop limit
limitedAcc :: (RealFrac a, Integral b) => a -> b -> b -> b-> Int -> [b]
limitedAcc r0 n0 i0 p0 s = last . take s . iterate f $ [p0, p0+i0]
    where f is = is ++ [accFromList r0 n0 is]

-- new cases with population limit
limited :: (RealFrac a, Integral b) => a -> b -> b -> b -> Int -> [b]
limited r0 n0 i0 p0 s = map f [1..s]
    where f = tailDiff . limitedAcc r0 n0 i0 p0


-- transpose list of lists
transpose :: [[a]] -> [[a]]
transpose ([]:_) = []
transpose xs = (map head xs) : transpose (map tail xs)


-- calculate results
results :: (Double, Int, Int, Int, Int) -> [[Int]]
results (r0, n, s, i, p) = ss : sim : simA : lim : limA : []
    where ss = [0..s]
          sim = map (simple r0 i) ss
          simA = map (simpleAcc r0 i p) ss
          lim = limited r0 n i p (s+1)
          limA = tail . limitedAcc r0 n i p $ (s+1)


-- Output
-- format pretty output
pretty :: [[Int]] -> String
pretty = unlines . (header head1:) . (header head2:) . map f
    where f = unwords . map (printf "%10d")

header :: [String] -> String
header = ((++)"#").unwords .  map (printf "%10s") 

head1 = ["","Simple Model","","Population Limited Model"]
head2 = ["Step","Infected", "Accumm.", "Infected", "Acc'd" ]

helpText = "usage: virus <R0> <N> <s> [<i> [<p>]] \n\
    \R0 base replication number\n\
    \N population size\n\ \s number of steps\n\
    \i0 initial infected (ratio or absolute)\n\ \p0 initial immune population (ratio or absolute)"

showParms :: [String] -> String
showParms (r:n:s:[]) = showParms (r:n:s:"1":"0":[])
showParms (r:n:s:i:[]) = showParms (r:n:s:i:"0":[])
showParms (r:n:s:i:p:[]) = "#R0=" ++ r ++ " N=" ++ n ++ " s=" ++ s ++ " i0=" ++ i ++ " p0=" ++ p


-- Input

-- comprehend numbers with kilo, Mega, Giga suffix
kiloToInt :: String -> Int
kiloToInt = round . kiloToDouble

kiloToDouble :: String -> Double
kiloToDouble cs
    | elem (last cs) ['0'..'9'] = read cs
    | otherwise = pref cs * suf cs
    where pref = read . init
          suf = f . last
          f 'k' = 1e3
          f 'M' = 1e6
          f 'G' = 1e9
          f  _ = error "Not a number."

-- input comprehension
parse :: [String] -> (Double,Int,Int,Int,Int) 
parse (a:b:c:[]) = parse [a,b,c,"1","0"]
parse (a:b:c:e:[]) = parse [a,b,c,e,"0"]
parse (a:b:c:d:e:_) = (r0, n0, s, f i, f p)
    where r0 = read a :: Double
          n0 = kiloToInt b
          s = read c :: Int
          i = kiloToDouble d
          p = kiloToDouble e
          f x
            | x < 1 && x >= 0 = round . (* fromIntegral n0) $ x
            | floor x == ceiling x = floor x -- input was integer number of total population
            | otherwise = error "Ratio of infected/immune population not in range 0 <= P < 1"
parse _ = error "Wrong number of arguments"


main = do
    args <- getArgs
    --let parms = parse args
    if args == [] || elem (head args) ["-h", "-?", "--help"]
      then putStrLn helpText >> exitWith(ExitFailure 1)
      else putStrLn $ showParms args

    let parms = parse args
    putStrLn.pretty.transpose $ results parms
