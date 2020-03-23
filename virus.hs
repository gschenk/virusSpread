import System.Environment
import System.Exit
import Text.Printf
import Data.Char

-- this programme calculates a few simple virus spread models.


-- simple exponential growth, each step n is one incubation period
-- simple :: Double -> Int -> Int
-- simple :: (RealFrac a, Floating a, Integral b) => a -> b -> b -> b
simple :: Double -> Int -> Int -> Int
simple r i n = round (r**a)
   where a = fromIntegral n + (log (fromIntegral i)/log r)


-- simple accumulated
simpleAcc :: Double -> Int -> Int -> Int
simpleAcc r i = (foldl (+) 0) .map(simple r i).l
   where l n = [0..n]


-- population limited accumulated growth
-- where rIni is initial reproduction rate
-- and nTot is total population
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
accFromList rIni nTot is =  f n
   where i = tailDiff is -- number infectious cases
         r = (*rIni).(+1).(/ (fromIntegral nTot)).negate.fromIntegral.last $ is -- effective replication number
         n = (+ last is) . round . (* r) . fromIntegral $ i -- newly infected
         f n
             | n > nTot = nTot
             | otherwise = n

-- accumulated cases new, and old, with pop limit
limitedAcc :: (RealFrac a, Integral b) => a -> b -> b -> Int -> [b]
limitedAcc rIni nTot iIni n = last . take n . iterate f $ [iIni]
    where f is = is ++ [accFromList rIni nTot is]

-- new cases with population limit
--limited :: Integral b => Double -> b -> Int -> Int -> [b]
limited :: (RealFrac a, Integral b) => a -> b -> b -> Int -> [b]
limited rIni nTot iIni n = map f [1..n]
    where f = tailDiff . limitedAcc rIni nTot iIni


-- transpose list of lists
transpose :: [[a]] -> [[a]]
transpose ([]:_) = []
transpose xs = (map head xs) : transpose (map tail xs)


-- calculate results
results :: (Double, Int, Int, Int, Double) -> [[Int]]
results (r,n,s,i,_) = ss : sim : simA : lim : limA : []
    where ss = [0..s]
          sim = map (simple r i) ss
          simA = map (simpleAcc r i) ss
          lim = limited r n i (s+1)
          limA = limitedAcc r n i (s+1)


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
    \i0 initial infected\n\ \p0 ratio of immune population"

showParms :: [String] -> String
showParms (r:n:s:[]) = showParms (r:n:s:"1":"0":[])
showParms (r:n:s:i:[]) = showParms (r:n:s:i:"0":[])
showParms (r:n:s:i:p:[]) = "#R0=" ++ r ++ " N=" ++ n ++ " s=" ++ s ++ " i0=" ++ i ++ " p0=" ++ p


-- Input

-- comprehend numbers with kilo, Mega, Giga suffix
kiloToInt :: String -> Int
kiloToInt cs
    | elem (last cs) ['0'..'9'] = read cs
    | otherwise = pref cs * suf cs
    where pref = read . init
          suf = floor . f . last
          f 'k' = 1e3
          f 'M' = 1e6
          f 'G' = 1e9
          f  _ = error "Not a number."

-- input comprehension
parse :: [String] -> (Double,Int,Int,Int,Double) 
parse (a:b:c:[]) = parse [a,b,c,"1","0"]
parse (a:b:c:e:[]) = parse [a,b,c,e,"0"]
parse (a:b:c:d:e:_) 
    | p < 1 && p >= 0 = (r0, n0, s, i0, p)
    | otherwise = error "Ratio of immune population not in range 0 <= P < 1"
    where r0 = read a :: Double
          n0 = kiloToInt b
          s = read c :: Int
          i0 = kiloToInt d
          p = read e :: Double
parse _ = error "Wrong number of arguments"


main = do
    args <- getArgs
    --let parms = parse args
    if args == [] || elem (head args) ["-h", "-?", "--help"]
      then putStrLn helpText >> exitWith(ExitFailure 1)
      else putStrLn $ showParms args

    let parms = parse args
    putStrLn.pretty.transpose $ results parms
