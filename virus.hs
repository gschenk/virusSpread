import System.Environment
import System.Exit
import Text.Printf

-- this programme calculates a few simple virus spread models.


-- simple exponential growth, each step n is one incubation period
-- simple :: Double -> Int -> Int
-- simple :: (RealFrac a, Floating a, Integral b) => a -> b -> b -> b
simple :: Double -> Int -> Int -> Int
simple r i n = round (r**a)
   where a = fromIntegral n + (log (fromIntegral i)/log r)


-- simple accumulated
--simpleAcc :: Double -> Int -> Int
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
-- accFromList :: (Integral b) => Double -> b -> [b] -> b
accFromList :: Double -> Int -> [Int] -> Int
--accFromList :: (RealFrac a, Integral b) => a -> b -> [b] -> b
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
-- limitedAcc :: Integral b => Double -> b -> Int -> [b]
limitedAcc rIni nTot iIni n = last . take n . iterate f $ [iIni]
    where f is = is ++ [accFromList rIni nTot is]

-- new cases with population limit
--limited :: Integral b => Double -> b -> Int -> Int -> [b]
limited rIni nTot iIni n = map f [1..n]
    where f = tailDiff . limitedAcc rIni nTot iIni


-- transpose list of lists
transpose ([]:_) = []
transpose xs = (map head xs) : transpose (map tail xs)


-- calculate results
results r n s i = ss : sim : simA : lim : limA : []
    where ss = [0..s]
          sim = map (simple r i) ss
          simA = map (simpleAcc r i) ss
          lim = limited r n i (s+1)
          limA = limitedAcc r n i (s+1)

-- format pretty output
pretty :: [[Int]] -> String
pretty = unlines . (header head1:) . (header head2:) . map f
    where f = unwords . map (printf "%10d")

header = ((++)"#").unwords .  map (printf "%10s") 

head1 = ["","Simple Model","","Population Limited Model"]
head2 = ["Step","Infected", "Accumm.", "Infected", "Acc'd" ]


parse (r:n:s:i:_) = [r,n,s,i]
parse _ = []

exit = exitWith(ExitSuccess)

main = do
    args <- getArgs
    let pars = parse args
    if pars == []
      then putStrLn "usage: virus <R> <N> <s>\nR replication number\nN population size\ns number of steps\ni initial infected" >> exitWith(ExitFailure 1)
      else putStrLn $ "# R=" ++ pars !! 0 ++ ", N=" ++ pars !! 1 ++ ", s=" ++ pars !! 2
    let rIni = read . head $ pars :: Double
    let (nTot:steps:iIni:[]) = map read . tail $ pars :: [Int]
--    let results = simple rIni steps
    let test = limited rIni nTot steps
    putStrLn.pretty.transpose $ results rIni nTot steps iIni
