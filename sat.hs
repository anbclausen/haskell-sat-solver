import System.Environment
import Data.Time.Clock.POSIX

main :: IO()
main = do  
    args <- getArgs
    contents <- readFile (args !! 0)
    let l = lines contents
    before <- getPOSIXTime
    putStrLn $ clean l
    after <- getPOSIXTime
    putStrLn $ "CPU time: " ++ show (after - before)

clean :: [String] -> String
clean (h:t)
    | head h == 'c' = clean t
    | head h == 'p' =
        case () of
         () | form == "cnf"     -> solve t vars
            | otherwise         -> error "needs to be in cnf format."
            where
                info = words h
                form = info !! 1
                vars = read $ info !! 2
    | otherwise = error "something went wrong. Make sure you only have lines starting with p and c before the data."

pprint :: [Int] -> String
pprint list
    | list == []    = "0"
    | otherwise     = show h ++ " " ++ pprint t
    where
        h = head list
        t = tail list

convertProg :: [String] -> [[Int]]
convertProg []      = []
convertProg (s:ss)  = [h] ++ convertProg ss
    where 
        h = map read $ words $ init s

solve :: [String] -> Int -> String
solve prog vars
    | null sols = "UNSAT"
    | otherwise = "SAT\n" ++ pprint sol
    where 
        sols    = [x | x <- permut vars, isSolution x cprog]
        sol     = head sols
        cprog   = convertProg prog

permut :: Int -> [[Int]]
permut 1 = [[1], [-1]]
permut n = r ++ f
  where
    prev = permut $ n-1
    r = [p ++ [n] | p <- prev]
    f = [p ++ [-n] | p <- prev]

isSolution :: [Int] -> [[Int]] -> Bool
isSolution sol (s:ss) 
    | end && clauseSatisfied s sol   = True
    | clauseSatisfied s sol          = isSolution sol ss
    | otherwise                         = False
    where 
        end     = null ss

clauseSatisfied :: [Int] -> [Int] -> Bool
clauseSatisfied [] sol      = False
clauseSatisfied (c:cs) sol  = c == sol !! i || clauseSatisfied cs sol
    where 
        i = (abs c) -1
