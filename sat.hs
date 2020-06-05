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
    | list == []    = ""
    | otherwise     = h ++ ' ' : pprint t
    where
        h = show $ head list
        t = tail list

convertProg :: [String] -> [[Int]]
convertProg []      = []
convertProg (s:ss)  = [h] ++ convertProg ss
    where 
        h = map read $ words $ init s

solve :: [String] -> Int -> String
solve prog vars
    | null sols = "UNSAT"
    | otherwise = "SAT\n" ++ psol
    where 
        sols    = permut vars cprog
        sol     = head sols
        cprog   = convertProg prog
        psol    = pprint $ reverse sol


permut :: Int -> [[Int]] -> [[Int]]
permut 1 _ = [[1], [-1]]
permut n cprog = ps
  where
    prev    = permut (n-1) cprog
    ps      = [x : p | p <- prev, x <- [n, -n], isSafe (x : p) cprog n]

isSafe :: [Int] -> [[Int]] -> Int -> Bool
isSafe _ [] _        = True
isSafe vars (c:cs) n
    | length c > n || safeClause vars c n   = isSafe vars cs n
    | otherwise                             = False

safeClause :: [Int] -> [Int] -> Int -> Bool
safeClause _ [] _           = False
safeClause vars (l:cs) n
    | pl > n                = True
    | l == vars !! i        = True
    | otherwise             = safeClause vars cs n
    where
        pl = abs l
        i = n - pl