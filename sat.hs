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
        sols    = [x | x <- permuts, isSolution x cprog vars]
        sol     = head sols
        cprog   = convertProg prog
        permuts = permut vars

permut :: Int -> [[Int]]
permut 1 = [[1], [-1]]
permut n = f ++ r
  where
    prev = permut $ n-1
    r = [[n] ++ p | p <- prev]
    f = [[-n] ++ p | p <- prev]

isSolution :: [Int] -> [[Int]] -> Int -> Bool
isSolution sol (s:ss) vars
    | end && cSatis   = True
    | cSatis          = isSolution sol ss vars
    | otherwise       = False
    where 
        end     = null ss
        cSatis  = clauseSatisfied s sol vars

clauseSatisfied :: [Int] -> [Int] -> Int -> Bool
clauseSatisfied [] sol vars      = False
clauseSatisfied (c:cs) sol vars  = c == sol !! i || clauseSatisfied cs sol vars
    where 
        i = vars - abs c
