import Permutation
import System.IO
import System.Exit
import System.Environment
import Data.Time
import qualified Data.Char as Char


data Puzzle = Puzzle { cards :: [Double]
                     , goal :: Double
                     } deriving (Show)

data ExprTree = ExprLeaf Double | ExprTree { branch1 :: ExprTree
                                           , branch2 :: ExprTree
                                           , operator :: Operator
                                           }

instance Show ExprTree where
    show (ExprLeaf x) = show $ floor x
    show (ExprTree l r op) = "(" ++ (show l) ++ (show op) ++ (show r) ++ ")" 

evaluate :: ExprTree -> Double
evaluate (ExprLeaf x) = x
evaluate (ExprTree l r op) = use op (evaluate l) (evaluate r)

isInt :: RealFrac a => a -> Bool
isInt x = fromInteger (round x) == x

-- Tournament rules don't allow fractions or negative numbers at any point
evaluateTourn :: ExprTree -> Double
evaluateTourn (ExprLeaf x) = x
evaluateTourn (ExprTree l r op) = let lVal = evaluateTourn l
                                      rVal = evaluateTourn r
                                  in  if (isInt lVal) && isInt (rVal) && (lVal >= 0) && (rVal >= 0) then use op lVal rVal else 0/0

data Operator = Add | Subtract | Multiply | Divide deriving (Eq, Enum)

use :: Fractional a => Operator -> a -> a -> a
use Add = (+)
use Subtract = (-)
use Multiply = (*)
use Divide = (/)

instance Show Operator where
    show Add = " + "
    show Subtract = " - "
    show Multiply = " * "
    show Divide = " / "


expressions :: [Double] -> [ExprTree]
expressions [] = []
expressions [x] = [ExprLeaf x]
expressions xs@[_,_] = [ExprTree (ExprLeaf x) (ExprLeaf y) op | [x,y] <- rotate xs, op <- [Add ..]]
expressions xs = let halves = [splitAt n ys | n <- [1..length xs - 1], ys <- permute xs]
                 in  [ExprTree l r op | (ys,zs) <- halves, l <- expressions ys, r <- expressions zs, op <- [Add ..]]

solve :: Puzzle -> [ExprTree]
solve (Puzzle cards goal) = [expr | expr <- expressions cards, evaluate expr == goal]

-- See note to `evaluateTourn`
solveTourn :: Puzzle -> [ExprTree]
solveTourn (Puzzle cards goal) = [expr | expr <- expressions cards, evaluateTourn expr == goal]

prompt :: String -> IO String
prompt msg = do putStr msg
                hFlush stdout
                getLine

displayHelp :: IO ()
displayHelp = putStrLn "\nusage: dekrypto [-H|-h]\n\
                       \\n\
                       \  -H : Use house rules (allow intermediate results to be negative and/or\n\
                       \       fractional)\n\
                       \  -h : Show this help message"

main = do args <- getArgs
          if "-h" `elem` args || "-?" `elem` args then displayHelp >> exitSuccess else return ()
          cardsString <- prompt "Cards (separated by spaces): "
          let cardStrings = words cardsString
          if not $ all (all Char.isDigit) cardStrings then die "Error: Cards must be integers separated by spaces." else return '\NUL'
          goalString <- prompt "Goal: "
          if not $ all Char.isDigit goalString then die "Error: Goal must be an integer." else return '\NUL'
          start <- getCurrentTime
          let cards = map read cardStrings :: [Double]
          let goal = read goalString :: Double
          let puzzle = Puzzle cards goal
          let solutions = (if "-H" `elem` args then solve else solveTourn) puzzle
          putStrLn $ if not $ null solutions then (init . tail . show . head) solutions ++ " = " ++ (show $ floor goal) else "No solutions."
          end <- getCurrentTime
          putStrLn $ "Solved in " ++ (init . show $ diffUTCTime end start) ++ " seconds"