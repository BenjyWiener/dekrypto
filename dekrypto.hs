import Permutation
import System.IO
import System.Exit
import System.Environment
import Data.Time
import qualified Data.Char as Char


data Puzzle = Puzzle { cards :: [Int]
                     , goal :: Int
                     } deriving (Show)

data Expression = Term Int | Expression { operator :: Operator
                                        , branch1 :: Expression
                                        , branch2 :: Expression
                                        }

instance Show Expression where
    show (Term x) = show x
    show (Expression op l r) = "(" ++ (show l) ++ (show op) ++ (show r) ++ ")" 

evaluate :: Expression -> Double
evaluate (Term x) = fromIntegral x
evaluate (Expression op l r) = use op (evaluate l) (evaluate r)

isInt :: Real a => a -> Bool
isInt x = toRational (round $ toRational x) == toRational x

-- Tournament rules don't allow fractions or negative numbers at any point
evaluateTourn :: Expression -> Double
evaluateTourn (Term x) = fromIntegral x
evaluateTourn (Expression op l r) = let lVal = evaluateTourn l
                                        rVal = evaluateTourn r
                                    in  if (isInt lVal) && isInt (rVal) && (lVal >= 0) && (rVal >= 0) then use op lVal rVal else (0/0 :: Double)

data Operator = Add | Subtract | Multiply | Divide deriving (Eq, Enum)

use :: Operator -> Double -> Double -> Double
use Add = (+)
use Subtract = (-)
use Multiply = (*)
use Divide = (/)

instance Show Operator where
    show Add = " + "
    show Subtract = " - "
    show Multiply = " * "
    show Divide = " / "


expressions :: [Int] -> [Expression]
expressions [] = []
expressions [x] = [Term x]
expressions xs@[_,_] = [Expression op (Term x) (Term y) | [x,y] <- rotate xs, op <- [Add ..]]
expressions xs = let halves = [splitAt n ys | n <- [1..length xs - 1], ys <- permute xs]
                 in  [Expression op l r | (ys,zs) <- halves, l <- expressions ys, r <- expressions zs, op <- [Add ..]]

solve :: Puzzle -> [Expression]
solve (Puzzle cards goal) = [expr | expr <- expressions cards, evaluate expr == fromIntegral goal]

-- See note to `evaluateTourn`
solveTourn :: Puzzle -> [Expression]
solveTourn (Puzzle cards goal) = [expr | expr <- expressions cards, evaluateTourn expr == fromIntegral goal]

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
          let cards = map read cardStrings :: [Int]
              goal = read goalString :: Int
              puzzle = Puzzle cards goal
              solutions = (if "-H" `elem` args then solve else solveTourn) puzzle
          putStrLn $ if not $ null solutions then (init . tail . show . head) solutions ++ " = " ++ (show goal) else "No solutions."
          end <- getCurrentTime
          putStrLn $ "Solved in " ++ (init . show $ diffUTCTime end start) ++ " seconds"