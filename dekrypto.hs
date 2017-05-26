import Data.List
import Permutation
import System.IO
import System.Exit
import qualified Data.Char as Char


data Puzzle = Puzzle { cards :: [Double]
                     , goal :: Double
                     } deriving (Show)

data ExprTree = ExprLeaf Double | ExprTree { branch1 :: ExprTree
                                           , branch2 :: ExprTree
                                           , operator :: Operator
                                           }

instance Eq ExprTree where
    (ExprLeaf _) == (ExprTree _ _ _) = False
    (ExprTree _ _ _) == (ExprLeaf _) = False
    (ExprLeaf x) == (ExprLeaf y) = x == y
    (ExprTree t1 t2 op1) == (ExprTree t3 t4 op2)
        | (t1,t2,op1) == (t3,t4,op2) = True
        | (op1 == op2) && (commutative op1) && (t1 == t4) && (t2 == t3) = True
        | otherwise = False

instance Show ExprTree where
    show (ExprLeaf x) = show $ floor x
    show (ExprTree t1 t2 op) = "(" ++ (show t1) ++ (show op) ++ (show t2) ++ ")" 

evaluate :: ExprTree -> Double
evaluate (ExprLeaf x) = x
evaluate (ExprTree t1 t2 op) = use op (evaluate t1) (evaluate t2)

data Operator = Add | Subtract | Multiply | Divide deriving (Eq, Enum)

use :: Fractional a => Operator -> a -> a -> a
use Add = (+)
use Subtract = (-)
use Multiply = (*)
use Divide = (/)

commutative :: Operator -> Bool
commutative Add = True
commutative Multiply = True
commutative _ = False

instance Show Operator where
    show Add = " + "
    show Subtract = " - "
    show Multiply = " * "
    show Divide = " / "


expressions :: [Double] -> [ExprTree]
expressions [] = []
expressions [x] = [ExprLeaf x]
expressions xs@[_,_] = [ExprTree (ExprLeaf x) (ExprLeaf y) f | [x,y] <- rotate xs, f <- [Add .. Divide]]
expressions xs = let halves = [splitAt n ys | n <- [1..length xs - 1], ys <- permute xs]
                 in  nub [ExprTree t1 t2 op | (ys,zs) <- halves, t1 <- expressions ys, t2 <- expressions zs, op <- [Add .. Divide]]

solve :: Puzzle -> [String]
solve (Puzzle cards goal) = [init . tail $ show expr | expr <- expressions cards, evaluate expr == goal]

prompt :: String -> IO String
prompt msg = do putStr msg
                hFlush stdout
                getLine

main = do cardsString <- prompt "Cards (separated by spaces): "
          let cardStrings = words cardsString
          if not $ all (all Char.isDigit) cardStrings then die "Error: Cards must be integers separated by spaces." else return '\NUL'
          goalString <- prompt "Goal: "
          if not $ all Char.isDigit goalString then die "Error: Goal must be an integer." else return '\NUL'
          let cards = map read cardStrings :: [Double]
          let goal = read goalString :: Double
          let puzzle = Puzzle cards goal
          let solutions = solve puzzle
          putStr $ if not $ null solutions then head solutions else "No solutions."