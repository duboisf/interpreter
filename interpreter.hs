import Data.Map (Map)
import qualified Data.Map as M

data Expression a = Plus   (Expression a) (Expression a)
                  | Minus  (Expression a) (Expression a)
                  | Times  (Expression a) (Expression a)
                  | Divide (Expression a) (Expression a)
                  | Variable String
                  | Number a

class Interpreter i where
    interpret :: Fractional a => i a -> Map String a -> a

instance Interpreter Expression where
    interpret (Plus a b) m = interpret a m + interpret b m
    interpret (Minus a b) m = interpret a m - interpret b m
    interpret (Times a b) m = interpret a m * interpret b m
    interpret (Divide a b) m = interpret a m / interpret b m
    interpret (Variable name) m = case M.lookup name m of
                                       Nothing -> error $ "Variable " ++ name ++ " not found"
                                       Just x -> x
    interpret (Number a) _ = a

test :: Fractional a => Expression a
test = Plus
        (Times
            (Variable "x")
            (Variable "y"))
        (Minus
            (Number 1)
            (Number 3.5))

variables :: Map String Double
variables = M.fromList [("x", 5), ("y", 6)]

main :: IO ()
main = print $ interpret test variables

