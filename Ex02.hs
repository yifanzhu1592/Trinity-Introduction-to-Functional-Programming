{- butrfeld Andrew Butterfield -}
module Ex02 where

name, idno, username :: String
name      =  "Yifan Zhu"  -- replace with your name
idno      =  "18300717"    -- replace with your student id
username  =  "zhuyi"   -- replace with your TCD username


declaration -- do not modify this
 = unlines
     [ ""
     , "@@@ This exercise is all my own work."
     , "@@@ Signed: " ++ name
     , "@@@ "++idno++" "++username
     ]

-- Datatypes and key functions -----------------------------------------------

-- do not change anything in this section !

type Id = String

data Expr
  = Val Double
  | Add Expr Expr
  | Mul Expr Expr
  | Sub Expr Expr
  | Dvd Expr Expr
  | Var Id
  | Def Id Expr Expr
  deriving (Eq, Show)

type Dict k d  =  [(k,d)]

define :: Dict k d -> k -> d -> Dict k d
define d s v = (s,v):d

find :: Dict String d -> String -> Either String d
find []             name              =  Left ("undefined var "++name)
find ( (s,v) : ds ) name | name == s  =  Right v
                         | otherwise  =  find ds name

type EDict = Dict String Double

v42 = Val 42 ; j42 = Just v42

-- do not change anything above !

-- Part 1 : Evaluating Expressions -- (50 test marks, worth 25 Exercise Marks) -

-- Implement the following function so all 'eval' tests pass.

-- eval should return `Left msg` if:
  -- (1) a divide by zero operation was going to be performed;
  -- (2) the expression contains a variable not in the dictionary.
  -- see test outcomes for the precise format of those messages

eval :: EDict -> Expr -> Either String Double

eval _ (Dvd e (Val 0)) = Left ("div by zero")
eval _ (Val e) = Right e
eval d (Var e) = find d e

eval d (Add e f)
  = case (eval d e, eval d f) of
    (Right x, Right y) -> Right (x+y)
    (Left ("div by zero"), Right y) -> Left("div by zero")
    (Right x, Left ("div by zero")) -> Left("div by zero")
    (Right x, Left ("undefined var x")) -> Left("undefined var x")
    (Left ("undefined var x"), Right y) -> Left("undefined var x")
    (Right x, Left ("undefined var y")) -> Left("undefined var y")
    (Left ("undefined var y"), Right y) -> Left("undefined var y")
    _ -> Left("div by zero")

eval d (Sub e f)
  = case (eval d e, eval d f) of
    (Right x, Right y) -> Right (x-y)
    (Left ("div by zero"), Right y) -> Left("div by zero")
    (Right x, Left ("div by zero")) -> Left("div by zero")
    (Right x, Left ("undefined var x")) -> Left("undefined var x")
    (Left ("undefined var x"), Right y) -> Left("undefined var x")
    (Right x, Left ("undefined var y")) -> Left("undefined var y")
    (Left ("undefined var y"), Right y) -> Left("undefined var y")
    _ -> Left("div by zero")

eval d (Mul e f)
  = case (eval d e, eval d f) of
    (Right x, Right y) -> Right (x*y)
    (Left ("div by zero"), Right y) -> Left("div by zero")
    (Right x, Left ("div by zero")) -> Left("div by zero")
    (Right x, Left ("undefined var x")) -> Left("undefined var x")
    (Left ("undefined var x"), Right y) -> Left("undefined var x")
    (Right x, Left ("undefined var y")) -> Left("undefined var y")
    (Left ("undefined var y"), Right y) -> Left("undefined var y")
    _ -> Left("div by zero")

eval d (Dvd e f)
  = case (eval d e, eval d f) of
    (Right x, Right y) -> Right (x/y)
    (Left ("div by zero"), Right y) -> Left("div by zero")
    (Right x, Left ("div by zero")) -> Left("div by zero")
    (Right x, Left ("undefined var x")) -> Left("undefined var x")
    (Left ("undefined var x"), Right y) -> Left("undefined var x")
    (Right x, Left ("undefined var y")) -> Left("undefined var y")
    (Left ("undefined var y"), Right y) -> Left("undefined var y")
    _ -> Left("div by zero")

eval d (Def n e f)
  = let x = eval d e
  in case (x) of
    Right x -> eval (define d n x) f
    _ -> Left("div by zero")

-- Part 1 : Expression Laws -- (15 test marks, worth 15 Exercise Marks) --------

{-

There are many, many laws of algebra that apply to our expressions, e.g.,

  x + y         =   y + z            Law 1
  (x + y) + z   =   x + (y + z)      Law 2
  (x - y) - z   =   x - (y + z)      Law 3
  x*x - y*y     =   (x + y)*(x - y)  Law 4
  ...

  We can implement these directly in Haskell using Expr

  Function LawN takes an expression:
    If it matches the "shape" of the law lefthand-side,
    it replaces it with the corresponding righthand "shape".
    If it does not match, it returns Nothing

    Implement Laws 1 through 4 above
-}


law1 :: Expr -> Maybe Expr
law1 e = case e of
         (Add x y) -> Just (Add y x)
         _ -> Nothing

law2 :: Expr -> Maybe Expr
law2 e = case e of
         (Add (Add x y) z) -> Just (Add x (Add y z))
         _ -> Nothing

law3 :: Expr -> Maybe Expr
law3 e = case e of
         (Sub (Sub x y) z) -> Just (Sub x (Add y z))
         _ -> Nothing

law4 :: Expr -> Maybe Expr
law4 e = case e of
         (Sub (Mul a c) (Mul b d)) -> law4check (a==c) (b==d) (Mul (Add a b) (Sub c d))
         _ -> Nothing
      
law4check :: Bool -> Bool -> Expr -> Maybe Expr
law4check x y z = case (x, y) of
                  (True, True) -> Just z
                  (_, _) -> Nothing
