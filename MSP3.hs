module MSP3 where

import Language.Haskell.TH hiding (Exp) -- Metaprogramming support for Haskell
import Language.Haskell.TH.Syntax hiding (Exp)
import System.IO.Unsafe
import Data.Time

type Code a = Q (TExp a)

data Exp  = Int Int
          | Var String
          | App String Exp
          | Add Exp Exp
          | Sub Exp Exp
          | Mul Exp Exp
          | Div Exp Exp
          | Ifz Exp Exp Exp
  deriving (Show)

-- “Declaration f x b” denotes “f = (λ x → b)”; i.e., top level items
data Declaration = Declaration { name :: String , parameter :: String , body :: Exp}
  deriving (Show)                 

-- The main method is the expression to be evaluated, given auxiliary definitions.
data Program = Program {supercombinators :: [Declaration] , mainMethod :: Exp}
  deriving (Show)

fact  :: Program
fact  = let 𝓍 = Var "x" in Program
  {supercombinators = [ Declaration { name      = "fact"
                                    , parameter = "x"
                                    , body      = Ifz        𝓍
                                                  {- then -} (Int 1)  
                                                  {- else -} (𝓍 `Mul`(App "fact" (𝓍 `Sub` Int 1)))
                                    }
                      ]
  , mainMethod      = App "fact" (Int 20 `Div` Int 2)
  }

-- A simple interpreter

type VariableTable = String -> Code Int
type FunctionTable = String -> Code (Int -> Maybe Int)
-- A function simply may not be defined at some point; or may ``not terminate.''

-- Function patching: f[x ≔ v] behaves like f but now goes to v at position x.
patch :: Eq a => (a -> b) -> a -> b -> (a -> b)
patch f x v = \y -> if x == y then v else f y

-- An opportunity to use a monad transformer ;-)

eval :: Exp -> VariableTable -> FunctionTable -> Code (Maybe Int)
eval (Int i) env fenv = [|| Just i ||]
eval (Var v) env fenv = [|| Just $$(env v) ||]
eval (App name  exp) env fenv =
  [||
  let f   = $$(fenv name)         :: Int -> Maybe Int
      arg = $$(eval exp env fenv) :: Maybe Int
  in    
      arg >>= f
  ||]
eval (Add e1 e2) env fenv     =
  [||
   let l = $$(eval e1 env fenv) :: Maybe Int
       r = $$(eval e2 env fenv) :: Maybe Int
   in pure (+) <*> l <*> r
  ||]

-- For “Add”, we could have instead written something like that for “Sub”.

eval (Sub e1 e2) env fenv     =
  [||
  do l <- $$(eval e1 env fenv)
     r <- $$(eval e2 env fenv)
     return (l - r)
  ||]

eval (Mul e1 e2) env fenv     =
  [||
  do l <- $$(eval e1 env fenv)
     r <- $$(eval e2 env fenv)
     return (l * r)
  ||]

eval (Div e1 e2) env fenv     =
  [||
  do l <- $$(eval e1 env fenv)
     r <- $$(eval e2 env fenv)
     if r == 0 then Nothing else Just (l `div` r)
  ||]

eval (Ifz c e1 e2) env fenv =
  [||
   do cexp <- $$(eval c  env fenv)
      if cexp == 0
        then $$(eval e1 env fenv)
        else $$(eval e2 env fenv)
   ||]

-- Lift a declaration into a function
deval :: Declaration -> VariableTable -> FunctionTable -> Code (Int -> Maybe Int)  -- Only difference is the `Maybe`
deval (Declaration name var body) env fenv =
  [|| let this = \x -> $$(eval body (patch env var [|| x ||] ) (patch fenv name [|| this ||]))
      in this
  ||]

-- Interpret a declaration as a function and add it to our function table,
-- if there are no more declarations then try to evaluate the main expression.
peval :: Program -> VariableTable -> FunctionTable -> Code (Maybe Int)  -- Only difference is the `Maybe`
peval (Program []     exp) env fenv = eval exp env fenv
peval (Program (d:ds) exp) env fenv = peval (Program ds exp) env fenv'
  where fenv' = patch fenv (name d) (deval d env fenv)

-- Usage: $$(run p)
run :: Program -> Code (Maybe Int)  -- Only difference is the `Maybe`
run p = peval p emptyEnv emptyEnv
  where emptyEnv = \x -> error ("Yikes! The name " ++ x ++ " is not defined")

fact_direct :: Maybe Int
fact_direct = let f n = if n == 0 then 1 else n * f (n - 1)
              in do arg <- Just (10 `div` 2)
                    Just (f arg)

-- If we remove the Maybe decorations, the byte difference is still
-- roughly 10,000 bytes.

{-
~10,000 byte difference; invoke the functions in ghci a few times.

ghci -XTemplateHaskell MSP2.hs

*MSP3> :set +s

*MSP3> $$(run fact)
Just 3628800
(0.02 secs, 86,424 bytes)

*MSP3> fact_direct 
3628800
(0.01 secs, 77,600 bytes)

-}

