module MSP2 where

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
  , mainMethod      = App "fact" (Int 10)
  }

-- A simple interpreter

type VariableTable = String -> Code Int
type FunctionTable = String -> Code (Int -> Int)

-- Function patching: f[x ≔ v] behaves like f but now goes to v at position x.
patch :: Eq a => (a -> b) -> a -> b -> (a -> b)
patch f x v = \y -> if x == y then v else f y

eval :: Exp -> VariableTable -> FunctionTable -> Code Int
eval (Int i) env fenv = [|| i ||]
eval (Var v) env fenv = env v
eval (App name  exp) env fenv = [|| $$(fenv name) $$(eval exp env fenv) ||]
eval (Add e1 e2) env fenv     = [|| $$(eval e1 env fenv)  + $$(eval e2 env fenv) ||]
eval (Sub e1 e2) env fenv   = [|| $$(eval e1 env fenv)  - $$(eval e2 env fenv) ||]
eval (Mul e1 e2) env fenv   = [|| $$(eval e1 env fenv)  * $$(eval e2 env fenv) ||]
eval (Div e1 e2) env fenv   = [|| $$(eval e1 env fenv)  `div` $$(eval e2 env fenv) ||]
eval (Ifz c e1 e2) env fenv = [|| if   $$(eval c  env fenv) == 0
                                  then $$(eval e1 env fenv)
                                  else $$(eval e2 env fenv)
                               ||]

-- Lift a declaration into a function
deval :: Declaration -> VariableTable -> FunctionTable -> Code (Int -> Int)
deval (Declaration name var body) env fenv =
  [|| let this = \x -> $$(eval body (patch env var [|| x ||] ) (patch fenv name [|| this ||]))
      in this
  ||]

{- Also possible:

deval (Declaration name var body) env fenv = this
  where this = [|| \x -> $$(eval body (patch env var [|| x ||] ) (patch fenv name this)) ||]
               -- no variable capture ;-)

-}
               
-- Interpret a declaration as a function and add it to our function table,
-- if there are no more declarations then try to evaluate the main expression.
peval :: Program -> VariableTable -> FunctionTable -> Code Int
peval (Program []     exp) env fenv = eval exp env fenv
peval (Program (d:ds) exp) env fenv = peval (Program ds exp) env fenv'
  where fenv' = patch fenv (name d) (deval d env fenv)

-- Usage: $$(run p)
run :: Program -> Code Int
run p = peval p emptyEnv emptyEnv
  where emptyEnv = \x -> error ("Yikes! The name " ++ x ++ " is not defined")

fact_direct :: Int
fact_direct = let f n = if n == 0 then 1 else n * f (n - 1) in f 10

{-
~2000 byte difference; invoke the functions in ghci a few times.

ghci -XTemplateHaskell MSP2.hs

*MSP2> :set +s

*MSP2> $$(run fact)
3628800
(0.02 secs, 79,864 bytes)

*MSP2> fact_direct 
3628800
(0.01 secs, 77,576 bytes)

-}
