module MSP where
  
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

factdef = head $ supercombinators fact1
factreal = deval factdef  (const  666) (const (const 666))

fact :: Program
fact = let 𝓍 = Var "x" in Program
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

type VariableTable = String -> Int
type FunctionTable = String -> Int -> Int

-- Function patching: f[x ≔ v] behaves like f but now goes to v at position x.
patch :: Eq a => (a -> b) -> a -> b -> (a -> b)
patch f x v = \y -> if x == y then v else f y

eval :: Exp -> VariableTable -> FunctionTable -> Int
eval (Int i) env fenv = i
eval (Var v) env fenv = env v
eval (App name  exp) env fenv = fenv name $ eval exp env fenv
eval (Add e1 e2) env fenv   =  eval e1 env fenv  +     eval e2 env fenv
eval (Sub e1 e2) env fenv   =  eval e1 env fenv  -     eval e2 env fenv
eval (Mul e1 e2) env fenv   =  eval e1 env fenv  *     eval e2 env fenv
eval (Div e1 e2) env fenv   =  eval e1 env fenv  `div` eval e2 env fenv
eval (Ifz c e1 e2) env fenv = if   eval c  env fenv == 0
                               then eval e1 env fenv
                               else eval e2 env fenv

-- Lift a declaration into a function
deval :: Declaration -> VariableTable -> FunctionTable -> (Int -> Int)
deval (Declaration name var body) env fenv = this
  where this = \x -> eval body (patch env var x) (patch fenv name this)

-- Interpret a declaration as a function and add it to our function table,
-- if there are no more declarations then try to evaluate the main expression.
peval :: Program -> VariableTable -> FunctionTable -> Int
peval (Program []     exp) env fenv = eval exp env fenv
peval (Program (d:ds) exp) env fenv = peval (Program ds exp) env fenv'
  where fenv' = patch fenv (name d) (deval d env fenv)

run :: Program -> Int
run p = peval p emptyEnv emptyEnv
  where emptyEnv = \x -> error ("Yikes! The name " ++ x ++ " is not defined")

fact_direct :: Int
fact_direct = let f n = if n == 0 then 1 else n * f (n - 1) in f 10

{-
~20,000 byte difference; invoke the functions in ghci a few times.

ghci -XTemplateHaskell MSP.hs

*MSP> :set +s

*MSP> run fact
3628800
(0.01 secs, 93,704 bytes)

*MSP> fact_direct 
3628800
(0.01 secs, 77,832 bytes)

By staging, we can bring the size difference by a whole magnitude, down to ~2000; see MSP2.hs
-}
