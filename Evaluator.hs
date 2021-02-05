module Evaluator where
import Parser
import Lexer (Position)

data Op = Equals
        | NotEquals
        | Plus
        | Times
        | Divide
        | Modulo 
        | Less
        | LessEq
        | Greater 
        | GreaterEq 
        | AndOp 
        | OrOp
        | NotOp
        | ImpliesOp 
        | UntilOp
        | AlwaysOp
        | EventuallyOp
        | NextOp
        | MkAccessor
        -- others?
        deriving (Show, Eq)


data FormulaExpr a = LocExpr Name Position (FormulaExpr a) 
                   | Accessor a 
                   | Op Op [FormulaExpr a] 
                   | Constant IValue
                   | FreezeVar Name Position
                   deriving (Show)

data Formula a = Atomic (FormulaExpr a)
               | Until (Formula a) (Formula a)
               | Not (Formula a)
               | And (Formula a) (Formula a) 
               | Or (Formula a) (Formula a) 
               | Implies (Formula a) (Formula a) 
               | Always (Formula a) 
               | Eventually (Formula a) 
               | Next (Formula a) 
               | Trivial | Absurd
               | LocFormula Name Position (Formula a)
               | FreezeIn Position Name (FormulaExpr a) (Formula a)
               deriving (Show)

type Accessor = String

type Env = [(Name, Value)]

data Value = Independent IValue 
           | StateDependent [Accessor] (FormulaExpr Accessor)
           | Formula [Accessor] (Formula Accessor)
           | Closure (Maybe (Name,Position)) Env [Name] Body
           | PartialOp Op [Value]
           deriving (Show)

data IValue = LitVal Lit
            | BoolVal Bool
            | Null
            deriving (Show, Eq)

data EvalError = ScopeError Position String
               | ModuloOnFloats Double Double
               | NotAFunction Value
               | BinaryOpOnInvalidTypes Op Value Value
               | UnaryOpOnInvalidTypes Op Value
               | FreezeLHSNotVar Position
               deriving (Show)


initialEnv :: Env
initialEnv = 
  [ ("true" , Independent $ BoolVal True )
  , ("false", Independent $ BoolVal False )
  , ("null", Independent $ Null)
  , ("_==_", PartialOp Equals [])
  , ("_!=_", PartialOp NotEquals [])
  , ("_+_", PartialOp Plus [])
  , ("_*_", PartialOp Times [])
  , ("_/_", PartialOp Divide [])
  , ("_%_", PartialOp Modulo  [])
  , ("_<_", PartialOp Less [])
  , ("_<=_", PartialOp LessEq [])
  , ("_>_", PartialOp Greater  [])
  , ("_>=_", PartialOp GreaterEq  [])
  , ("_&&_", PartialOp AndOp  [])
  , ("_||_", PartialOp OrOp [])
  , ("not_", PartialOp NotOp [])
  , ("_==>_", PartialOp ImpliesOp  [])
  , ("_until_", PartialOp UntilOp [])
  , ("always_", PartialOp AlwaysOp [])
  , ("eventually_", PartialOp EventuallyOp [])
  , ("next_", PartialOp NextOp [])
  , ("access", PartialOp MkAccessor [])
  ]

evaluate :: Env -> Body -> Either EvalError Value
evaluate g (Bind f ps b1 b2) = do 
   v1 <- evalBind g ps b1 
   let g' = (f,v1):g
   evaluate g' b2
evaluate g (Done e) = evalExpr g e

evalBind g [] b = evaluate g b
evalBind g xs b = Right $ Closure Nothing g xs b

evalExpr :: Env -> Expr -> Either EvalError Value
evalExpr g (Var p s) = case lookup s g of 
    Just (StateDependent as e) ->  Right $ StateDependent as (LocExpr s p e)
    Just (Formula as f) -> Right $ Formula as (LocFormula s p f)
    Just (Closure Nothing env as b) -> Right $ Closure (Just (s,p)) env as b
    Just v  -> Right $ v
    Nothing -> Left $ ScopeError p s
evalExpr g (App e1 e2) = do v1 <- evalExpr g e1; v2 <- evalExpr g e2; app v1 v2
evalExpr g (Freeze p (Var _ n) e2 e3) = do 
      v1 <- evalExpr g e2
      v2 <- evalExpr ((n, StateDependent [] (FreezeVar n p)):g) e3
      makeFreeze v1 v2 
  where 
    makeFreeze (StateDependent as t) (Formula as' f) = Right $ Formula (as ++ as') $ FreezeIn p n t f
    makeFreeze (Independent t) (Formula as' f) = makeFreeze (StateDependent [] (Constant t)) (Formula as' f)
    makeFreeze t (StateDependent as f) = makeFreeze t (Formula as (Atomic f))
    makeFreeze t (Independent f) = makeFreeze t (StateDependent [] (Constant f))
evalExpr g (Freeze p _ _ _) = Left $ FreezeLHSNotVar p
evalExpr g (Literal p l) = Right $ Independent $ LitVal l

app :: Value -> Value -> Either EvalError Value
app (PartialOp o [v1]) v2
 | o `elem` [Equals,NotEquals]  =  binaryEqOp o v1 v2
 | o `elem` [Plus,Times,Divide,Modulo] = binaryArithOp o v1 v2
 | o `elem` [Less,LessEq,Greater,GreaterEq] = binaryComparisonOp o v1 v2 
 | o `elem` [AndOp, OrOp, ImpliesOp] = binaryBooleanOp o v1 v2
 | o `elem` [UntilOp] = binaryTemporalOp o v1 v2
app (PartialOp o []) v 
 | o `elem` [NotOp] = unaryBooleanOp o v
 | o `elem` [AlwaysOp,EventuallyOp,NextOp] = unaryTemporalOp o v
 | o `elem` [MkAccessor] = unarySelectorOp o v

app (PartialOp o vs) v2 = Right $ PartialOp o (vs ++ [v2]) 
app (Closure pos g [n] b) v2 = do 
   r <- evaluate  ((n,v2):g) b
   case pos of 
     Nothing -> pure r
     Just (n,p) -> do 
       case r of 
         Formula as f -> Right $ Formula as (LocFormula n p f)
         StateDependent as t -> Right $ StateDependent as (LocExpr n p t) 
         t -> Right $ t
    
app (Closure p g (n:ns) b) v2 = Right $ Closure p ((n,v2):g) ns b
app v1 v2 = Left $ NotAFunction v1

binaryEqOp o (Independent (LitVal (IntLit i))) (Independent (LitVal (FloatLit j)))
  = binaryEqOp o (Independent (LitVal (FloatLit (fromIntegral i)))) (Independent (LitVal (FloatLit j)))
binaryEqOp o (Independent (LitVal (FloatLit i))) (Independent (LitVal (IntLit j)))
  = binaryEqOp o (Independent (LitVal (FloatLit i))) (Independent (LitVal (FloatLit (fromIntegral j))))
binaryEqOp o (Independent v1) (Independent v2) = Right $ Independent (BoolVal (funFor o v1 v2))
  where funFor Equals    = (==)
        funFor NotEquals = (/=)
binaryEqOp o (StateDependent a1 e1) (StateDependent a2 e2) = Right $ StateDependent (a1 ++ a2) $ Op o [e1,e2]
binaryEqOp o (StateDependent a1 e1) (Independent v2)       = Right $ StateDependent a1 $ Op o [e1, Constant v2]
binaryEqOp o (Independent v1) (StateDependent a2 e2)       = Right $ StateDependent a2 $ Op o [Constant v1, e2]
binaryEqOp o v1 v2 = Left $ BinaryOpOnInvalidTypes o v1 v2

binaryArithOp o (Independent (LitVal (IntLit i))) (Independent (LitVal (IntLit j))) 
  = Right $ Independent (LitVal (IntLit (intFunFor o i j)))
  where intFunFor Plus  = (+)
        intFunFor Times = (*)
        intFunFor Divide = div
        intFunFor Modulo = mod
binaryArithOp Modulo (Independent (LitVal (FloatLit i))) (Independent (LitVal (FloatLit j))) 
  = Left $ ModuloOnFloats i j
binaryArithOp o (Independent (LitVal (FloatLit i))) (Independent (LitVal (FloatLit j)))
  = Right $ Independent (LitVal (FloatLit (floatFunFor o i j)))
  where floatFunFor Plus = (+)
        floatFunFor Times = (*)
        floatFunFor Divide = (/)
binaryArithOp o (Independent (LitVal (IntLit i))) (Independent (LitVal (FloatLit j)))
  = binaryArithOp o (Independent (LitVal (FloatLit (fromIntegral i)))) (Independent (LitVal (FloatLit j)))
binaryArithOp o (Independent (LitVal (FloatLit i))) (Independent (LitVal (IntLit j)))
  = binaryArithOp o (Independent (LitVal (FloatLit i))) (Independent (LitVal (FloatLit (fromIntegral j))))
binaryArithOp o (StateDependent a1 e1) (StateDependent a2 e2) = Right $ StateDependent (a1 ++ a2) $ Op o [e1,e2]
binaryArithOp o (StateDependent a1 e1) (Independent v2)       = Right $ StateDependent a1 $ Op o [e1, Constant v2]
binaryArithOp o (Independent v1) (StateDependent a2 e2)       = Right $ StateDependent a2 $ Op o [Constant v1, e2]
binaryArithOp o v1 v2 = Left $ BinaryOpOnInvalidTypes o v1 v2

compareFunFor Less = (<)
compareFunFor LessEq = (<=)
compareFunFor Greater = (>)
compareFunFor GreaterEq = (>=)

binaryComparisonOp o (Independent (LitVal (IntLit i))) (Independent (LitVal (IntLit j))) 
  = Right $ Independent (BoolVal (compareFunFor o i j))
binaryComparisonOp o (Independent (LitVal (FloatLit i))) (Independent (LitVal (FloatLit j)))
  = Right $ Independent (BoolVal (compareFunFor o i j))
binaryComparisonOp o (Independent (LitVal (IntLit i))) (Independent (LitVal (FloatLit j)))
  = binaryComparisonOp o (Independent (LitVal (FloatLit (fromIntegral i)))) (Independent (LitVal (FloatLit j)))
binaryComparisonOp o (Independent (LitVal (FloatLit i))) (Independent (LitVal (IntLit j)))
  = binaryComparisonOp o (Independent (LitVal (FloatLit i))) (Independent (LitVal (FloatLit (fromIntegral j))))
binaryComparisonOp o (StateDependent a1 e1) (StateDependent a2 e2) = Right $ StateDependent (a1 ++ a2) $ Op o [e1,e2]
binaryComparisonOp o (StateDependent a1 e1) (Independent v2)       = Right $ StateDependent a1 $ Op o [e1, Constant v2]
binaryComparisonOp o (Independent v1) (StateDependent a2 e2)       = Right $ StateDependent a2 $ Op o [Constant v1, e2]
binaryComparisonOp o v1 v2 = Left $ BinaryOpOnInvalidTypes o v1 v2


formulaFor True = Trivial
formulaFor False = Absurd

binaryBooleanOp o (Independent (BoolVal v1)) (Independent (BoolVal v2))
  = Right $ Independent (BoolVal (boolFunFor o v1 v2))
  where boolFunFor AndOp = (&&)
        boolFunFor OrOp  = (||)
        boolFunFor ImpliesOp = (\x y -> not x || y)
binaryBooleanOp o (StateDependent a1 e1) (StateDependent a2 e2) = Right $ StateDependent (a1 ++ a2) $ Op o [e1,e2]
binaryBooleanOp o (StateDependent a1 e1) (Independent v2)       = Right $ StateDependent a1 $ Op o [e1, Constant v2]
binaryBooleanOp o (Independent v1) (StateDependent a2 e2)       = Right $ StateDependent a2 $ Op o [Constant v1, e2]
binaryBooleanOp o (Formula a1 f1) (Formula a2 f2) = Right $ Formula (a1 ++ a2) (formulaFunFor o f1 f2)
  where formulaFunFor AndOp = And
        formulaFunFor OrOp  = Or
        formulaFunFor ImpliesOp = Implies
binaryBooleanOp o (Formula a1 f1) (StateDependent a2 f2) = binaryBooleanOp o (Formula a1 f1) (Formula a2 (Atomic f2))
binaryBooleanOp o (StateDependent a1 f1) (Formula a2 f2) = binaryBooleanOp o (Formula a1 (Atomic f1)) (Formula a2 f2)
binaryBooleanOp o (Independent (BoolVal v1)) (Formula a2 f2) = binaryBooleanOp o (Formula [] (formulaFor v1)) (Formula a2 f2)
binaryBooleanOp o (Formula a1 f1) (Independent (BoolVal v2)) = binaryBooleanOp o (Formula a1 f1) (Formula [] (formulaFor v2))
binaryBooleanOp o v1 v2 = Left $ BinaryOpOnInvalidTypes o v1 v2

binaryTemporalOp o v1 (Independent (BoolVal v2))
  = binaryTemporalOp o v1 (Formula [] (formulaFor v2))
binaryTemporalOp o (Independent (BoolVal v1)) v2
  = binaryTemporalOp o (Formula [] (formulaFor v1)) v2
binaryTemporalOp o (StateDependent a1 e1) (StateDependent a2 e2) 
  = binaryTemporalOp o (Formula a1 (Atomic e1)) (Formula a2 (Atomic e2))
binaryTemporalOp o (Formula a1 f1) (Formula a2 f2) = Right $ Formula (a1 ++ a2) (formulaFunFor o f1 f2)
  where formulaFunFor UntilOp = Until
binaryTemporalOp o (Formula a1 f1) (StateDependent a2 f2) = binaryTemporalOp o (Formula a1 f1) (Formula a2 (Atomic f2))
binaryTemporalOp o (StateDependent a1 f1) (Formula a2 f2) = binaryTemporalOp o (Formula a1 (Atomic f1)) (Formula a2 f2)
binaryTemporalOp o v1 v2 = Left $ BinaryOpOnInvalidTypes o v1 v2

unaryBooleanOp o (Independent (BoolVal v)) = Right $ Independent (BoolVal (funFor o v))
  where funFor NotOp = not
unaryBooleanOp o (StateDependent a1 e1) = Right $ StateDependent a1 $ Op o [e1]
unaryBooleanOp o (Formula a1 f1)  = Right $ Formula a1 (Not f1)
unaryBooleanOp o v = Left $ UnaryOpOnInvalidTypes o v

unaryTemporalOp o (Independent (BoolVal v)) = unaryTemporalOp o (Formula [] (formulaFor v))
unaryTemporalOp o (StateDependent a v) = unaryTemporalOp o (Formula a (Atomic v))
unaryTemporalOp o (Formula a f) = Right $ Formula a (funFor o f)
  where funFor NextOp = Next
        funFor AlwaysOp = Always
        funFor EventuallyOp = Eventually
unaryTemporalOp o v = Left $ UnaryOpOnInvalidTypes o v

unarySelectorOp o (Independent (LitVal (SelectorLit a))) = Right $ StateDependent [a] (Accessor a) 
unarySelectorOp o v = Left $ UnaryOpOnInvalidTypes o v

