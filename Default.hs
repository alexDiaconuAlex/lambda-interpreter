module Default where

import Lambda
import Binding

-- Variables (for convenience)
vx = Var "x"
vy = Var "y"
vz = Var "z"
vf = Var "f"
vg = Var "g"
vh = Var "h"
vm = Var "m"
vn = Var "n"
vu = Var "u"
vv = Var "v"

-- Basic combinators
m = Abs "x" $ App vx vx -- \x.(x x)
i = Abs "x" $ vx 
k = Abs "x" $ Abs "y" $ vx -- \x.\y.x
ki = Abs "x" $ Abs "y" $ vy -- \x.\y.y
c = Abs "x" $ Abs "y" $ Abs "z" $ App (App vx vz) vy
y = Abs "f" $ App fix fix -- \f.(\x.(f (x x))    \x.(f (x x)))
  where fix = Abs "x" $ App vf (App vx vx) -- \x.(f (x x))

-- 4.1. Boolean encodings
bTrue = (Abs "x" (Abs "y" vx))
bFalse = (Abs "x" (Abs "y" vy))
bAnd = (Abs "x" (Abs "y" (App (App vx vy) vx)))
bOr = (Abs "x" (Abs "y" (App (App vx vx) vy)))
bNot = (Abs "x" (App (App vx bFalse) bTrue))
bXor = (Abs "x" (Abs "y" (App (App vx (App bNot vy)) vy)))

-- 4.2. Pair encodings
pair = (Abs "x" (Abs "y" (Abs "z" (App (App vz vx) vy))))
first = (Abs "x" (App vx bTrue))
second = (Abs "x" (App vx bFalse))

-- 4.3. Natural number encodings
n0 = (Abs "f" (Abs "x" vx))
n1 = (Abs "f" (Abs "x" (App vf vx)))
n2 = (Abs "f" ((Abs "x" (App vf (App vf vx)))))
nSucc = (Abs "n" (Abs "f" (Abs "x" (App vf (App (App vn vf) vx)))))
nPred = (Abs "n" (Abs "f" (Abs "x" (App (App (App vn (Abs "g" (Abs "h" (App vh (App vg vf))))) (Abs "u" vx)) (Abs "v" vv)))))
nAdd = (Abs "n" (Abs "m" (Abs "f" (Abs "x" (App (App vn vf) (App (App vm vf) vx) )))))
nSub = (Abs "m" (Abs "n" (App (App vn nPred) vm)))
nMult = (Abs "m" (Abs "n" (Abs "f" (App vm (App vn vf)))))
-- nMult = undefined

-- Default Cont t
defaultContext :: Context
defaultContext = 
    [ ("M", m)
    , ("I", i)
    , ("K", k)
    , ("KI", ki)
    , ("C", c)
    , ("Y", y)
    , ("TRUE", bTrue)
    , ("FALSE", bFalse)
    , ("AND", bAnd)
    , ("OR", bOr)
    , ("NOT", bNot)
    , ("XOR", bXor)
    , ("PAIR", pair)
    , ("FST", first)
    , ("SND", second)
    , ("N0", n0)
    , ("N1", n1)
    , ("N2", n2)
    , ("SUCC", nSucc)
    , ("PRED", nPred)
    ,("ADD", nAdd)
    , ("SUB", nSub)
    , ("MULT", nMult)
    ]
