module Lambda where

import Data.List (nub, (\\))

data Lambda = Var String -- constructori
            | App Lambda Lambda
            | Abs String Lambda
            | Macro String

instance Show Lambda where
    show (Var x) = x -- x
    show (App e1 e2) = "(" ++ show e1 ++ " " ++ show e2 ++ ")" -- (e1, e2)
    show (Abs x e) = "λ" ++ x ++ "." ++ show e -- \x.e
    show (Macro x) = x

instance Eq Lambda where
    e1 == e2 = eq e1 e2 ([],[],[])
      where
        eq (Var x) (Var y) (env,xb,yb) = elem (x,y) env || (not $ elem x xb || elem y yb)
        eq (App e1 e2) (App f1 f2) env = eq e1 f1 env && eq e2 f2 env
        eq (Abs x e) (Abs y f) (env,xb,yb) = eq e f ((x,y):env,x:xb,y:yb)
        eq (Macro x) (Macro y) _ = x == y
        eq _ _ _ = False

-- 1.1.
vars :: Lambda -> [String]
vars (Var x) = [x]
vars (App e1 e2) = (nub ((vars e1) ++ (vars e2)))
vars (Abs s e) = nub(s : (vars e))

-- 1.2.
freeVars :: Lambda -> [String]
freeVars (Var x) = [x]
freeVars (App e1 e2) = (nub ((freeVars e1) ++ (freeVars e2)))
freeVars (Abs x e) = (nub (filter(\y -> not(x == y)) (freeVars e)))

-- 1.3.
recursiveAlphabet :: [String] -> [String]
recursiveAlphabet set1 = [a ++ b | a <- set1, b <- "":alphabet]
  where
    alphabet = map (:[]) ['a'..'z']  --'a':[] ..

generateNames 1 = map (:[]) ['a'..'z'] ++ (generateNames 2)

generateNames n = (recursiveAlphabet (generateNames (n - 1))) ++ (generateNames (n + 1))
--a b .. z aa ab ac .. zz ++ aaa aab .. zzz => [a b .. z aa ab ac .. zz aaa aab .. zzz]
newVar :: [String] -> String
newVar vars = (head (filter(\x -> (notElem x vars)) (generateNames 1))) --datorita evaluarei lenese se opreste cand gaseste primul element de la head 
-- vars: [a c b e] -> [d].. 

-- 1.4.
isNormalForm :: Lambda -> Bool
isNormalForm (Var _) = True
isNormalForm (Abs _ e) = (isNormalForm e) -- \orice.e
isNormalForm (App (Abs _ _) _) = False
isNormalForm (App _ (Abs _ _)) = False
isNormalForm (App e1 e2) = (isNormalForm e1) && (isNormalForm e2) -- (e1 e2)

-- 1.5.

helperReduce :: String -> String -> Lambda -> Lambda
helperReduce old a (App (Var x) (Var y)) = 
  if (old == x && old == y) then (App (Var a) (Var a))
  else if (old == x) then (App (Var a) (Var y))
  else if (old == y) then (App (Var x) (Var a))
  else (App (Var x) (Var y))
helperReduce old a (Abs y e1) =
  (Abs a (helperReduce old a e1))
helperReduce old a (App e1 e2) =
  App(helperReduce old a e1) (helperReduce old a e2)

helperReduce old a (Var x) = if (old == x) then (Var a)
                          else (Var x)

reduce :: String -> Lambda -> Lambda -> Lambda
reduce x (Var y) e = if (x == y) then e
                      else (Var y)
reduce x (App e1 e2) e = App(reduce x e1 e) (reduce x e2 e)
reduce x (Abs y e1) e = if (x == y) then (Abs y e1)
                        else if (notElem y (freeVars e)) then (Abs y (reduce x e1 e))
                        else 
                          let a = newVar (x : y : freeVars e1 ++ freeVars e) -- let a be a different variable than exist
                          -- here substitute the y with a in e1
                              eAux = (helperReduce y a e1) -- \y.(x a)
                          in (reduce x (Abs a eAux) e)

-- 1.6.
normalStep :: Lambda -> Lambda
normalStep (Var x) = (Var x)
normalStep (App(Abs x e1) e2) = (reduce x e1 e2)
normalStep (App(App(Abs x e1) p1) e2) =
  App(reduce x e1 p1) e2

-- \a.(\x.e1 e2) -> w
normalStep (Abs a (App (Abs x e1) e2)) =
  Abs a (reduce x e1 e2)

-- + (pt simplifyCtx)
normalStep (App e1 e2) =
  if (not (isNormalForm e1)) then (App (normalStep e1) e2)
  else (App e1 (normalStep e2))
normalStep (Abs x e1) = (Abs x (normalStep e1))

-- 1.7.
applicativeStep :: Lambda -> Lambda
applicativeStep (Var x) = Var x
applicativeStep (Abs x e) = Abs x (applicativeStep e)

applicativeStep (App (Abs x e1) e2) =
  if (not (isNormalForm e2)) then App (Abs x e1) (applicativeStep e2)
  else reduce x e1 e2

applicativeStep (App e1 e2) =
  if (not (isNormalForm e1)) then (App (applicativeStep e1) e2)
  else (App e1 (applicativeStep e2))

-- 1.8.
simplify :: (Lambda -> Lambda) -> Lambda -> [Lambda]
simplify stepType e = if (isNormalForm e) then [e]
                      else e : simplify stepType (stepType e)

normal :: Lambda -> [Lambda]
normal = simplify normalStep

applicative :: Lambda -> [Lambda]
applicative = simplify applicativeStep
