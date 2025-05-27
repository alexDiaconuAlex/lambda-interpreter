module Binding where

import Lambda

type Context = [(String, Lambda)]

data Line = Eval Lambda -- Eval este un constructor al tipului de date Line
          | Binding String Lambda deriving (Eq) -- Binding este al doilea constructor al tipului de date Line
                                                -- leaga un Macro de Expresia ei din "Context"

instance Show Line where
    show (Eval l) = show l
    show (Binding s l) = s ++ " = " ++ show l

-- 3.1.
simplifyCtx :: Context -> (Lambda -> Lambda) -> Lambda -> Either String [Lambda]
simplifyCtx context stepType e = do
    exprSubstituit <- substitute context e
    return (simplify stepType exprSubstituit)

-- simplifyCtx context stepType e = case substitute context e of
--                                     Left s -> Left s
--                                     Right lambda2 -> Right (simplify stepType lambda2)

-- simplifyCtx [("X", Var "x")] id (Macro "X")
-- - inlocuiește `Macro "X"` cu `Var "x"` → nu e nimic de evaluat → `[Var "x"]`

-- simplifyCtx [("C", App (Macro "K") (Macro "I")), ("K", k), ("I", i)] normalStep (Macro "C")
-- - inlocuiește Macro "C" cu App (Macro "K") (Macro "I")
-- - apoi înlocuiește Macro "K" cu `k`, Macro "I" cu `i`
-- - ajungi la `App k i`
-- - apoi simplifici pas cu pas până rămâne o expresie normală
-- - returnezi toți pașii sub formă de listă

substitute :: Context -> Lambda -> Either String Lambda -- acuma trebuie sa returnez expresia Lambda substituita
substitute context (Var x) = Right (Var x)
substitute context (Abs x e) = do
    exprSubstituita <- (substitute context e)
    return (Abs x exprSubstituita)
substitute context (App e1 e2) = do
    e1Substituita <- (substitute context e1)
    e2Substituita <- (substitute context e2)
    return (App e1Substituita e2Substituita)
substitute context (Macro name) = do
    case lookup name context of
        Just val -> (substitute context val)
        Nothing  -> Left ("Acest Macro nu este in lista de perechi pe nume <context>")


normalCtx :: Context -> Lambda -> Either String [Lambda]
normalCtx ctx = simplifyCtx ctx normalStep

applicativeCtx :: Context -> Lambda -> Either String [Lambda]
applicativeCtx ctx = simplifyCtx ctx applicativeStep
