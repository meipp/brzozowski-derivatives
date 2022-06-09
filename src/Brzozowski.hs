module Brzozowski where

data Regex =
    Phi
  | Lambda
  | Symbol Char
  | Concatenation Regex Regex
  | Iterate Regex
  | Not Regex
  | And Regex Regex
  | Or Regex Regex
  deriving (Eq, Show)

delta :: Regex -> Regex
delta Phi = Phi
delta Lambda = Lambda
delta (Symbol a) = Phi
delta (Concatenation p q) = if delta p == Lambda && delta q == Lambda then Lambda else Phi
delta (Iterate p) = Lambda
delta (Not p) = if delta p == Lambda then Phi else Lambda
delta (And p q) = if delta p == Lambda && delta q == Lambda then Lambda else Phi
delta (Or p q) = if delta p == Lambda || delta q == Lambda then Lambda else Phi

deriveSymbol :: Char -> Regex -> Regex
deriveSymbol _ Phi = Phi
deriveSymbol _ Lambda = Phi
deriveSymbol a (Symbol b) = if a == b then Lambda else Phi
deriveSymbol a (Concatenation p q) = Or (Concatenation (deriveSymbol a p) q) (Concatenation (delta p) (deriveSymbol a q))
deriveSymbol a (Iterate p) = Concatenation (deriveSymbol a p) (Iterate p)
deriveSymbol a (Not p) = Not (deriveSymbol a p)
deriveSymbol a (And p q) = And (deriveSymbol a p) (deriveSymbol a q)
deriveSymbol a (Or p q) = Or (deriveSymbol a p) (deriveSymbol a q)

deriveSequence :: [Char] -> Regex -> Regex
deriveSequence [] p = p
deriveSequence (a:as) p = deriveSequence as (deriveSymbol a p)

match :: Regex -> [Char] -> Bool
match p as = delta (deriveSequence as p) == Lambda

normalForm :: [Char] -> Regex -> Regex
-- TODO deep
normalForm alphabet p = foldl Or (delta p) (map (\a -> Concatenation (Symbol a) (deriveSymbol a p)) alphabet)

simplify :: Regex -> Regex
simplify Phi = Phi
simplify Lambda = Lambda
simplify (Symbol a) = Symbol a
simplify (Concatenation Phi _) = Phi
simplify (Concatenation _ Phi) = Phi
simplify (Concatenation Lambda p) = simplify p
simplify (Concatenation p Lambda) = simplify p
simplify (Concatenation p q) = Concatenation (simplify p) (simplify q)
simplify (Iterate Phi) = Phi
simplify (Iterate Lambda) = Lambda
simplify (Iterate p) = Iterate (simplify p)
simplify (Not p) = Not (simplify p)
simplify (And Phi _) = Phi
simplify (And _ Phi) = Phi
simplify (And p q) = And (simplify p) (simplify q)
simplify (Or Phi p) = simplify p
simplify (Or p Phi) = simplify p
simplify (Or p q) = Or (simplify p) (simplify q)

fullSimplify :: Regex -> Regex
fullSimplify p = if p == p' then p else fullSimplify p'
  where p' = simplify p

depth :: Regex -> Integer
depth Phi = 0
depth Lambda = 0
depth (Symbol _) = 0
depth (Concatenation p q) = 1 + max (depth p) (depth q)
depth (Iterate p) = 1 + depth p
depth (Not p) = 1 + depth p
depth (And p q) = 1 + max (depth p) (depth q)
depth (Or p q) = 1 + max (depth p) (depth q)

length' :: Regex -> Integer
length' Phi = 0
length' Lambda = 0
length' (Symbol _) = 1
length' (Concatenation p q) = length' p + length' q
length' (Iterate p) = length' p + 1
length' (Not p) = length' p + 1
length' (And p q) = length' p + length' q + 1
length' (Or p q) = length' p + length' q + 1

i :: Regex
i = Not Phi
