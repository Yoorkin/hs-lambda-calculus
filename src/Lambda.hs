module Lambda(
    alias,
    expression,
    insertPair,
    simplify,
    replace,
    Pairs
) where

import Parser
import qualified Data.Map as Map
import Control.Monad.State
import Data.List(delete)

-- free variables of expression
free :: Expression -> [String]
free (Term x) = [x]
free (Abs v x) = delete v (free x)
free (App xs x) = free xs ++ free x

-- substitution  Expression[String := Expression]
substitude :: Expression -> String -> Expression -> Expression
substitude (Term t) x r = if t==x then r else Term t
substitude (App p q) x r = App (substitude p x r) (substitude q x r)
substitude (Abs v e) x r | v==x = Abs v (substitude e x r)
                         | v/=x = if v `elem` free r
                                then substitude (convert (v ++ "'") (Abs v e)) x r
                                else Abs v $ substitude e x r
-- alpha-conversion
convert :: String -> Expression -> Expression
convert v' (Abs v e) = Abs v' (cvtbound v v' e)
        where cvtbound a b (Term v) = Term $ if v==a then b else v
              cvtbound a b (Abs v e) = Abs v $ if v==a then e else cvtbound a b e
              cvtbound a b (App l r) = App (cvtbound a b l) (cvtbound a b r)

-- beta-reduction  (λx.t) s → t[x:=s]
reduce :: Expression -> Expression -> Expression
reduce (Abs v e) x = substitude e v x
reduce l r = App l r


needReduction :: Table -> Expression -> Bool
needReduction _ (App (Abs _ _) _) = True
needReduction t (App (Term v) r) = Map.member v t || needReduction t r
needReduction t (App l r) = needReduction t l || needReduction t r
needReduction t (Abs _ e) = needReduction t e
needReduction _ (Term _) = False

simplify :: Table -> Expression -> Expression
simplify t (App (Term v) r) = case Map.lookup v t of
                                Just x -> simplify t (App x r)
                                Nothing -> App (Term v) (simplify t r)
simplify t (App l r) = let x = reduce (simplify t l) (simplify t r)
                     in if needReduction t (App l r) then simplify t x else x
simplify t (Abs v e) = Abs v $ simplify t e
simplify _ x = x


renameAll' :: Expression -> State (Int,[String]) Expression
renameAll' x = case x of
        (App l r) -> App <$> renameAll' l <*> renameAll' r
        (Term v) -> do (n,cv) <- get
                       if v `elem` cv then return $ Term v
                                      else do put (n+1,cv)
                                              return $ Term $ 'x' : show n
        (Abs v e) -> do (n,cv) <- get
                        put (n+1, v:cv)
                        e' <- renameAll' e
                        put (n+1, cv)
                        return $ convert ('x' : show n) (Abs v e')

normalize :: Expression -> Expression
normalize x = evalState (renameAll' x) (0,[])

type Table = Map.Map String Expression
type Alias = Map.Map Expression String
type Pairs = (Table,Alias)

alias :: Expression -> Pairs -> Maybe String
alias e p = Map.lookup (normalize e) (snd p)

expression :: String -> Pairs -> Maybe Expression
expression s p = Map.lookup s (fst p)

insertPair :: String -> Expression -> Pairs -> Pairs
insertPair s e p = let (table, alias) = p
                       e' = normalize $ simplify (fst p) $ replace (fst p) e
                   in (Map.insert s e table, Map.insert e' s alias)

replace :: Table -> Expression -> Expression
replace t (Abs a e) = Abs a (replace t e)
replace t (App xs x) = App (replace t xs) (replace t x)
replace t (Term v) = case Map.lookup v t of
                                Just x -> replace t x
                                Nothing -> Term v
