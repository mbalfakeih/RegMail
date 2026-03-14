{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Finitor where

import Language
import Data.Map
import Data.List hiding (lookup, insert)
import Prelude hiding (lookup)
import Data.Maybe
import Control.Monad.State.Lazy

rangeBudget :: CharRange -> Int
rangeBudget cr = if (end cr) - (start cr) < 1 then 0 else 1

getRangeIfTerminal :: String -> State CFG (Maybe CharRange)
getRangeIfTerminal var = do
    ps <- gets (lookup var . prods)
    case ps of
        Just [Terminal _ cr] -> return $ Just cr
        _ -> return Nothing

generateVarName :: String -> Int -> String
generateVarName var b = var ++ "__lim" ++ show b

elaborateBudgets ::  CFG -> Maybe CFG
elaborateBudgets cfg= case runState (elaborateVar (startRule cfg) (fromMaybe Unlimited $ lookup (startRule cfg) $ budgets cfg)) cfg of
    (Nothing, _) -> Nothing
    (Just start', cfg') -> Just (CFG { startRule = start', prods = prods cfg', budgets = budgets cfg'  })
    where
        -- I desire this variable with this budget. Returns new CFG and name of the variable or Nothing if this is impossible (in which case CFG is unchanged)
        elaborateVar :: String -> Budget -> State CFG (Maybe String)
        elaborateVar var (Finite b) = do
            case var' of
                Nothing -> return $ Just var -- already elaborated
                Just var' -> do
                    ps <- gets (fromJust . lookup var . prods)
                    ps' <- concat <$> mapM (fmap (fromMaybe []) . elaborateProd var' b) ps
                    if Prelude.null ps' then return Nothing else do
                        prods' <- gets (insert var' ps' . prods)
                        modify (modifyProds prods')
                        return $ Just var'
                 
            where
                var' = do
                    currBudget <- gets (fromMaybe Unlimited . lookup var . budgets)
                    case currBudget of
                        Unlimited -> Just $ generateVarName var b
                        (Finite b') -> if b == b' && "__lim" `isSubsequenceOf` var then Nothing else Just $ generateVarName var b
        elaborateVar var Unlimited = do
            ps <- gets (fromJust . lookup var . prods)
            ps' <- concat <$> mapM (fmap (fromMaybe []) . elaborateProd var Unlimited) ps
            if Prelude.null ps' then return Nothing else return $ Just var

        -- I desire a version of this production that generates at most this many chars
        elaborateProd :: String -> Budget -> Production -> State CFG (Maybe [Production])
        elaborateProd _ (Finite b) (Terminal name cr) = if rangeBudget cr > b then return Nothing else do
            newBudgets <- insert name (Finite (rangeBudget cr)) <$> gets budgets 
            modify (modifyBudgets newBudgets)
            return $ Just $ [Terminal name cr]
        elaborateProd name (Finite b) t@(NonTerminal _ l r) = if b < 0 then return Nothing else do
            rangeL <- getRangeIfTerminal l
            rangeR <- getRangeIfTerminal r
            case (rangeL, rangeR) of
                (Just crL, Just crR) -> if (rangeBudget crL + rangeBudget crR) > b then return Nothing else return $ Just t
                (Just cr, Nothing) -> do
                    let remBudget = b - (rangeBudget cr)
                    r' <- elaborateVar r (Finite remBudget)
                    return $ (:[]) . NonTerminal name l <$> r'
                (Nothing, Just cr) -> do
                    let remBudget = b - (rangeBudget cr)
                    l' <- elaborateVar l (Finite remBudget)
                    return $ (\l -> [NonTerminal name l r]) <$> l'
                (Nothing, Nothing) -> do
                        res <- helper b
                        if Prelude.null res then return Nothing else return $ Just res
                    where
                        helper :: Int -> State CFG [Production]
                        helper (-1) = return []
                        helper b' = do
                            l' <- elaborateVar l (Finite b')
                            r' <- elaborateVar r (Finite (b - b'))
                            rest <- helper (b' - 1)
                            return $ maybeToList (uncurry (NonTerminal name) <$> ((,) <$> l' <*> r')) ++ rest
        elaborateProd name Unlimited (NonTerminal _ l r) = do
            lb <- gets (fromMaybe Unlimited . lookup l . budgets)
            rb <- gets (fromMaybe Unlimited . lookup r . budgets)
            l' <- elaborateVar l lb
            r' <- elaborateVar r rb
            return $ (:[]) . uncurry (NonTerminal name) <$> ((,) <$> l' <*> r')
        elaborateProd name Unlimited (Terminal _ t) = return $ Just [Terminal name t]