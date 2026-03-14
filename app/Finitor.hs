{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE RecordWildCards #-}

module Finitor where

import Language
import Data.Map
import Data.List hiding (lookup, insert)
import Prelude hiding (lookup)
import Data.Maybe
import Control.Monad.State.Lazy
import Debug.Trace
import Control.Monad (when)

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
            -- traceM ("Elaborating " ++ var ++ " budget " ++ show b)
            let var' = generateVarName var b
            currBudget <- gets (fromMaybe Unlimited . lookup var' . budgets)
            -- let var' = generateVarNameIfNeeded var b currBudget
            case currBudget of
                Finite b' -> if b == b' then return $ Just var' else error "Inconsistent elaborations" -- already elaborated
                Unlimited -> do
                    budgets' <- gets (insert var' (Finite b) . budgets)
                    modify (modifyBudgets budgets')
                    ps <- gets (fromJust . lookup var . prods)
                    ps' <- concat <$> mapM (fmap (fromMaybe []) . elaborateProd var' (Finite b)) ps
                    if Prelude.null ps' then do 
                        budgets' <- gets (Data.Map.delete var' . budgets)
                        modify (modifyBudgets budgets')
                        return Nothing
                    else do
                        prods' <- gets (insert var' ps' . prods)
                        modify (modifyProds prods')
                        -- budgets' <- gets (insert var' (Finite b) . budgets)
                        -- modify (modifyBudgets budgets')
                        return $ Just var'
        elaborateVar var Unlimited = do
            -- traceM ("Elaborating " ++ var ++ " unlimited")
            ps <- gets (fromJust . lookup var . prods)
            ps' <- concat <$> mapM (fmap (fromMaybe []) . elaborateProd var Unlimited) ps
            if Prelude.null ps' then return Nothing else do
                prods' <- gets (insert var ps' . prods)
                modify (modifyProds prods')
                return $ Just var

        -- I desire a version of this production that generates at most this many chars
        elaborateProd :: String -> Budget -> Production -> State CFG (Maybe [Production])
        elaborateProd name' (Finite b) p@(Terminal name cr) = if rangeBudget cr > b then return Nothing else do
            -- traceM ("Elaborating prod " ++ showWithBudget (Finite b) p)
            when (name /= name') $ error "Incorrect target name for terminal" 
            newBudgets <- insert name (Finite (rangeBudget cr)) <$> gets budgets 
            modify (modifyBudgets newBudgets)
            return $ Just $ [Terminal name cr]
        elaborateProd name (Finite b) p@(NonTerminal _ l r) = if b < 0 then return Nothing else do
            -- traceM ("Elaborating prod " ++ showWithBudget (Finite b) p)
            rangeL <- getRangeIfTerminal l
            rangeR <- getRangeIfTerminal r
            case (rangeL, rangeR) of
                (Just crL, Just crR) -> if (rangeBudget crL + rangeBudget crR) > b then return Nothing else return $ Just [NonTerminal name l r]
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
        elaborateProd name Unlimited p@(NonTerminal _ l r) = do
            -- traceM ("Elaborating prod " ++ showWithBudget Unlimited p)
            lb <- gets (fromMaybe Unlimited . lookup l . budgets)
            rb <- gets (fromMaybe Unlimited . lookup r . budgets)
            l' <- elaborateVar l lb
            r' <- elaborateVar r rb
            return $ (:[]) . uncurry (NonTerminal name) <$> ((,) <$> l' <*> r')
        elaborateProd name Unlimited (Terminal name' t) = if name /= name' then error "Incorrect names" else return $ Just [Terminal name t]