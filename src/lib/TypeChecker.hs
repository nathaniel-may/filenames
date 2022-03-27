{-# LANGUAGE GADTs, FlexibleContexts, TupleSections #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module TypeChecker where

import           Control.Monad.Except -- import all
import           Control.Monad.Reader -- import all
import           CustomPrelude        -- import all
import qualified Data.Map             as M
import           Exceptions           (TypeException(..))
import           Types                -- import all


typecheck :: MonadError TypeException m => ExprU -> m ExprT
typecheck x = snd <$> runReaderT (typecheck_ x) builtins

typecheck_ :: (MonadReader Env m, MonadError TypeException m) => ExprU -> m Closure
-- nothing in source file
typecheck_ (RootU []) = throwError EmptySourceFile

-- program root is defined by assignment to value "format"
typecheck_ (RootU exprs) = do
    closures <- traverse typecheck_ exprs
    types <- traverse inferType closures
    let mismatches = filter (/=UnitTag) types
    -- all top-level definitions must be assignments. Should be enforced here, not at parse time.
    _ <- case mismatches of
        []  -> pure ()
        -- only reporting first mismatch if there are several. could change the exception to show them all.
        (got : _) -> throwError $ TopLevelNotAssignment got
    table <- ask
    let (tables, _) = unzip closures
    let finalTable = foldr M.union table tables
    finalExprt <- maybe (throwError FormatNotFound) pure (M.lookup (Name "format") finalTable)
    pure (finalTable, finalExprt)

typecheck_ (StringU s) =
    asks (, StringT s)

typecheck_ (IntU i) =
    asks (, IntT i)

typecheck_ (BoolU b) =
    asks (, BoolT b)

typecheck_ (ListU []) =
    asks (, ListT StringTag [])

typecheck_ (ListU elems@(x : _)) = do
    table <- ask
    expectedType <- inferType =<< typecheck_ x
    checked <- traverse typecheck_ elems
    inferred <- traverse inferType checked
    let mismatches = filter (/=expectedType) inferred
    case mismatches of
        [] -> pure (table, ListT expectedType $ snd <$> checked)
        (got : _) -> throwError (ListTypeMismatch expectedType got)

typecheck_ (AssignmentU name v) = do
    (table2, exprt) <- typecheck_ v
    -- TODO this overwrites assignments. should check if it exists first
    pure (M.insert name exprt table2, UnitT)

typecheck_ (IdentifierU name) = do
    table <- ask
    maybe (throwError $ NoValueNamed name) (pure . (table,)) (M.lookup name table)

typecheck_ (ApplyU f e) = do
    (table, ft) <- typecheck_ f
    tf <- inferType (table, ft)
    (table, et) <- typecheck_ e
    te <- inferType (table, et)
    table <- ask
    case ft of 
        (FnT name (FnTag p ret) applied) -> 
            if p /= te
            then throwError $ TypeMismatch p te
            else pure (table, FnT name ret (applied <> [et])) -- TODO should I reverse this order instead?
        _ -> throwError $ CannotApplyNotAFunction tf te


checkType :: (MonadError TypeException m) => Closure -> Type -> m ()
checkType (_, UnitT) UnitTag = pure ()
checkType closure UnitTag = throwError . TypeMismatch UnitTag =<< inferType closure
checkType (_, StringT _) StringTag = pure ()
checkType closure StringTag = throwError . TypeMismatch StringTag =<< inferType closure
checkType (_, IntT _) IntTag = pure ()
checkType closure IntTag = throwError . TypeMismatch IntTag =<< inferType closure
checkType (_, BoolT _) BoolTag = pure ()
checkType closure BoolTag = throwError . TypeMismatch BoolTag =<< inferType closure
checkType (table, ListT t' xs) expected@(ListTag t) = 
    if t /= t'
    then throwError $ TypeMismatch expected (ListTag t')
    else traverse_ (\x -> checkType (table, x) t) xs
checkType closure expected@(ListTag _) = throwError . TypeMismatch expected =<< inferType closure
checkType (table, ApplyT f e) expected = do
    te <- inferType (table, e)
    tf <- inferType (table, f)
    case tf of
        (FnTag _ ret) ->
            if ret /= expected
            then throwError $ TypeMismatch expected ret
            else pure ()
        nonfn -> throwError $ CannotApplyNotAFunction nonfn te
checkType closure FnTag{} = throwError . TypeMismatch BoolTag =<< inferType closure


inferType :: (MonadError TypeException m) => Closure -> m Type
inferType (_, UnitT) = pure UnitTag
inferType (_, StringT _) = pure StringTag
inferType (_, IntT _) = pure IntTag
inferType (_, BoolT _) = pure BoolTag
inferType closure@(table, ListT tag _) = runReaderT (checkType closure (ListTag tag)) table $> ListTag tag
inferType (table, ApplyT f e) = do
    tf <- inferType (table, f)
    te <- inferType (table, e)
    case tf of
        (FnTag p ret) -> 
            if p /= te
            then throwError $ TypeMismatch p te
            else pure ret
        _ -> throwError $ CannotApplyNotAFunction tf te
inferType (_, FnDefT{}) = pure UnitTag
-- TODO I'm just blindly believing it's right here. is that ok?
inferType (_, FnT _ tag _) = pure tag


builtins :: Env
builtins = M.fromList [
    (Name "<",   FnDefT (Name "<")  BoolTag [IntTag, IntTag])
  , (Name ">",   FnDefT (Name ">")  BoolTag [IntTag, IntTag])
  , (Name "==",  FnDefT (Name "==") BoolTag [IntTag, IntTag])
  , (Name "<=",  FnDefT (Name "<=") BoolTag [IntTag, IntTag])
  , (Name ">=",  FnDefT (Name ">=") BoolTag [IntTag, IntTag])
  ]
