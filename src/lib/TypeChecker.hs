{-# LANGUAGE GADTs, FlexibleContexts, TupleSections, LambdaCase #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module TypeChecker where

import           Control.Monad.Except -- import all
import           Control.Monad.Reader -- import all
import           CustomPrelude        -- import all
import qualified Data.Map             as M
import           Exceptions           (TypeException(..))
import           Types                -- import all


typecheckFromRoot :: MonadError TypeException m => ExprU -> m ExprT
typecheckFromRoot body@(BodyU assignments) = do
    let nonAssignment = filter (\case {AssignmentU{} -> False; _ -> True}) assignments
    case nonAssignment of
        [] -> pure ()
        _ -> throwError TopLevelNotAssignment
    (table, _) <- runReaderT (typecheck_ body) builtins
    -- program root is defined by top-level assignment to value "format"
    case M.lookup (Name "format") table of
        Nothing  -> throwError FormatNotFound
        (Just root) -> pure root
-- TODO is this the best error? maybe split up into multiple errors?
typecheckFromRoot _ = throwError TopLevelNotAssignment


typecheck :: MonadError TypeException m => ExprU -> m ExprT
typecheck x = snd <$> runReaderT (typecheck_ x) builtins


typecheck_ :: (MonadReader Env m, MonadError TypeException m) => ExprU -> m Closure
-- nothing in source file
typecheck_ (BodyU []) = throwError EmptyBody

typecheck_ (BodyU exprs) = do
    closures <- traverse typecheck_ exprs
    let (_, types) = unzip closures
    let nonUnitClosures = takeWhile (/=UnitT) types
    table <- ask
    -- all top-level definitions must be assignments. Should be enforced here, not at parse time.
    finalExprt <- case nonUnitClosures of
        [] -> pure UnitT  -- expected at the top level but nowhere else really (TODO make special case ala BodyU True for root?)
        [return] -> pure return
        unassigned -> throwError . MultipleUnassignedValuesInBody =<< traverse (curry inferType table) unassigned
    let (tables, _) = unzip closures
    let finalTable = foldr M.union table tables
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
    traceM ("**********" <> tshow ft <> " APPLIED TO " <> tshow et)
    te <- inferType (table, et)
    table <- ask
    case ft of 
        (FnT name (FnTag p ret) applied) -> 
            if p /= te
            then throwError $ TypeMismatch p te
            else pure (table, FnT name ret (applied <> [et])) -- TODO should I reverse this order instead?
        _ -> throwError $ CannotApplyNotAFunction tf te


-- TODO put Type before Closure so it can be curried better?
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
-- TODO fix this vv
checkType closure FnTag{} = throwError . TypeMismatch BoolTag =<< inferType closure
-- TODO stub till Parsers have a type representation
checkType closure ParserTag{} = throwError . TypeMismatch BoolTag =<< inferType closure


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
    (Name "delim",    FnT (Name "delim")    (FnTag StringTag (FnTag (FnTag IntTag BoolTag) (FnTag (ListTag StringTag) ParserTag))) [])
  , (Name "no_delim", FnT (Name "no_delim") (FnTag (FnTag IntTag BoolTag) (FnTag (ListTag StringTag) ParserTag)) [])
  , (Name "<",  FnT (Name "<")  (FnTag IntTag (FnTag IntTag BoolTag)) [])
  , (Name ">",  FnT (Name ">")  (FnTag IntTag (FnTag IntTag BoolTag)) [])
  , (Name "==", FnT (Name "==") (FnTag IntTag (FnTag IntTag BoolTag)) [])
  , (Name "<=", FnT (Name "<=") (FnTag IntTag (FnTag IntTag BoolTag)) [])
  , (Name ">=", FnT (Name ">=") (FnTag IntTag (FnTag IntTag BoolTag)) [])
  ]
