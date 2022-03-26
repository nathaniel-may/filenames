{-# LANGUAGE GADTs, FlexibleContexts, TupleSections #-} -- TODO is FlexibleContexts necessary?

module TypeChecker where

import           Control.Monad.Error  -- import all
import           Control.Monad.Except -- import all
import           Control.Monad.Reader -- import all
import           CustomPrelude        -- import all
import qualified Data.Map as M
import           Exceptions           (TypeException(..))
import           Types                -- import all


typecheck :: ExprU -> Either TypeException ExprT
-- typecheck x = snd <$> runReader (typecheck_ x :: ReaderT Env (Either TypeException) Closure) builtins
typecheck x = snd <$> runReader (runExceptT (typecheck_ x)) builtins

type Closure = (Env, ExprT)

typecheck_ :: (MonadReader Env m, MonadError TypeException m) => ExprU -> m Closure -- TODO OLD TYPE: ValueTable -> ExprU -> Either TypeException (ValueTable, ExprT)
-- nothing in source file
typecheck_ (RootU []) = throwError EmptySourceFile

-- program root is defined by assignment to value "format"
-- typecheck_ (RootU (expr : exprs)) = do
--     table <- ask
--     (table1, exprt) <- typecheck_ expr
--     tablesNExprts <- traverse typecheck_ exprs
--     let (tables, exprts) = unzip tablesNExprts
--     types <- traverse (inferType table) (exprt : exprts)
--     let mismatches = filter (/=UnitTag) types
--     -- all top-level definitions must be assignments. Should be enforced here, not at parse time.
--     _ <- case mismatches of
--         []  -> pure $ Right ()
--         -- only reporting first mismatch if there are several. could change the exception to show them all.
--         (got : _) -> pure . Left $ TopLevelNotAssignment got
--     let finalTable = foldr M.union table1 tables
--     finalExprt <- maybeToRight FormatNotFound (M.lookup (Name "format") finalTable)
--     pure (finalTable, finalExprt)

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

-- typecheck_ table (AssignmentU name v) = do
--     (table2, exprt) <- typecheck_ table v
--     -- TODO this overwrites assignments. should check if it exists first
--     pure (M.insert name exprt table2, UnitT)

-- typecheck_ table (IdentifierU name) = do
--     exprt <- maybeToRight (NoValueNamed name) (M.lookup name table)
--     pure (table, exprt)

-- typecheck_ table (FnCallU name params) = do
--     exprt <- maybeToRight (NoValueNamed name) (M.lookup name table)
--     case exprt of
--         (FnDefT _ ret' params') -> do
--             -- throws away the tables from param typechecking
--             -- no additional definitions should be added there
--             (_, checked) <- unzip <$> traverse (typecheck_ table) params
--             inferred <- traverse (inferType table) checked
--             let mismatches = filter (uncurry (/=)) (params' `zip` inferred)
--             case mismatches of
--                 [] -> Right (table, FnCallT name ret' checked)
--                 ((expected, got) : _) -> Left $ TypeMismatchFnParam name expected got
--         got -> Left . NotAFunction name =<< inferType table got



checkType :: (MonadReader Env m, MonadError TypeException m) => Closure -> Type -> m ()
-- TODO OLD TYPE: Env -> Type -> ExprT -> Either TypeException ()
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
checkType (table, FnCallT name ret' params') (FnTag ret params) = do
    if ret /= ret' then throwError $ TypeMismatchFnReturn name ret ret' else pure ()
    let lparams = length params
    let lparams' = length params'
    if' (lparams /= lparams') (throwError $ TypeMismatchNumFnParams name lparams lparams') (pure ())
    inferred <- traverse (curry inferType table) params'
    let mismatches = filter (uncurry (/=)) (params `zip` inferred)
    case mismatches of
        [] -> pure ()
        ((expected, got) : _) -> throwError $ TypeMismatchFnParam name expected got
checkType closure FnTag{} = throwError . TypeMismatch BoolTag =<< inferType closure

inferType :: (MonadReader Env m, MonadError TypeException m) => Closure -> m Type
-- TODO OLD TYPE: Env -> ExprT -> Either TypeException Type
inferType (_, UnitT) = pure UnitTag
inferType (_, StringT _) = pure StringTag
inferType (_, IntT _) = pure IntTag
inferType (_, BoolT _) = pure BoolTag
inferType closure@(table, ListT tag xs) = checkType closure (ListTag tag) $> ListTag tag
inferType (table, FnCallT _ ret params) = FnTag ret <$> traverse (curry inferType table) params
inferType (_, FnDefT{}) = pure UnitTag

builtins :: Env
builtins = M.fromList [
    (Name "<",   FnDefT (Name "<")  BoolTag [IntTag, IntTag])
  , (Name ">",   FnDefT (Name ">")  BoolTag [IntTag, IntTag])
  , (Name "==",  FnDefT (Name "==") BoolTag [IntTag, IntTag])
  , (Name "<=",  FnDefT (Name "<=") BoolTag [IntTag, IntTag])
  , (Name ">=",  FnDefT (Name ">=") BoolTag [IntTag, IntTag])
  ]
