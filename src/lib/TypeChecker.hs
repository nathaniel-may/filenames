{-# LANGUAGE GADTs, FlexibleContexts, TupleSections, LambdaCase #-}
{-# OPTIONS_GHC -Wno-name-shadowing #-}

module TypeChecker where

import           Control.Monad.Except -- import all
import           Control.Monad.Reader -- import all
import           CustomPrelude        -- import all
import qualified Data.Map             as M
import           Exceptions           (TypeException(..))
import           Types                -- import all


typecheck :: MonadError TypeException m => ExprU -> m ExprT
typecheck x = do
    root <- snd <$> runReaderT (typecheck_ x) builtins
    case root of
        FnT (Name "schema") _ _ -> pure root
        _ -> throwError TopLevelMustBeSchema


typecheck_ :: (MonadReader Env m, MonadError TypeException m) => ExprU -> m Closure
typecheck_ (StringU s) =
    asks (, StringT s)

-- TODO properly type the hole
typecheck_ (ListU []) =
    asks (, ListT StringTag [])

typecheck_ (ListU elems@(x : _)) = do
    table <- ask
    expectedType <- inferType =<< typecheck_ x
    typed <- traverse typecheck_ elems
    let val = ListT expectedType $ fmap snd typed
    checkType (table, val) (ListTag expectedType)
    pure (table, val)

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


-- TODO put Type before Closure so it can be curried better?
checkType :: (MonadError TypeException m) => Closure -> Type -> m ()
checkType (_, StringT _) StringTag = pure ()
checkType (_, AttributeT _) AttributeTag = pure ()
-- empty lists are the types they say they are
checkType (_, ListT _ []) (ListTag _) = pure ()
checkType (table, ListT t' xs) expected@(ListTag t) = 
    if t /= t'
    then throwError $ TypeMismatch expected (ListTag t')
    else traverse_ (\x -> checkType (table, x) t) xs
checkType (table, RecordT name fields) expected@(RecordTag tagName fieldTags) =
    if tagName /= name
    then (throwError . TypeMismatch expected) . RecordTag name =<< traverse (\(name, expr) -> (name,) <$> inferType (table, expr)) fields
    else traverse_ (\((_, expr), (_, tag)) -> checkType (table, expr) tag) (fields `zip` fieldTags)
checkType closure expected@(ListTag _) = throwError . TypeMismatch expected =<< inferType closure
checkType closure expected@FnTag{} = do
    got <- inferType closure
    when (got /= expected) (throwError $ TypeMismatch expected got)
checkType closure tag = throwError . TypeMismatch tag =<< inferType closure

inferType :: (MonadError TypeException m) => Closure -> m Type
inferType (_, StringT _) = pure StringTag
inferType (_, AttributeT _) = pure AttributeTag
-- empty lists are the types they say they are
inferType (_, ListT tag []) = pure (ListTag tag)
inferType (table, ListT _ (x : _)) = ListTag <$> inferType (table, x)
inferType (_, FnT _ tag _) = pure tag
inferType (table, RecordT name fields) =
    RecordTag name <$> traverse (\(fieldName, expr) -> (fieldName,) <$> inferType (table, expr)) fields

builtins :: Env
builtins = M.fromList
  [ (Name "part", FnT (Name "part") (FnTag StringTag (FnTag (ListTag StringTag) (FnTag (ListTag AttributeTag) (RecordTag (Name "part") [(Name "name", StringTag), (Name "values", ListTag StringTag), (Name "attributes", ListTag AttributeTag)])))) [])
  , (Name "schema", FnT (Name "schema") (FnTag StringTag (FnTag (ListTag (RecordTag (Name "part") [(Name "name", StringTag), (Name "values", ListTag StringTag), (Name "attributes", ListTag AttributeTag)]))(ListTag (RecordTag (Name "schema") [(Name "delimiter", StringTag), (Name "parts", ListTag (RecordTag (Name "part") [(Name "name", StringTag), (Name "values", ListTag StringTag), (Name "attributes", ListTag AttributeTag)]))])))) [])
  , (Name "optional", AttributeT Optional)
  , (Name "unique", AttributeT Unique)
  ]
