{-# LANGUAGE GADTs #-}

module TypeChecker where

import CustomPrelude -- import all
import Exceptions    (TypeException(..))
import Types         -- import all
    
typecheck :: ExprU -> Either TypeException ExprT
typecheck (StringU s) = Right $ StringT s
typecheck (IntU i) = Right $ IntT i
typecheck (BoolU b) = Right $ BoolT b
typecheck (ListU []) = Right $ ListT StringTag []
typecheck (ListU elems@(x : _)) = do
    expectedType <- inferType =<< typecheck x
    checked <- sequence $ typecheck <$> elems
    inferred <- sequence $ inferType <$> checked
    if all (== expectedType) inferred
    then Right $ ListT expectedType checked
    else Left Boop2

checkType :: Type -> ExprT -> Either TypeException ()
checkType StringTag (StringT _) = Right ()
checkType StringTag _ = Left Boop1
checkType IntTag (IntT _) = Right ()
checkType IntTag _ = Left Boop1
checkType BoolTag (BoolT _) = Right ()
checkType BoolTag _ = Left Boop1
checkType (ListTag t) (ListT t' xs) = 
    if t == t'
    then Left Boop2
    else mapM_ (checkType t) xs
checkType (ListTag _) _ = Left Boop1

inferType :: ExprT -> Either TypeException Type
inferType (StringT _) = Right StringTag
inferType (IntT _) = Right IntTag
inferType (BoolT _) = Right BoolTag
inferType (ListT tag xs) = mapM_ (checkType tag) xs >> pure (ListTag tag)
