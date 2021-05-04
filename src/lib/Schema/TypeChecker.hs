module Schema.TypeChecker where

import           CustomPrelude      -- import all
import           Schema.Exceptions  -- import all
import           Schema.Types       -- import all


-- | top-level entry point to type checking
typecheck :: Expr -> Either CompileException ExprT
typecheck (StringU str) = Right $ String str
typecheck (CharU mc) = Right $ Char mc
typecheck (ListU elems@(x : _)) = do
    expectedElemType <- inferType =<< typecheck x
    checked <- traverse typecheck elems
    let possibility = List expectedElemType checked
    fmap (const possibility) (checkType (ListTag expectedElemType) possibility)
typecheck (ListU _) = Right $ List StringTag []

checkType :: Type -> ExprT -> Either CompileException ()
checkType StringTag (String _) = Right ()
checkType tag (String _) = Left $ TypeException tag StringTag
checkType CharTag (Char _) = Right ()
checkType tag (Char _) = Left $ TypeException tag CharTag
checkType (ListTag t) (List t' xs) = 
    if t /= t'
    then Left $ TypeException (ListTag t) (ListTag t')
    else mapM_ (checkType t) xs
checkType t (List t' _) = Left $ TypeException (ListTag t) (ListTag t')

inferType :: ExprT -> Either CompileException Type
inferType (String _) = Right StringTag
inferType (Char _) = Right CharTag
inferType (List tag xs) = mapM_ (checkType tag) xs >> pure (ListTag tag)
