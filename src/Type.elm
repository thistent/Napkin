module Type exposing (..)


type Kind ann
    = Type ann
    | KindArrow ann (Kind ann) (Kind ann)


type Type tyname uni ann
    = TyVar ann tyname
    | TyFun ann (Type tyname uni ann) (Type tyname uni ann)
    | TyIFix ann (Type tyname uni ann) (Type tyname uni ann)
    | TyForall ann tyname (Kind ann) (Type tyname uni ann)
    | TyBuiltin ann (Some (TypeIn uni))
    | TyLam ann tyname (Kind ann) (Type tyname uni ann)
    | TyApp ann (Type tyname uni ann) (Type tyname uni ann)


type StaticBuiltinName
    = AddInteger
    | SubtractInteger
    | MultiplyInteger
    | DivideInteger
    | QuotientInteger
    | RemainderInteger
    | ModInteger
    | LessThanInteger
    | LessThanEqInteger
    | GreaterThanInteger
    | GreaterThanEqInteger
    | EqInteger
    | Concatenate
    | TakeByteString
    | DropByteString
    | SHA2
    | SHA3
    | VerifySignature
    | EqByteString
    | GtByteString
    | IfThenElse


type alias DynamicBuiltinName =
    String


type BuiltinName
    = StaticBuiltinName StaticBuiltinName
    | DynamicBuiltinName DynamicBuiltinName


type Term tyname name uni ann
    = Var ann name
    | TyAbs ann tyname (Kind ann) (Term tyname name uni ann)
    | LamAbs ann name (Type tyname uni ann) (Term tyname name uni ann)



{- FIXME: Not Complete -}
