module Spago.Purs.EntryPoint
  ( hasMainExport
  , EntryPointCheckResult(..)
  ) where

import Prelude

import Data.Array as Array
import Data.Array.NonEmpty as NonEmptyArray
import Data.Maybe (Maybe(..))
import Data.Newtype (unwrap)
import Data.Tuple (snd)
import PureScript.CST (RecoveredParserResult(..), parseModule)
import PureScript.CST.Types as CST

data EntryPointCheckResult
  = MainExported
  | MainWrongType
  | MainNotDeclared
  | MainNotExported
  | ParseError String

-- | Check if the given PureScript source code declares and exports `main`
hasMainExport :: String -> EntryPointCheckResult
hasMainExport sourceCode = case parseModule sourceCode of
  ParseSucceeded mod -> checkModule mod
  ParseSucceededWithErrors mod _ -> checkModule mod
  ParseFailed _ -> ParseError "Failed to parse module"

checkModule :: forall e. CST.Module e -> EntryPointCheckResult
checkModule (CST.Module { header: CST.ModuleHeader { exports }, body: CST.ModuleBody { decls } }) =
  let
    hasMainDecl = Array.any isMainDecl decls
    hasMainSignature = Array.any isMainSignature decls
    hasCorrectType = Array.any isMainEffectUnit decls
    isExported = case exports of
      Nothing -> true -- No explicit exports = everything exported
      Just exportList -> Array.any isMainExport (separatedToArray (unwrap exportList).value)
  in
    if not hasMainDecl then MainNotDeclared
    else if not isExported then MainNotExported
    -- Only check type if there's a signature; if no signature, we can't verify the type
    else if hasMainSignature && not hasCorrectType then MainWrongType
    else MainExported

-- | Check for DeclValue or DeclSignature with name "main"
isMainDecl :: forall e. CST.Declaration e -> Boolean
isMainDecl = case _ of
  CST.DeclValue { name } -> getIdentName name == "main"
  CST.DeclSignature (CST.Labeled { label }) -> getIdentName label == "main"
  _ -> false

-- | Check if there's any type signature for "main"
isMainSignature :: forall e. CST.Declaration e -> Boolean
isMainSignature = case _ of
  CST.DeclSignature (CST.Labeled { label }) -> getIdentName label == "main"
  _ -> false

-- | Check for main :: Effect Unit (unqualified)
isMainEffectUnit :: forall e. CST.Declaration e -> Boolean
isMainEffectUnit = case _ of
  CST.DeclSignature
    ( CST.Labeled
        { label: CST.Name { name: CST.Ident "main" }
        , value: CST.TypeApp fn args
        }
    ) -> isEffectConstructor fn && isSingletonUnit args
  _ -> false
  where
  isEffectConstructor = case _ of
    CST.TypeConstructor (CST.QualifiedName { name: CST.Proper "Effect" }) -> true
    _ -> false
  isSingletonUnit args =
    NonEmptyArray.length args == 1 && isUnitConstructor (NonEmptyArray.head args)
  isUnitConstructor = case _ of
    CST.TypeConstructor (CST.QualifiedName { name: CST.Proper "Unit" }) -> true
    _ -> false

-- | Check if export is ExportValue with name "main"
isMainExport :: forall e. CST.Export e -> Boolean
isMainExport = case _ of
  CST.ExportValue name -> getIdentName name == "main"
  _ -> false

-- | Extract the string from a Name Ident
getIdentName :: CST.Name CST.Ident -> String
getIdentName (CST.Name { name: CST.Ident s }) = s

-- | Convert Separated to Array
separatedToArray :: forall a. CST.Separated a -> Array a
separatedToArray (CST.Separated { head, tail }) = Array.cons head (map snd tail)
