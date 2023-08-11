# PureScript Language CST Parser

A parser for the PureScript concrete syntax tree.

## Usage

The supported parsers are exported from `PureScript.CST`. The parsers support
some error recovery, which is reflected in the `RecoveredParserResult` type.
The CST types in `PureScript.CST.Types` are indexed by an error type, which
is fixed to `RecoveredError` in the case of failures. Parses that succeed
without failures have the error type fixed to `Void`.

```purescript
import PureScript.CST (RecoveredParserResult(..), parseModule)

example = case parseModule myModuleSource of
  ParseSucceeded cst ->
    -- `cst` is type `Module Void` to indicate no errors
  ParseSucceededWithErrors cst errors ->
    -- `cst is type `Module RecoveredError` and contains error nodes at points of failure.
  ParseFailed error ->
    -- An unrecoverable error was encountered.
```

## Traversals

`PureScript.CST.Traversal` contains traversals for analyzing and rewriting
the CST. These folds take a language visitor record with functions for
handling the primary types in the CST. Default records are provided that do
nothing for the cases you don't care about.

For example, if you wanted to quickly gather all the identifiers used in the
expressions of a module along with locations, you might use `foldMapModule`
and only provide a case for `onExpr`.

```purescript
import Prelude
import Data.Map (SemigroupMap(..))
import Data.Map as Map
import Data.Set (Set)
import Data.Set as Set
import Data.Tuple (Tuple(..))
import PureScript.CST.Traversal (foldMapModule, defaultMonoidalVisitor)
import PureScript.CST.Types as CST

type QualifiedIdent = Tuple (Maybe CST.ModuleName) CST.Ident
type UsageMap = SemigroupMap QualifiedIdent (Set CST.SourceRange)

getExprIdents :: forall a. CST.Module a -> UsageMap
getExprIdents = foldMapModule $ defaultMonoidalVisitor
  { onExpr = case _ of
      CST.ExprIdent (CST.QualifiedName ident) ->
        SemigroupMap
          $ Map.singleton (Tuple ident."module" ident.name)
          $ Set.singleton ident.token.range
      _ -> mempty
  }
```

## Development

The provided integration test attempts to parse a provided package set, and
will report any errors it encounters as well as listing the fastest and
slowest parse times along with the mean parse time for the set.

```sh
npm run parse-package-set
```

You can also benchmark or parse a single file:

```sh
npm run bench-file MyModule.purs
npm run parse-file -- MyModule.purs --tokens
```
