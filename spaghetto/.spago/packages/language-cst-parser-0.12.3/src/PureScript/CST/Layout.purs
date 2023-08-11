module PureScript.CST.Layout
  ( LayoutStack
  , LayoutDelim(..)
  , currentIndent
  , isIndented
  , insertLayout
  , lytToken
  ) where

import Prelude

import Data.Array as Array
import Data.Foldable (find)
import Data.List (List(..), (:))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..), snd, uncurry)
import PureScript.CST.Types (SourcePos, SourceToken, Token(..))

type LayoutStack = List (Tuple SourcePos LayoutDelim)

data LayoutDelim
  = LytRoot
  | LytTopDecl
  | LytTopDeclHead
  | LytDeclGuard
  | LytCase
  | LytCaseBinders
  | LytCaseGuard
  | LytLambdaBinders
  | LytParen
  | LytBrace
  | LytSquare
  | LytIf
  | LytThen
  | LytProperty
  | LytForall
  | LytTick
  | LytLet
  | LytLetStmt
  | LytWhere
  | LytOf
  | LytDo
  | LytAdo

derive instance eqLayoutDelim :: Eq LayoutDelim
derive instance ordLayoutDelim :: Ord LayoutDelim

currentIndent :: LayoutStack -> Maybe SourcePos
currentIndent = go
  where
  go = case _ of
    Tuple pos lyt : stk
      | isIndented lyt -> Just pos
      | otherwise -> go stk
    _ ->
      Nothing

isIndented :: LayoutDelim -> Boolean
isIndented = case _ of
  LytLet -> true
  LytLetStmt -> true
  LytWhere -> true
  LytOf -> true
  LytDo -> true
  LytAdo -> true
  _ -> false

isTopDecl :: SourcePos -> LayoutStack -> Boolean
isTopDecl tokPos = case _ of
  Tuple lytPos LytWhere : Tuple _ LytRoot : Nil
    | tokPos.column == lytPos.column -> true
  _ -> false

lytToken :: SourcePos -> Token -> SourceToken
lytToken pos value =
  { range: { start: pos, end: pos }
  , leadingComments: []
  , trailingComments: []
  , value
  }

insertLayout :: SourceToken -> SourcePos -> LayoutStack -> Tuple LayoutStack (Array (Tuple SourceToken LayoutStack))
insertLayout src@{ range, value: tok } nextPos stack =
  insert (Tuple stack [])
  where
  tokPos = range.start

  insert state@(Tuple stk acc) = case tok of
    -- `data` declarations need masking (LytTopDecl) because the usage of `|`
    -- should not introduce a LytDeclGard context.
    TokLowerName Nothing "data" ->
      case state # insertDefault of
        state'@(Tuple stk' _) | isTopDecl tokPos stk' ->
          state' # pushStack tokPos LytTopDecl
        state' ->
          state' # popStack (_ == LytProperty)

    -- `class` declaration heads need masking (LytTopDeclHead) because the
    -- usage of commas in functional dependencies.
    TokLowerName Nothing "class" ->
      case state # insertDefault of
        state'@(Tuple stk' _) | isTopDecl tokPos stk' ->
          state' # pushStack tokPos LytTopDeclHead
        state' ->
          state' # popStack (_ == LytProperty)

    TokLowerName Nothing "where" ->
      case stk of
        Tuple _ LytTopDeclHead : stk' ->
          Tuple stk' acc # insertToken src # insertStart LytWhere
        Tuple _ LytProperty : stk' ->
          Tuple stk' acc # insertToken src
        _ ->
          state # collapse whereP # insertToken src # insertStart LytWhere
      where
      -- `where` always closes do blocks:
      --     example = do do do do foo where foo = ...
      --
      -- `where` closes layout contexts even when indented at the same level:
      --     example = case
      --       Foo -> ...
      --       Bar -> ...
      --       where foo = ...
      whereP _ LytDo = true
      whereP lytPos lyt = offsideEndP lytPos lyt

    TokLowerName Nothing "in" ->
      case collapse inP state of
        -- `let/in` is not allowed in `ado` syntax. `in` is treated as a
        -- delimiter and must always close the `ado`.
        --    example = ado
        --      foo <- ...
        --      let bar = ...
        --      in ...
        Tuple ((Tuple pos1 LytLetStmt) : (Tuple pos2 LytAdo) : stk') acc' ->
          Tuple stk' acc' # insertEnd pos1.column # insertEnd pos2.column # insertToken src
        Tuple (Tuple pos1 lyt : stk') acc' | isIndented lyt ->
          Tuple stk' acc' # insertEnd pos1.column # insertToken src
        _ ->
          state # insertDefault # popStack (_ == LytProperty)
      where
      inP _ LytLet = false
      inP _ LytAdo = false
      inP _ lyt = isIndented lyt

    TokLowerName Nothing "let" ->
      state # insertKwProperty next
      where
      next state'@(Tuple stk' _) = case stk' of
        Tuple p LytDo : _ | p.column == tokPos.column ->
          state' # insertStart LytLetStmt
        Tuple p LytAdo : _ | p.column == tokPos.column ->
          state' # insertStart LytLetStmt
        _ ->
          state' # insertStart LytLet

    TokLowerName _ "do" ->
      state # insertKwProperty (insertStart LytDo)

    TokLowerName _ "ado" ->
      state # insertKwProperty (insertStart LytAdo)

    -- `case` heads need masking due to commas.
    TokLowerName Nothing "case" ->
      state # insertKwProperty (pushStack tokPos LytCase)

    TokLowerName Nothing "of" ->
      case collapse indentedP state of
        -- When `of` is matched with a `case`, we are in a case block, and we
        -- need to mask additional contexts (LytCaseBinders, LytCaseGuards)
        -- due to commas.
        Tuple (Tuple _ LytCase : stk') acc' ->
          Tuple stk' acc' # insertToken src # insertStart LytOf # pushStack nextPos LytCaseBinders
        state' ->
          state' # insertDefault # popStack (_ == LytProperty)

    -- `if/then/else` is considered a delimiter context. This allows us to
    -- write chained expressions in `do` blocks without stair-stepping:
    --     example = do
    --       foo
    --       if ... then
    --         ...
    --       else if ... then
    --         ...
    --       else
    --         ...
    TokLowerName Nothing "if" ->
      state # insertKwProperty (pushStack tokPos LytIf)

    TokLowerName Nothing "then" ->
      case state # collapse indentedP of
        Tuple (Tuple _ LytIf : stk') acc' ->
          Tuple stk' acc' # insertToken src # pushStack tokPos LytThen
        _ ->
          state # insertDefault # popStack (_ == LytProperty)

    TokLowerName Nothing "else" ->
      case state # collapse indentedP of
        Tuple (Tuple _ LytThen : stk') acc' ->
          Tuple stk' acc' # insertToken src
        _ ->
          -- We don't want to insert a layout separator for top-level `else` in
          -- instance chains.
          case state # collapse offsideP of
            state'@(Tuple stk' _) | isTopDecl tokPos stk' ->
              state' # insertToken src
            state' ->
              state' # insertSep # insertToken src # popStack (_ == LytProperty)

    -- `forall` binders need masking because the usage of `.` should not
    -- introduce a LytProperty context.
    TokForall _ ->
      state # insertKwProperty (pushStack tokPos LytForall)

    -- Lambdas need masking because the usage of `->` should not close a
    -- LytDeclGuard or LytCaseGuard context.
    TokBackslash ->
      state # insertDefault # pushStack tokPos LytLambdaBinders

    TokRightArrow _ ->
      state # collapse arrowP # popStack guardP # insertToken src
      where
      arrowP _ LytDo = true
      arrowP _ LytOf = false
      arrowP lytPos lyt = offsideEndP lytPos lyt

      guardP LytCaseBinders = true
      guardP LytCaseGuard = true
      guardP LytLambdaBinders = true
      guardP _ = false

    TokEquals ->
      case state # collapse equalsP of
        Tuple (Tuple _ LytDeclGuard : stk') acc' ->
          Tuple stk' acc' # insertToken src
        _ ->
          state # insertDefault
      where
      equalsP _ LytWhere = true
      equalsP _ LytLet = true
      equalsP _ LytLetStmt = true
      equalsP _ _ = false

    -- Guards need masking because of commas.
    TokPipe -> do
      case collapse offsideEndP state of
        state'@(Tuple (Tuple _ LytOf : _) _) ->
          state' # pushStack tokPos LytCaseGuard # insertToken src
        state'@(Tuple (Tuple _ LytLet : _) _) ->
          state' # pushStack tokPos LytDeclGuard # insertToken src
        state'@(Tuple (Tuple _ LytLetStmt : _) _) ->
          state' # pushStack tokPos LytDeclGuard # insertToken src
        state'@(Tuple (Tuple _ LytWhere : _) _) ->
          state' # pushStack tokPos LytDeclGuard # insertToken src
        _ ->
          state # insertDefault

    -- Ticks can either start or end an infix expression. We preemptively
    -- collapse all indentation contexts in search of a starting delimiter,
    -- and backtrack if we don't find one.
    TokTick -> do
      case state # collapse indentedP of
        Tuple (Tuple _ LytTick : stk') acc' ->
          Tuple stk' acc' # insertToken src
        _ ->
          state # collapse offsideEndP # insertSep # insertToken src # pushStack tokPos LytTick

    -- In general, commas should close all indented contexts.
    --     example = [ do foo
    --                    bar, baz ]
    TokComma -> do
      case state # collapse indentedP of
        -- If we see a LytBrace, then we are in a record type or literal.
        -- Record labels need masking so we can use unquoted keywords as labels
        -- without accidentally littering layout delimiters.
        state'@(Tuple (Tuple _ LytBrace : _) _) ->
          state' # insertToken src # pushStack tokPos LytProperty
        state' ->
          state' # insertToken src

    -- TokDot tokens usually entail property access, which need masking so we
    -- can use unquoted keywords as labels.
    TokDot -> do
      case state # insertDefault of
        Tuple (Tuple _ LytForall : stk') acc' ->
          Tuple stk' acc'
        state' ->
          state' # pushStack tokPos LytProperty

    TokLeftParen ->
      state # insertDefault # pushStack tokPos LytParen

    TokLeftBrace ->
      state # insertDefault # pushStack tokPos LytBrace # pushStack tokPos LytProperty

    TokLeftSquare ->
      state # insertDefault # pushStack tokPos LytSquare

    TokRightParen ->
      state # collapse indentedP # popStack (_ == LytParen) # insertToken src

    TokRightBrace ->
      state # collapse indentedP # popStack (_ == LytProperty) # popStack (_ == LytBrace) # insertToken src

    TokRightSquare ->
      state # collapse indentedP # popStack (_ == LytSquare) # insertToken src

    TokString _ _ ->
      state # insertDefault # popStack (_ == LytProperty)

    TokLowerName Nothing _ ->
      state # insertDefault # popStack (_ == LytProperty)

    TokOperator _ _ ->
      state # collapse offsideEndP # insertSep # insertToken src

    _ ->
      state # insertDefault

  insertDefault state =
    state # collapse offsideP # insertSep # insertToken src

  insertStart lyt state@(Tuple stk _) =
    -- We only insert a new layout start when it's going to increase indentation.
    -- This prevents things like the following from parsing:
    --     instance foo :: Foo where
    --     foo = 42
    case find (isIndented <<< snd) stk of
      Just (Tuple pos _) | nextPos.column <= pos.column -> state
      _ -> state # pushStack nextPos lyt # insertToken (lytToken nextPos (TokLayoutStart nextPos.column))

  insertSep state@(Tuple stk acc) = case stk of
    -- LytTopDecl is closed by a separator.
    Tuple lytPos LytTopDecl : stk' | sepP lytPos ->
      Tuple stk' acc # insertToken sepTok
    -- LytTopDeclHead can be closed by a separator if there is no `where`.
    Tuple lytPos LytTopDeclHead : stk' | sepP lytPos ->
      Tuple stk' acc # insertToken sepTok
    Tuple lytPos lyt : _ | indentSepP lytPos lyt ->
      case lyt of
        -- If a separator is inserted in a case block, we need to push an
        -- additional LytCaseBinders context for comma masking.
        LytOf -> state # insertToken sepTok # pushStack tokPos LytCaseBinders
        _ -> state # insertToken sepTok
    _ ->
      state
    where
    sepTok = lytToken tokPos (TokLayoutSep tokPos.column)

  insertKwProperty k state =
    case state # insertDefault of
      Tuple (Tuple _ LytProperty : stk') acc' ->
        Tuple stk' acc'
      state' ->
        k state'

  insertEnd indent =
    insertToken (lytToken tokPos (TokLayoutEnd indent))

  insertToken token (Tuple stk acc) =
    Tuple stk (acc `Array.snoc` (Tuple token stk))

  pushStack lytPos lyt (Tuple stk acc) =
    Tuple (Tuple lytPos lyt : stk) acc

  popStack p (Tuple (Tuple _ lyt : stk') acc)
    | p lyt = Tuple stk' acc
  popStack _ state = state

  collapse p = uncurry go
    where
    go (Tuple lytPos lyt : stk') acc
      | p lytPos lyt =
          go stk'
            if isIndented lyt then acc `Array.snoc` (Tuple (lytToken tokPos (TokLayoutEnd lytPos.column)) stk')
            else acc
    go stk acc =
      Tuple stk acc

  indentedP =
    const isIndented

  offsideP lytPos lyt =
    isIndented lyt && tokPos.column < lytPos.column

  offsideEndP lytPos lyt =
    isIndented lyt && tokPos.column <= lytPos.column

  indentSepP lytPos lyt =
    isIndented lyt && sepP lytPos

  sepP lytPos =
    tokPos.column == lytPos.column && tokPos.line /= lytPos.line
