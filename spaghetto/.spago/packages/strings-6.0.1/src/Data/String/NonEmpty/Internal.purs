-- | While most of the code in this module is safe, this module does
-- | export a few partial functions and the `NonEmptyString` constructor.
-- | While the partial functions are obvious from the `Partial` constraint in
-- | their type signature, the `NonEmptyString` constructor can be overlooked
-- | when searching for issues in one's code. See the constructor's
-- | documentation for more information.
module Data.String.NonEmpty.Internal where

import Prelude

import Data.Foldable (class Foldable)
import Data.Foldable as F
import Data.Maybe (Maybe(..), fromJust)
import Data.Semigroup.Foldable (class Foldable1)
import Data.String as String
import Data.String.Pattern (Pattern)
import Data.Symbol (class IsSymbol, reflectSymbol)
import Prim.TypeError as TE
import Type.Proxy (Proxy)
import Unsafe.Coerce (unsafeCoerce)

-- | A string that is known not to be empty.
-- |
-- | You can use this constructor to create a `NonEmptyString` that isn't
-- | non-empty, breaking the guarantee behind this newtype. It is
-- | provided as an escape hatch mainly for the `Data.NonEmpty.CodeUnits`
-- | and `Data.NonEmpty.CodePoints` modules. Use this at your own risk
-- | when you know what you are doing.
newtype NonEmptyString = NonEmptyString String

derive newtype instance eqNonEmptyString ∷ Eq NonEmptyString
derive newtype instance ordNonEmptyString ∷ Ord NonEmptyString
derive newtype instance semigroupNonEmptyString ∷ Semigroup NonEmptyString

instance showNonEmptyString :: Show NonEmptyString where
  show (NonEmptyString s) = "(NonEmptyString.unsafeFromString " <> show s <> ")"

-- | A helper class for defining non-empty string values at compile time.
-- |
-- | ``` purescript
-- | something :: NonEmptyString
-- | something = nes (Proxy :: Proxy "something")
-- | ```
class MakeNonEmpty (s :: Symbol) where
  nes :: Proxy s -> NonEmptyString

instance makeNonEmptyBad :: TE.Fail (TE.Text "Cannot create an NonEmptyString from an empty Symbol") => MakeNonEmpty "" where
  nes _ = NonEmptyString ""

else instance nonEmptyNonEmpty :: IsSymbol s => MakeNonEmpty s where
  nes p = NonEmptyString (reflectSymbol p)

-- | A newtype used in cases to specify a non-empty replacement for a pattern.
newtype NonEmptyReplacement = NonEmptyReplacement NonEmptyString

derive newtype instance eqNonEmptyReplacement :: Eq NonEmptyReplacement
derive newtype instance ordNonEmptyReplacement :: Ord NonEmptyReplacement
derive newtype instance semigroupNonEmptyReplacement ∷ Semigroup NonEmptyReplacement

instance showNonEmptyReplacement :: Show NonEmptyReplacement where
  show (NonEmptyReplacement s) = "(NonEmptyReplacement " <> show s <> ")"

-- | Creates a `NonEmptyString` from a `String`, returning `Nothing` if the
-- | input is empty.
-- |
-- | ```purescript
-- | fromString "" = Nothing
-- | fromString "hello" = Just (NES.unsafeFromString "hello")
-- | ```
fromString :: String -> Maybe NonEmptyString
fromString = case _ of
  "" -> Nothing
  s -> Just (NonEmptyString s)

-- | A partial version of `fromString`.
unsafeFromString :: Partial => String -> NonEmptyString
unsafeFromString = fromJust <<< fromString

-- | Converts a `NonEmptyString` back into a standard `String`.
toString :: NonEmptyString -> String
toString (NonEmptyString s) = s

-- | Appends a string to this non-empty string. Since one of the strings is
-- | non-empty we know the result will be too.
-- |
-- | ```purescript
-- | appendString (NonEmptyString "Hello") " world" == NonEmptyString "Hello world"
-- | appendString (NonEmptyString "Hello") "" == NonEmptyString "Hello"
-- | ```
appendString :: NonEmptyString -> String -> NonEmptyString
appendString (NonEmptyString s1) s2 = NonEmptyString (s1 <> s2)

-- | Prepends a string to this non-empty string. Since one of the strings is
-- | non-empty we know the result will be too.
-- |
-- | ```purescript
-- | prependString "be" (NonEmptyString "fore") == NonEmptyString "before"
-- | prependString "" (NonEmptyString "fore") == NonEmptyString "fore"
-- | ```
prependString :: String -> NonEmptyString -> NonEmptyString
prependString s1 (NonEmptyString s2) = NonEmptyString (s1 <> s2)

-- | If the string starts with the given prefix, return the portion of the
-- | string left after removing it. If the prefix does not match or there is no
-- | remainder, the result will be `Nothing`.
-- |
-- | ```purescript
-- | stripPrefix (Pattern "http:") (NonEmptyString "http://purescript.org") == Just (NonEmptyString "//purescript.org")
-- | stripPrefix (Pattern "http:") (NonEmptyString "https://purescript.org") == Nothing
-- | stripPrefix (Pattern "Hello!") (NonEmptyString "Hello!") == Nothing
-- | ```
stripPrefix :: Pattern -> NonEmptyString -> Maybe NonEmptyString
stripPrefix pat = fromString <=< liftS (String.stripPrefix pat)

-- | If the string ends with the given suffix, return the portion of the
-- | string left after removing it. If the suffix does not match or there is no
-- | remainder, the result will be `Nothing`.
-- |
-- | ```purescript
-- | stripSuffix (Pattern ".exe") (NonEmptyString "purs.exe") == Just (NonEmptyString "purs")
-- | stripSuffix (Pattern ".exe") (NonEmptyString "purs") == Nothing
-- | stripSuffix (Pattern "Hello!") (NonEmptyString "Hello!") == Nothing
-- | ```
stripSuffix :: Pattern -> NonEmptyString -> Maybe NonEmptyString
stripSuffix pat = fromString <=< liftS (String.stripSuffix pat)

-- | Checks whether the pattern appears in the given string.
-- |
-- | ```purescript
-- | contains (Pattern "needle") (NonEmptyString "haystack with needle") == true
-- | contains (Pattern "needle") (NonEmptyString "haystack") == false
-- | ```
contains :: Pattern -> NonEmptyString -> Boolean
contains = liftS <<< String.contains

-- | Compare two strings in a locale-aware fashion. This is in contrast to
-- | the `Ord` instance on `String` which treats strings as arrays of code
-- | units:
-- |
-- | ```purescript
-- | NonEmptyString "ä" `localeCompare` NonEmptyString "b" == LT
-- | NonEmptyString "ä" `compare` NonEmptyString "b" == GT
-- | ```
localeCompare :: NonEmptyString -> NonEmptyString -> Ordering
localeCompare (NonEmptyString a) (NonEmptyString b) = String.localeCompare a b

-- | Replaces the first occurence of the pattern with the replacement string.
-- |
-- | ```purescript
-- | replace (Pattern "<=") (NonEmptyReplacement "≤") (NonEmptyString "a <= b <= c") == NonEmptyString "a ≤ b <= c"
-- | ```
replace :: Pattern -> NonEmptyReplacement -> NonEmptyString -> NonEmptyString
replace pat (NonEmptyReplacement (NonEmptyString rep)) (NonEmptyString s) =
  NonEmptyString (String.replace pat (String.Replacement rep) s)

-- | Replaces all occurences of the pattern with the replacement string.
-- |
-- | ```purescript
-- | replaceAll (Pattern "<=") (NonEmptyReplacement "≤") (NonEmptyString "a <= b <= c") == NonEmptyString "a ≤ b ≤ c"
-- | ```
replaceAll :: Pattern -> NonEmptyReplacement -> NonEmptyString -> NonEmptyString
replaceAll pat (NonEmptyReplacement (NonEmptyString rep)) (NonEmptyString s) =
  NonEmptyString (String.replaceAll pat (String.Replacement rep) s)

-- | Returns the argument converted to lowercase.
-- |
-- | ```purescript
-- | toLower (NonEmptyString "hElLo") == NonEmptyString "hello"
-- | ```
toLower :: NonEmptyString -> NonEmptyString
toLower (NonEmptyString s) = NonEmptyString (String.toLower s)

-- | Returns the argument converted to uppercase.
-- |
-- | ```purescript
-- | toUpper (NonEmptyString "Hello") == NonEmptyString "HELLO"
-- | ```
toUpper :: NonEmptyString -> NonEmptyString
toUpper (NonEmptyString s) = NonEmptyString (String.toUpper s)

-- | Removes whitespace from the beginning and end of a string, including
-- | [whitespace characters](http://www.ecma-international.org/ecma-262/5.1/#sec-7.2)
-- | and [line terminators](http://www.ecma-international.org/ecma-262/5.1/#sec-7.3).
-- | If the string is entirely made up of whitespace the result will be Nothing.
-- |
-- | ```purescript
-- | trim (NonEmptyString "   Hello  \n World\n\t    ") == Just (NonEmptyString "Hello  \n World")
-- | trim (NonEmptyString "   \n") == Nothing
-- | ```
trim :: NonEmptyString -> Maybe NonEmptyString
trim (NonEmptyString s) = fromString (String.trim s)

-- | Joins the strings in a container together as a new string, inserting the
-- | first argument as separator between them. The result is not guaranteed to
-- | be non-empty.
-- |
-- | ```purescript
-- | joinWith ", " [NonEmptyString "apple", NonEmptyString "banana"] == "apple, banana"
-- | joinWith ", " [] == ""
-- | ```
joinWith :: forall f. Foldable f => String -> f NonEmptyString -> String
joinWith splice = F.intercalate splice <<< coe
  where
    coe :: f NonEmptyString -> f String
    coe = unsafeCoerce

-- | Joins non-empty strings in a non-empty container together as a new
-- | non-empty string, inserting a possibly empty string as separator between
-- | them. The result is guaranteed to be non-empty.
-- |
-- | ```purescript
-- | -- array syntax is used for demonstration here, it would need to be a real `Foldable1`
-- | join1With ", " [NonEmptyString "apple", NonEmptyString "banana"] == NonEmptyString "apple, banana"
-- | join1With "" [NonEmptyString "apple", NonEmptyString "banana"] == NonEmptyString "applebanana"
-- | ```
join1With :: forall f. Foldable1 f => String -> f NonEmptyString -> NonEmptyString
join1With splice = NonEmptyString <<< joinWith splice

-- | Joins possibly empty strings in a non-empty container together as a new
-- | non-empty string, inserting a non-empty string as a separator between them.
-- | The result is guaranteed to be non-empty.
-- |
-- | ```purescript
-- | -- array syntax is used for demonstration here, it would need to be a real `Foldable1`
-- | joinWith1 (NonEmptyString ", ") ["apple", "banana"] == NonEmptyString "apple, banana"
-- | joinWith1 (NonEmptyString "/") ["a", "b", "", "c", ""] == NonEmptyString "a/b//c/"
-- | ```
joinWith1 :: forall f. Foldable1 f => NonEmptyString -> f String -> NonEmptyString
joinWith1 (NonEmptyString splice) = NonEmptyString <<< F.intercalate splice

liftS :: forall r. (String -> r) -> NonEmptyString -> r
liftS f (NonEmptyString s) = f s
