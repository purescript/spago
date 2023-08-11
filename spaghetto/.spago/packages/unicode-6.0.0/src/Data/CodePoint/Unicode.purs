module Data.CodePoint.Unicode
  ( -- Predicates
    isAscii
  , isAsciiLower
  , isAsciiUpper
  , isLatin1
  , isLower
  , isUpper
  , isAlpha
  , isAlphaNum
  , isLetter
  , isDigit -- Deprecated
  , isDecDigit
  , isOctDigit
  , isHexDigit
  , isControl
  , isPrint
  , isSpace
  , isSymbol
  , isSeparator
  , isPunctuation
  , isMark
  , isNumber

  , digitToInt -- Deprecated
  , hexDigitToInt
  , decDigitToInt
  , octDigitToInt

  -- Case conversion
  , toLower
  , toUpper
  , toTitle
  , caseFold
  , toLowerSimple
  , toUpperSimple
  , toTitleSimple
  , caseFoldSimple

  -- Unicode General Categories
  , GeneralCategory(..)
  , unicodeCatToGeneralCat
  , generalCatToInt
  , generalCatToUnicodeCat
  , generalCategory
  ) where

import Prelude

import Data.Char (toCharCode)
import Data.CodePoint.Unicode.Internal.Casing as Casing
import Data.CodePoint.Unicode.Internal (UnicodeCategory(..), uTowtitle, uTowlower, uTowupper, uIswalnum, uIswalpha, uIswlower, uIswupper, uIswspace, uIswprint, uIswcntrl, uGencat)
import Data.Enum (fromEnum)
import Data.Maybe (Maybe(..))
import Data.String.CodePoints (CodePoint, codePointFromChar)
import Prim.TypeError (class Warn, Text)
import Unsafe.Coerce (unsafeCoerce)

-- | Unicode General Categories (column 2 of the UnicodeData table) in
-- | the order they are listed in the Unicode standard (the Unicode
-- | Character Database, in particular).
-- |
-- | *Examples*
-- |
-- | Basic usage:
-- |
-- | ```
-- | >>> :t OtherLetter
-- | OtherLetter :: GeneralCategory
-- | ```
-- |
-- | `Eq` instance:
-- |
-- | ```
-- | >>> UppercaseLetter == UppercaseLetter
-- | true
-- | >>> UppercaseLetter == LowercaseLetter
-- | false
-- | ```
-- |
-- | `Ord` instance:
-- |
-- | ```
-- | >>> NonSpacingMark <= MathSymbol
-- | true
-- | ```
-- |
-- | `Enum` instance (TODO: this is not implemented yet):
-- |
-- | ```
-- | >>> enumFromTo ModifierLetter SpacingCombiningMark
-- | [ModifierLetter,OtherLetter,NonSpacingMark,SpacingCombiningMark]
-- | ```
-- |
-- | `Show` instance:
-- |
-- | ```
-- | >>> show EnclosingMark
-- | "EnclosingMark"
-- | ```
-- |
-- | `Bounded` instance:
-- |
-- | ```
-- | >>> bottom :: GeneralCategory
-- | UppercaseLetter
-- | >>> top :: GeneralCategory
-- | NotAssigned
-- | ```
data GeneralCategory
  = UppercaseLetter -- ^ Lu: Letter, Uppercase
  | LowercaseLetter -- ^ Ll: Letter, Lowercase
  | TitlecaseLetter -- ^ Lt: Letter, Titlecase
  | ModifierLetter -- ^ Lm: Letter, Modifier
  | OtherLetter -- ^ Lo: Letter, Other
  | NonSpacingMark -- ^ Mn: Mark, Non-Spacing
  | SpacingCombiningMark -- ^ Mc: Mark, Spacing Combining
  | EnclosingMark -- ^ Me: Mark, Enclosing
  | DecimalNumber -- ^ Nd: Number, Decimal
  | LetterNumber -- ^ Nl: Number, Letter
  | OtherNumber -- ^ No: Number, Other
  | ConnectorPunctuation -- ^ Pc: Punctuation, Connector
  | DashPunctuation -- ^ Pd: Punctuation, Dash
  | OpenPunctuation -- ^ Ps: Punctuation, Open
  | ClosePunctuation -- ^ Pe: Punctuation, Close
  | InitialQuote -- ^ Pi: Punctuation, Initial quote
  | FinalQuote -- ^ Pf: Punctuation, Final quote
  | OtherPunctuation -- ^ Po: Punctuation, Other
  | MathSymbol -- ^ Sm: Symbol, Math
  | CurrencySymbol -- ^ Sc: Symbol, Currency
  | ModifierSymbol -- ^ Sk: Symbol, Modifier
  | OtherSymbol -- ^ So: Symbol, Other
  | Space -- ^ Zs: Separator, Space
  | LineSeparator -- ^ Zl: Separator, Line
  | ParagraphSeparator -- ^ Zp: Separator, Paragraph
  | Control -- ^ Cc: Other, Control
  | Format -- ^ Cf: Other, Format
  | Surrogate -- ^ Cs: Other, Surrogate
  | PrivateUse -- ^ Co: Other, Private Use
  | NotAssigned -- ^ Cn: Other, Not Assigned

generalCatToInt :: GeneralCategory -> Int
generalCatToInt UppercaseLetter = 1
generalCatToInt LowercaseLetter = 2
generalCatToInt TitlecaseLetter = 3
generalCatToInt ModifierLetter = 4
generalCatToInt OtherLetter = 5
generalCatToInt NonSpacingMark = 6
generalCatToInt SpacingCombiningMark = 7
generalCatToInt EnclosingMark = 8
generalCatToInt DecimalNumber = 9
generalCatToInt LetterNumber = 10
generalCatToInt OtherNumber = 11
generalCatToInt ConnectorPunctuation = 12
generalCatToInt DashPunctuation = 13
generalCatToInt OpenPunctuation = 14
generalCatToInt ClosePunctuation = 15
generalCatToInt InitialQuote = 16
generalCatToInt FinalQuote = 17
generalCatToInt OtherPunctuation = 18
generalCatToInt MathSymbol = 19
generalCatToInt CurrencySymbol = 20
generalCatToInt ModifierSymbol = 21
generalCatToInt OtherSymbol = 22
generalCatToInt Space = 23
generalCatToInt LineSeparator = 24
generalCatToInt ParagraphSeparator = 25
generalCatToInt Control = 26
generalCatToInt Format = 27
generalCatToInt Surrogate = 28
generalCatToInt PrivateUse = 29
generalCatToInt NotAssigned = 30

generalCatToUnicodeCat :: GeneralCategory -> UnicodeCategory
generalCatToUnicodeCat UppercaseLetter = NUMCAT_LU
generalCatToUnicodeCat LowercaseLetter = NUMCAT_LL
generalCatToUnicodeCat TitlecaseLetter = NUMCAT_LT
generalCatToUnicodeCat ModifierLetter = NUMCAT_LM
generalCatToUnicodeCat OtherLetter = NUMCAT_LO
generalCatToUnicodeCat NonSpacingMark = NUMCAT_MN
generalCatToUnicodeCat SpacingCombiningMark = NUMCAT_MC
generalCatToUnicodeCat EnclosingMark = NUMCAT_ME
generalCatToUnicodeCat DecimalNumber = NUMCAT_ND
generalCatToUnicodeCat LetterNumber = NUMCAT_NL
generalCatToUnicodeCat OtherNumber = NUMCAT_NO
generalCatToUnicodeCat ConnectorPunctuation = NUMCAT_PC
generalCatToUnicodeCat DashPunctuation = NUMCAT_PD
generalCatToUnicodeCat OpenPunctuation = NUMCAT_PS
generalCatToUnicodeCat ClosePunctuation = NUMCAT_PE
generalCatToUnicodeCat InitialQuote = NUMCAT_PI
generalCatToUnicodeCat FinalQuote = NUMCAT_PF
generalCatToUnicodeCat OtherPunctuation = NUMCAT_PO
generalCatToUnicodeCat MathSymbol = NUMCAT_SM
generalCatToUnicodeCat CurrencySymbol = NUMCAT_SC
generalCatToUnicodeCat ModifierSymbol = NUMCAT_SK
generalCatToUnicodeCat OtherSymbol = NUMCAT_SO
generalCatToUnicodeCat Space = NUMCAT_ZS
generalCatToUnicodeCat LineSeparator = NUMCAT_ZL
generalCatToUnicodeCat ParagraphSeparator = NUMCAT_ZP
generalCatToUnicodeCat Control = NUMCAT_CC
generalCatToUnicodeCat Format = NUMCAT_CF
generalCatToUnicodeCat Surrogate = NUMCAT_CS
generalCatToUnicodeCat PrivateUse = NUMCAT_CO
generalCatToUnicodeCat NotAssigned = NUMCAT_CN

unicodeCatToGeneralCat :: UnicodeCategory -> GeneralCategory
unicodeCatToGeneralCat NUMCAT_LU = UppercaseLetter
unicodeCatToGeneralCat NUMCAT_LL = LowercaseLetter
unicodeCatToGeneralCat NUMCAT_LT = TitlecaseLetter
unicodeCatToGeneralCat NUMCAT_LM = ModifierLetter
unicodeCatToGeneralCat NUMCAT_LO = OtherLetter
unicodeCatToGeneralCat NUMCAT_MN = NonSpacingMark
unicodeCatToGeneralCat NUMCAT_MC = SpacingCombiningMark
unicodeCatToGeneralCat NUMCAT_ME = EnclosingMark
unicodeCatToGeneralCat NUMCAT_ND = DecimalNumber
unicodeCatToGeneralCat NUMCAT_NL = LetterNumber
unicodeCatToGeneralCat NUMCAT_NO = OtherNumber
unicodeCatToGeneralCat NUMCAT_PC = ConnectorPunctuation
unicodeCatToGeneralCat NUMCAT_PD = DashPunctuation
unicodeCatToGeneralCat NUMCAT_PS = OpenPunctuation
unicodeCatToGeneralCat NUMCAT_PE = ClosePunctuation
unicodeCatToGeneralCat NUMCAT_PI = InitialQuote
unicodeCatToGeneralCat NUMCAT_PF = FinalQuote
unicodeCatToGeneralCat NUMCAT_PO = OtherPunctuation
unicodeCatToGeneralCat NUMCAT_SM = MathSymbol
unicodeCatToGeneralCat NUMCAT_SC = CurrencySymbol
unicodeCatToGeneralCat NUMCAT_SK = ModifierSymbol
unicodeCatToGeneralCat NUMCAT_SO = OtherSymbol
unicodeCatToGeneralCat NUMCAT_ZS = Space
unicodeCatToGeneralCat NUMCAT_ZL = LineSeparator
unicodeCatToGeneralCat NUMCAT_ZP = ParagraphSeparator
unicodeCatToGeneralCat NUMCAT_CC = Control
unicodeCatToGeneralCat NUMCAT_CF = Format
unicodeCatToGeneralCat NUMCAT_CS = Surrogate
unicodeCatToGeneralCat NUMCAT_CO = PrivateUse
unicodeCatToGeneralCat NUMCAT_CN = NotAssigned

instance showGeneralCategory :: Show GeneralCategory where
  show UppercaseLetter = "UppercaseLetter"
  show LowercaseLetter = "LowercaseLetter"
  show TitlecaseLetter = "TitlecaseLetter"
  show ModifierLetter = "ModifierLetter"
  show OtherLetter = "OtherLetter"
  show NonSpacingMark = "NonSpacingMark"
  show SpacingCombiningMark = "SpacingCombiningMark"
  show EnclosingMark = "EnclosingMark"
  show DecimalNumber = "DecimalNumber"
  show LetterNumber = "LetterNumber"
  show OtherNumber = "OtherNumber"
  show ConnectorPunctuation = "ConnectorPunctuation"
  show DashPunctuation = "DashPunctuation"
  show OpenPunctuation = "OpenPunctuation"
  show ClosePunctuation = "ClosePunctuation"
  show InitialQuote = "InitialQuote"
  show FinalQuote = "FinalQuote"
  show OtherPunctuation = "OtherPunctuation"
  show MathSymbol = "MathSymbol"
  show CurrencySymbol = "CurrencySymbol"
  show ModifierSymbol = "ModifierSymbol"
  show OtherSymbol = "OtherSymbol"
  show Space = "Space"
  show LineSeparator = "LineSeparator"
  show ParagraphSeparator = "ParagraphSeparator"
  show Control = "Control"
  show Format = "Format"
  show Surrogate = "Surrogate"
  show PrivateUse = "PrivateUse"
  show NotAssigned = "NotAssigned"

instance eqGeneralCategory :: Eq GeneralCategory where
  eq UppercaseLetter UppercaseLetter = true
  eq LowercaseLetter LowercaseLetter = true
  eq TitlecaseLetter TitlecaseLetter = true
  eq ModifierLetter ModifierLetter = true
  eq OtherLetter OtherLetter = true
  eq NonSpacingMark NonSpacingMark = true
  eq SpacingCombiningMark SpacingCombiningMark = true
  eq EnclosingMark EnclosingMark = true
  eq DecimalNumber DecimalNumber = true
  eq LetterNumber LetterNumber = true
  eq OtherNumber OtherNumber = true
  eq ConnectorPunctuation ConnectorPunctuation = true
  eq DashPunctuation DashPunctuation = true
  eq OpenPunctuation OpenPunctuation = true
  eq ClosePunctuation ClosePunctuation = true
  eq InitialQuote InitialQuote = true
  eq FinalQuote FinalQuote = true
  eq OtherPunctuation OtherPunctuation = true
  eq MathSymbol MathSymbol = true
  eq CurrencySymbol CurrencySymbol = true
  eq ModifierSymbol ModifierSymbol = true
  eq OtherSymbol OtherSymbol = true
  eq Space Space = true
  eq LineSeparator LineSeparator = true
  eq ParagraphSeparator ParagraphSeparator = true
  eq Control Control = true
  eq Format Format = true
  eq Surrogate Surrogate = true
  eq PrivateUse PrivateUse = true
  eq NotAssigned NotAssigned = true
  eq _ _ = false

instance ordGeneralCategory :: Ord GeneralCategory where
  compare catA catB = compare (generalCatToInt catA) (generalCatToInt catB)

instance boundedGeneralCategory :: Bounded GeneralCategory where
  bottom = UppercaseLetter
  top = NotAssigned

-- | The Unicode general category of the character.
-- |
-- | *Examples*
-- |
-- | Basic usage:
-- |
-- | ```
-- | >>> generalCategory (codePointFromChar 'a')
-- | Just LowercaseLetter
-- | >>> generalCategory (codePointFromChar 'A')
-- | Just UppercaseLetter
-- | >>> generalCategory (codePointFromChar '0')
-- | Just DecimalNumber
-- | >>> generalCategory (codePointFromChar '%')
-- | Just OtherPunctuation
-- | >>> generalCategory (codePointFromChar '♥')
-- | Just OtherSymbol
-- | >>> generalCategory (codePointFromChar '\31')
-- | Just Control
-- | >>> generalCategory (codePointFromChar ' ')
-- | Just Space
-- | ```
generalCategory :: CodePoint -> Maybe GeneralCategory
generalCategory = map unicodeCatToGeneralCat <<< uGencat <<< fromEnum

-- | Selects the first 128 characters of the Unicode character set,
-- | corresponding to the ASCII character set.
isAscii :: CodePoint -> Boolean
isAscii c = c < codePointFromChar '\x80'

-- | Selects the first 256 characters of the Unicode character set,
-- | corresponding to the ISO 8859-1 (Latin-1) character set.
isLatin1 :: CodePoint -> Boolean
isLatin1 c = c <= codePointFromChar '\xff'

-- | Selects ASCII lower-case letters,
-- | i.e. characters satisfying both `isAscii` and `isLower`.
isAsciiLower :: CodePoint -> Boolean
isAsciiLower c = c >= codePointFromChar 'a' && c <= codePointFromChar 'z'

-- | Selects ASCII upper-case letters,
-- | i.e. characters satisfying both `isAscii` and `isUpper`.
isAsciiUpper :: CodePoint -> Boolean
isAsciiUpper c = c >= codePointFromChar 'A' && c <= codePointFromChar 'Z'

-- | Selects control characters, which are the non-printing characters of
-- | the Latin-1 subset of Unicode.
isControl :: CodePoint -> Boolean
isControl = uIswcntrl <<< fromEnum

-- | Selects printable Unicode characters
-- | (letters, numbers, marks, punctuation, symbols and spaces).
isPrint :: CodePoint -> Boolean
isPrint = uIswprint <<< fromEnum

-- | Returns `true` for any Unicode space character, and the control
-- | characters `\t`, `\n`, `\r`, `\f`, `\v`.
-- |
-- | `isSpace` includes non-breaking space.
isSpace :: CodePoint -> Boolean
-- The magic 0x377 used in the code below isn't really that magical. As of
-- 2014, all the codepoints at or below 0x377 have been assigned, so we
-- shouldn't have to worry about any new spaces appearing below there.
isSpace c =
  if uc <= 0x337 then uc == 32 || (uc >= 9 && uc <= 13) || uc == 0xa0
  else uIswspace uc
  where
  uc :: Int
  uc = fromEnum c

-- | Selects upper-case or title-case alphabetic Unicode characters (letters).
-- | Title case is used by a small number of letter ligatures like the
-- | single-character form of /Lj/.
isUpper :: CodePoint -> Boolean
isUpper = uIswupper <<< fromEnum

-- | Selects lower-case alphabetic Unicode characters (letters).
isLower :: CodePoint -> Boolean
isLower = uIswlower <<< fromEnum

-- | Selects alphabetic Unicode characters (lower-case, upper-case and
-- | title-case letters, plus letters of caseless scripts and modifiers letters).
isAlpha :: CodePoint -> Boolean
isAlpha = uIswalpha <<< fromEnum

-- | Selects alphabetic or numeric digit Unicode characters.
-- |
-- | Note that numeric digits outside the ASCII range are selected by this
-- | function but not by `isDigit`.  Such digits may be part of identifiers
-- | but are not used by the printer and reader to represent numbers.
isAlphaNum :: CodePoint -> Boolean
isAlphaNum = uIswalnum <<< fromEnum

-- | Selects ASCII decimal digits, i.e. `0..9`.
isDecDigit :: CodePoint -> Boolean
isDecDigit c =
  let
    diff = (fromEnum c - toCharCode '0')
  in
    diff <= 9 && diff >= 0

-- | Selects ASCII octal digits, i.e. `0..7`.
isOctDigit :: CodePoint -> Boolean
isOctDigit c =
  let
    diff = (fromEnum c - toCharCode '0')
  in
    diff <= 7 && diff >= 0

-- | Selects ASCII hexadecimal digits,
-- | i.e. `0..9, A..F, a..f`.
isHexDigit :: CodePoint -> Boolean
isHexDigit c = isDecDigit c
  || (let diff = (fromEnum c - toCharCode 'A') in diff <= 5 && diff >= 0)
  || (let diff = (fromEnum c - toCharCode 'a') in diff <= 5 && diff >= 0)

isDigit :: Warn (Text "'isDigit' is deprecated, use 'isDecDigit', 'isHexDigit', or 'isOctDigit' instead") => CodePoint -> Boolean
isDigit = isDecDigit

-- | Selects Unicode punctuation characters, including various kinds
-- | of connectors, brackets and quotes.
-- |
-- | This function returns `true` if its argument has one of the
-- | following `GeneralCategory`s, or `false` otherwise:
-- |
-- | - `ConnectorPunctuation`
-- | - `DashPunctuation`
-- | - `OpenPunctuation`
-- | - `ClosePunctuation`
-- | - `InitialQuote`
-- | - `FinalQuote`
-- | - `OtherPunctuation`
-- |
-- | These classes are defined in the
-- | [Unicode Character Database])http://www.unicode.org/reports/tr44/tr44-14.html#GC_Values_Table)
-- | part of the Unicode standard. The same document defines what is
-- | and is not a "Punctuation".
-- |
-- | *Examples*
-- |
-- | Basic usage:
-- |
-- | ```
-- | >>> isPunctuation (codePointFromChar 'a')
-- | false
-- | >>> isPunctuation (codePointFromChar '7')
-- | false
-- | >>> isPunctuation (codePointFromChar '♥')
-- | false
-- | >>> isPunctuation (codePointFromChar '"')
-- | true
-- | >>> isPunctuation (codePointFromChar '?')
-- | true
-- | >>> isPunctuation (codePointFromChar '—')
-- | true
-- | ```
isPunctuation :: CodePoint -> Boolean
isPunctuation c =
  case generalCategory c of
    Just ConnectorPunctuation -> true
    Just DashPunctuation -> true
    Just OpenPunctuation -> true
    Just ClosePunctuation -> true
    Just InitialQuote -> true
    Just FinalQuote -> true
    Just OtherPunctuation -> true
    _ -> false

-- | Selects Unicode symbol characters, including mathematical and
-- | currency symbols.
-- |
-- | This function returns `true` if its argument has one of the
-- | following `GeneralCategory`s, or `false` otherwise:
-- |
-- | - `MathSymbol`
-- | - `CurrencySymbol`
-- | - `ModifierSymbol`
-- | - `OtherSymbol`
-- |
-- | These classes are defined in the
-- | [Unicode Character Database](http://www.unicode.org/reports/tr44/tr44-14.html#GC_Values_Table),
-- | part of the Unicode standard. The same document defines what is
-- | and is not a "Symbol".
-- |
-- | *Examples*
-- |
-- | Basic usage:
-- |
-- | ```
-- | >>> isSymbol (codePointFromChar 'a')
-- | false
-- | >>> isSymbol (codePointFromChar '6')
-- | false
-- | >>> isSymbol (codePointFromChar '=')
-- | true
-- | ```
-- |
-- | The definition of \"math symbol\" may be a little
-- | counter-intuitive depending on one's background:
-- |
-- | ```
-- | >>> isSymbol (codePointFromChar '+')
-- | true
-- | >>> isSymbol (codePointFromChar '-')
-- | false
-- | ```
isSymbol :: CodePoint -> Boolean
isSymbol c =
  case generalCategory c of
    Just MathSymbol -> true
    Just CurrencySymbol -> true
    Just ModifierSymbol -> true
    Just OtherSymbol -> true
    _ -> false

-- | Convert a code point to the corresponding upper-case sequence of code points.
-- | Any other character is returned unchanged.
toUpper :: CodePoint -> Array CodePoint
toUpper = modifyFull Casing.upper

-- | Convert a code point to the corresponding lower-case sequence of code points.
-- | Any other character is returned unchanged.
toLower :: CodePoint -> Array CodePoint
toLower = modifyFull Casing.lower

-- | Convert a code point to the corresponding title-case or upper-case
-- | sequence of code points.  (Title case differs from upper case only for a
-- | small number of ligature characters.)
-- | Any other character is returned unchanged.
toTitle :: CodePoint -> Array CodePoint
toTitle = modifyFull Casing.title

-- | Convert a code point to the corresponding case-folded sequence of code
-- | points, for implementing caseless matching.
-- | Any other character is returned unchanged.
caseFold :: CodePoint -> Array CodePoint
caseFold = modifyFull Casing.foldFull

-- | Convert a code point to the corresponding upper-case code point, if any.
-- | Any other character is returned unchanged.
toUpperSimple :: CodePoint -> CodePoint
toUpperSimple = modify uTowupper

-- | Convert a code point to the corresponding lower-case code point, if any.
-- | Any other character is returned unchanged.
toLowerSimple :: CodePoint -> CodePoint
toLowerSimple = modify uTowlower

-- | Convert a code point to the corresponding title-case or upper-case
-- | code point, if any.  (Title case differs from upper case only for a small
-- | number of ligature characters.)
-- | Any other character is returned unchanged.
toTitleSimple :: CodePoint -> CodePoint
toTitleSimple = modify uTowtitle

-- | Convert a code point to the corresponding case-folded code point.
-- | Any other character is returned unchanged.
caseFoldSimple :: CodePoint -> CodePoint
caseFoldSimple = modify Casing.fold

-- | Convert a single digit `Char` to the corresponding `Just Int` if its argument
-- | satisfies `isHexDigit` (one of `0..9, A..F, a..f`). Anything else converts to `Nothing`
-- |
-- | ```
-- | >>> import Data.Traversable
-- |
-- | >>> traverse (hexDigitToInt <<< codePointFromChar) ['0','1','2','3','4','5','6','7','8','9']
-- | (Just [0,1,2,3,4,5,6,7,8,9])
-- |
-- | >>> traverse (hexDigitToInt <<< codePointFromChar) ['a','b','c','d','e','f']
-- | (Just [10,11,12,13,14,15])
-- |
-- | >>> traverse (hexDigitToInt <<< codePointFromChar) ['A','B','C','D','E','F']
-- | (Just [10,11,12,13,14,15])
-- |
-- | >>> hexDigitToInt (codePointFromChar 'G')
-- | Nothing
-- | ```
hexDigitToInt :: CodePoint -> Maybe Int
hexDigitToInt c = result
  where
  result :: Maybe Int
  result
    | dec <= 9 && dec >= 0 = Just dec
    | hexLower <= 5 && hexLower >= 0 = Just $ hexLower + 10
    | hexUpper <= 5 && hexUpper >= 0 = Just $ hexUpper + 10
    | otherwise = Nothing

  dec :: Int
  dec = fromEnum c - toCharCode '0'

  hexLower :: Int
  hexLower = fromEnum c - toCharCode 'a'

  hexUpper :: Int
  hexUpper = fromEnum c - toCharCode 'A'

-- | Convert a single digit `Char` to the corresponding `Just Int` if its argument
-- | satisfies `isDecDigit` (one of `0..9`). Anything else converts to `Nothing`
-- |
-- | ```
-- | >>> import Data.Traversable
-- |
-- | >>> traverse decDigitToInt ['0','1','2','3','4','5','6','7','8','9']
-- | (Just [0,1,2,3,4,5,6,7,8,9])
-- |
-- | >>> decDigitToInt 'a'
-- | Nothing
-- | ```
decDigitToInt :: CodePoint -> Maybe Int
decDigitToInt c
  | isDecDigit c = Just $ fromEnum c - toCharCode '0'
  | otherwise = Nothing

-- | Convert a single digit `Char` to the corresponding `Just Int` if its argument
-- | satisfies `isOctDigit` (one of `0..7`). Anything else converts to `Nothing`
-- |
-- | ```
-- | >>> import Data.Traversable
-- |
-- | >>> traverse octDigitToInt ['0','1','2','3','4','5','6','7']
-- | (Just [0,1,2,3,4,5,6,7])
-- |
-- | >>> octDigitToInt '8'
-- | Nothing
-- | ```
octDigitToInt :: CodePoint -> Maybe Int
octDigitToInt c
  | isOctDigit c = Just $ fromEnum c - toCharCode '0'
  | otherwise = Nothing

digitToInt :: Warn (Text "'digitToInt' is deprecated, use 'decDigitToInt', 'hexDigitToInt', or 'octDigitToInt' instead") => CodePoint -> Maybe Int
digitToInt = hexDigitToInt

-- | Selects alphabetic Unicode characters (lower-case, upper-case and
-- | title-case letters, plus letters of caseless scripts and
-- | modifiers letters).
-- |
-- | This function returns `true` if its argument has one of the
-- | following `GeneralCategory`s, or `false` otherwise:
-- |
-- | - `UppercaseLetter`
-- | - `LowercaseLetter`
-- | - `TitlecaseLetter`
-- | - `ModifierLetter`
-- | - `OtherLetter`
-- |
-- | These classes are defined in the
-- | [Unicode Character Database](http://www.unicode.org/reports/tr44/tr44-14.html#GC_Values_Table)
-- | part of the Unicode standard. The same document defines what is
-- | and is not a "Letter".
-- |
-- | *Examples*
-- |
-- | Basic usage:
-- |
-- | ```
-- | >>> isLetter (codePointFromChar 'a')
-- | true
-- | >>> isLetter (codePointFromChar 'A')
-- | true
-- | >>> isLetter (codePointFromChar '0')
-- | false
-- | >>> isLetter (codePointFromChar '%')
-- | false
-- | >>> isLetter (codePointFromChar '♥')
-- | false
-- | >>> isLetter (codePointFromChar '\x1F')
-- | false
-- | ```
-- |
-- | Ensure that 'isLetter' and 'isAlpha' are equivalent.
-- |
-- | ```
-- | >>> chars = enumFromTo bottom top :: Array CodePoint
-- | >>> letters = map isLetter chars
-- | >>> alphas = map isAlpha chars
-- | >>> letters == alphas
-- | true
-- | ```
isLetter :: CodePoint -> Boolean
isLetter c =
  case generalCategory c of
    Just UppercaseLetter -> true
    Just LowercaseLetter -> true
    Just TitlecaseLetter -> true
    Just ModifierLetter -> true
    Just OtherLetter -> true
    _ -> false

-- | Selects Unicode mark characters, for example accents and the
-- | like, which combine with preceding characters.
-- |
-- | This function returns `true` if its argument has one of the
-- | following `GeneralCategory`s, or `false` otherwise:
-- |
-- | - `NonSpacingMark`
-- | - `SpacingCombiningMark`
-- | - `EnclosingMark`
-- |
-- | These classes are defined in the
-- | [Unicode Character Database](http://www.unicode.org/reports/tr44/tr44-14.html#GC_Values_Table),
-- | part of the Unicode standard. The same document defines what is
-- | and is not a "Mark".
-- |
-- | *Examples*
-- |
-- | Basic usage:
-- |
-- | ```
-- | >>> isMark (codePointFromChar 'a')
-- | false
-- | >>> isMark (codePointFromChar '0')
-- | false
-- | ```
-- |
-- | Combining marks such as accent characters usually need to follow
-- | another character before they become printable:
-- |
-- | ```
-- | >>> map isMark (toCodePointArray "ò")
-- | [false,true]
-- | ```
-- |
-- | Puns are not necessarily supported:
-- |
-- | ```
-- | >>> isMark (codePointFromChar '✓')
-- | false
-- | ```
isMark :: CodePoint -> Boolean
isMark c =
  case generalCategory c of
    Just NonSpacingMark -> true
    Just SpacingCombiningMark -> true
    Just EnclosingMark -> true
    _ -> false

-- | Selects Unicode numeric characters, including digits from various
-- | scripts, Roman numerals, et cetera.
-- |
-- | This function returns `true` if its argument has one of the
-- | following `GeneralCategory`s, or `false` otherwise:
-- |
-- | * `DecimalNumber`
-- | * `LetterNumber`
-- | * `OtherNumber`
-- |
-- | These classes are defined in the
-- | [Unicode Character Database](http://www.unicode.org/reports/tr44/tr44-14.html#GC_Values_Table),
-- | part of the Unicode standard. The same document defines what is
-- | and is not a "Number".
-- |
-- | *Examples*
-- |
-- | Basic usage:
-- |
-- | ```
-- | >>> isNumber (codePointFromChar 'a')
-- | false
-- | >>> isNumber (codePointFromChar '%')
-- | false
-- | >>> isNumber (codePointFromChar '3')
-- | true
-- | ```
-- |
-- | ASCII @\'0\'@ through @\'9\'@ are all numbers:
-- |
-- | ```
-- | >>> and $ map (isNumber <<< codePointFromChar) (enumFromTo '0' '9' :: Array Char)
-- | true
-- | ```
-- |
-- | Unicode Roman numerals are \"numbers\" as well:
-- |
-- | ```
-- | >>> isNumber (codePointFromChar 'Ⅸ')
-- | true
-- | ```
isNumber :: CodePoint -> Boolean
isNumber c =
  case generalCategory c of
    Just DecimalNumber -> true
    Just LetterNumber -> true
    Just OtherNumber -> true
    _ -> false

-- | Selects Unicode space and separator characters.
-- |
-- | This function returns `true` if its argument has one of the
-- | following `GeneralCategory`s, or `false` otherwise:
-- |
-- | - `Space`
-- | - `LineSeparator`
-- | - `ParagraphSeparator`
-- |
-- | These classes are defined in the
-- | [Unicode Character Database](http://www.unicode.org/reports/tr44/tr44-14.html#GC_Values_Table)
-- | part of the Unicode standard. The same document defines what is
-- | and is not a "Separator".
-- |
-- | *Examples*
-- |
-- | Basic usage:
-- |
-- | ```
-- | >>> isSeparator (codePointFromChar 'a')
-- | false
-- | >>> isSeparator (codePointFromChar '6')
-- | false
-- | >>> isSeparator (codePointFromChar ' ')
-- | true
-- | >>> isSeparator (codePointFromChar '-')
-- | false
-- | ```
-- |
-- | Warning: newlines and tab characters are not considered
-- | separators.
-- |
-- | ```
-- | >>> isSeparator (codePointFromChar '\n')
-- | false
-- | >>> isSeparator (codePointFromChar '\t')
-- | false
-- | ```
-- |
-- | But some more exotic characters are (like HTML's @&nbsp;@):
-- |
-- | ```
-- | >>> isSeparator (codePointFromChar '\xA0')
-- | true
-- | ```
isSeparator :: CodePoint -> Boolean
isSeparator c =
  case generalCategory c of
    Just Space -> true
    Just LineSeparator -> true
    Just ParagraphSeparator -> true
    _ -> false

-- Helper functions
modify :: (Int -> Int) -> (CodePoint -> CodePoint)
modify = unsafeCoerce

modifyFull :: (Int -> Array Int) -> (CodePoint -> Array CodePoint)
modifyFull = unsafeCoerce
