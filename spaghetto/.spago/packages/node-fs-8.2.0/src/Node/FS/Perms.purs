module Node.FS.Perms
  ( Perm
  , mkPerm
  , none
  , read
  , write
  , execute
  , all
  , Perms
  , mkPerms
  , permsAll
  , permsReadWrite
  , permsFromString
  , permsToString
  , permsToInt
  ) where

import Prelude

import Data.Enum (toEnum)
import Data.Int (fromStringAs, octal)
import Data.Maybe (Maybe(..), isNothing, fromJust)
import Data.String (Pattern(Pattern), joinWith, drop, indexOf)
import Data.String.CodeUnits (charAt, toCharArray)
import Partial.Unsafe (unsafePartial)

-- | A `Perm` value specifies what is allowed to be done with a particular
-- | file by a particular class of user &mdash; that is, whether it is
-- | readable, writable, and/or executable. It has a `Semiring` instance, which
-- | allows you to combine permissions:
-- |
-- | - `(+)` adds `Perm` values together. For example, `read + write` means
-- |   "readable and writable".
-- | - `(*)` masks permissions. It can be thought of as selecting only the
-- |    permissions that two `Perm` values have in common. For example:
-- |    `(read + write) * (write + execute) == write`.
-- |
-- | You can think also of a `Perm` value as a subset of the set
-- | `{ read, write, execute }`; then, `(+)` and `(*)` represent set union and
-- | intersection respectively.
newtype Perm = Perm { r :: Boolean, w :: Boolean, x :: Boolean }

instance eqPerm :: Eq Perm where
  eq (Perm { r: r1, w: w1, x: x1 }) (Perm { r: r2, w: w2, x: x2 }) =
    r1 == r2 && w1 == w2 && x1 == x2

instance ordPerm :: Ord Perm where
  compare (Perm { r: r1, w: w1, x: x1 }) (Perm { r: r2, w: w2, x: x2 }) =
    compare [ r1, w1, x1 ] [ r2, w2, x2 ]

instance showPerm :: Show Perm where
  show p | p == none = "none"
  show p | p == all = "all"
  show (Perm { r: r, w: w, x: x }) =
    joinWith " + " ps
    where
    ps =
      (if r then [ "read" ] else [])
        <> (if w then [ "write" ] else [])
        <>
          (if x then [ "execute" ] else [])

instance semiringPerm :: Semiring Perm where
  add (Perm { r: r0, w: w0, x: x0 }) (Perm { r: r1, w: w1, x: x1 }) =
    Perm { r: r0 || r1, w: w0 || w1, x: x0 || x1 }
  zero = Perm { r: false, w: false, x: false }
  mul (Perm { r: r0, w: w0, x: x0 }) (Perm { r: r1, w: w1, x: x1 }) =
    Perm { r: r0 && r1, w: w0 && w1, x: x0 && x1 }
  one = Perm { r: true, w: true, x: true }

-- | Create a `Perm` value. The arguments represent the readable, writable, and
-- | executable permissions, in that order.
mkPerm :: Boolean -> Boolean -> Boolean -> Perm
mkPerm r w x = Perm { r: r, w: w, x: x }

-- | No permissions. This is the identity of the `Semiring` operation `(+)`
-- | for `Perm`; that is, it is the same as `zero`.
none :: Perm
none = zero

-- | The "readable" permission.
read :: Perm
read = Perm { r: true, w: false, x: false }

-- | The "writable" permission.
write :: Perm
write = Perm { r: false, w: true, x: false }

-- | The "executable" permission.
execute :: Perm
execute = Perm { r: false, w: false, x: true }

-- | All permissions: readable, writable, and executable. This is the identity
-- | of the `Semiring` operation `(*)` for `Perm`; that is, it is the same as
-- | `one`.
all :: Perm
all = one

-- | A `Perms` value includes all the permissions information about a
-- | particular file or directory, by storing a `Perm` value for each of the
-- | file owner, the group, and any other users.
newtype Perms = Perms { u :: Perm, g :: Perm, o :: Perm }

instance eqPerms :: Eq Perms where
  eq (Perms { u: u1, g: g1, o: o1 }) (Perms { u: u2, g: g2, o: o2 }) =
    u1 == u2 && g1 == g2 && o1 == o2

instance ordPerms :: Ord Perms where
  compare (Perms { u: u1, g: g1, o: o1 }) (Perms { u: u2, g: g2, o: o2 }) =
    compare [ u1, g1, o1 ] [ u2, g2, o2 ]

instance showPerms :: Show Perms where
  show (Perms { u: u, g: g, o: o }) =
    "mkPerms " <> joinWith " " (f <$> [ u, g, o ])
    where
    f perm =
      let
        str = show perm
      in
        if isNothing $ indexOf (Pattern " ") str then str
        else "(" <> str <> ")"

-- | Attempt to parse a `Perms` value from a `String` containing an octal
-- | integer. For example,
-- | `permsFromString "0644" == Just (mkPerms (read + write) read read)`.
permsFromString :: String -> Maybe Perms
permsFromString = _perms <<< toCharArray <<< dropPrefix zeroChar
  where
  zeroChar = unsafePartial $ fromJust $ toEnum 48

  dropPrefix x xs
    | charAt 0 xs == Just x = drop 1 xs
    | otherwise = xs

  _perms [ u, g, o ] =
    mkPerms <$> permFromChar u
      <*> permFromChar g
      <*> permFromChar o
  _perms _ = Nothing

permFromChar :: Char -> Maybe Perm
permFromChar c = case c of
  '0' -> Just $ none
  '1' -> Just $ execute
  '2' -> Just $ write
  '3' -> Just $ write + execute
  '4' -> Just $ read
  '5' -> Just $ read + execute
  '6' -> Just $ read + write
  '7' -> Just $ read + write + execute
  _ -> Nothing

-- | Create a `Perms` value. The arguments represent the owner's, group's, and
-- | other users' permission sets, respectively.
mkPerms :: Perm -> Perm -> Perm -> Perms
mkPerms u g o = Perms { u: u, g: g, o: o }

permsAll :: Perms
permsAll = mkPerms all all all

permsReadWrite :: Perms
permsReadWrite = mkPerms all all none

-- | Convert a `Perm` to an octal digit. For example:
-- |
-- | * `permToInt r == 4`
-- | * `permToInt w == 2`
-- | * `permToInt (r + w) == 6`
permToInt :: Perm -> Int
permToInt (Perm { r: r, w: w, x: x }) =
  (if r then 4 else 0)
    + (if w then 2 else 0)
    + (if x then 1 else 0)

-- | Convert a `Perm` to an octal string, via `permToInt`.
permToString :: Perm -> String
permToString = show <<< permToInt

-- | Convert a `Perms` value to an octal string, in a format similar to that
-- | accepted by `chmod`. For example:
-- | `permsToString (mkPerms (read + write) read read) == "0644"`
permsToString :: Perms -> String
permsToString (Perms { u: u, g: g, o: o }) =
  "0"
    <> permToString u
    <> permToString g
    <> permToString o

-- | Convert a `Perms` value to an `Int`, via `permsToString`.
permsToInt :: Perms -> Int
permsToInt = unsafePartial $ fromJust <<< fromStringAs octal <<< permsToString
