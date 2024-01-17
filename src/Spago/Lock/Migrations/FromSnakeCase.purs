module Spago.Lock.Migrations.FromSnakeCase where

import Spago.Core.Prelude

import Data.Array.NonEmpty as NonEmptyArray
import Data.List (List(..))
import Data.List as List
import Data.String as String
import Data.String.Regex as Regex
import Data.String.Regex.Flags as RegexFlags
import Data.String.Regex.Unsafe as UnsafeRegex
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Traversable (sequence)
import Prim.RowList as RL
import Spago.Core.Config (Dependencies, ExtraPackage, RemotePackage, SetAddress)
import Spago.Lock (LockEntry, Lockfile)
import Spago.Yaml (YamlMigrationStep(..))
import Type.Proxy (Proxy(..))

migrateLockFromSnakeCase :: List YamlMigrationStep
migrateLockFromSnakeCase = migrateLockFromSnakeCase' (Proxy :: Proxy Lockfile)

class MigrateLockFromSnakeCase' :: Type -> Constraint
class MigrateLockFromSnakeCase' a where
  migrateLockFromSnakeCase' :: Proxy a -> List YamlMigrationStep

-- SetAddress doesn't have any memebers using snake case
instance MigrateLockFromSnakeCase' SetAddress where
  migrateLockFromSnakeCase' _ = Nil

instance MigrateLockFromSnakeCase' ExtraPackage where
  migrateLockFromSnakeCase' _ = Nil

-- Dependencies should not be modified
instance MigrateLockFromSnakeCase' Dependencies where
  migrateLockFromSnakeCase' _ = Nil

instance MigrateLockFromSnakeCase' LockEntry where
  migrateLockFromSnakeCase' _ = Nil

-- Values under this field should not be modified
instance 
  ( MigrateLockFromSnakeCase' v
  ) => MigrateLockFromSnakeCase' (Map k v) where
  migrateLockFromSnakeCase' _ = List.singleton $
    YamlMigrationStep
      { key: "foo"
      , toPrevKey: Nothing
      , updateValue: migrateLockFromSnakeCase' (Proxy :: Proxy v)
      }

instance MigrateLockFromSnakeCase' String where
  migrateLockFromSnakeCase' _ = Nil

instance MigrateLockFromSnakeCase' (Set PackageName) where
  migrateLockFromSnakeCase' _ = Nil

-- Single-word usage
instance MigrateLockFromSnakeCase' RemotePackage where
  migrateLockFromSnakeCase' _ = Nil

instance MigrateLockFromSnakeCase' Range where
  migrateLockFromSnakeCase' _ = Nil

instance 
  ( MigrateLockFromSnakeCase' a
  ) => MigrateLockFromSnakeCase' (Maybe a) where
  migrateLockFromSnakeCase' _ = migrateLockFromSnakeCase' (Proxy :: Proxy a)

instance 
  ( MigrateLockFromSnakeCase' a
  ) => MigrateLockFromSnakeCase' (Array a) where
  migrateLockFromSnakeCase' _ = migrateLockFromSnakeCase' (Proxy :: Proxy a)

instance 
  ( RL.RowToList r rl
  , MigrateLockFromSnakeCaseRow rl
  ) => MigrateLockFromSnakeCase' { | r } where
  migrateLockFromSnakeCase' _ = migrateLockFromSnakeCaseRow (Proxy :: Proxy rl)

class MigrateLockFromSnakeCaseRow :: RL.RowList Type -> Constraint
class MigrateLockFromSnakeCaseRow rl where
  migrateLockFromSnakeCaseRow :: Proxy rl -> List YamlMigrationStep

instance MigrateLockFromSnakeCaseRow RL.Nil where
  migrateLockFromSnakeCaseRow _ = Nil

instance 
  ( MigrateLockFromSnakeCaseRow tail
  , MigrateLockFromSnakeCase' a
  , IsSymbol sym
  ) => MigrateLockFromSnakeCaseRow (RL.Cons sym a tail) where
  migrateLockFromSnakeCaseRow _ = 
    (YamlMigrationStep
      { key: reflectSymbol (Proxy :: Proxy sym)
      , toPrevKey: Just toSnakeCase
      , updateValue: migrateLockFromSnakeCase' (Proxy :: Proxy a)
      }) : migrateLockFromSnakeCaseRow (Proxy :: Proxy tail)

toSnakeCase :: String -> String
toSnakeCase key = case Regex.match splitCamelCase key of
  Just parts | Just parts' <- sequence parts ->
    NonEmptyArray.intercalate "_" $ map String.toLower parts'
  _ -> key
  where
  splitCamelCase = UnsafeRegex.unsafeRegex "(?:[A-Z][a-z]*|[a-z]+)" RegexFlags.global