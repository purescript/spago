module Spago.Core.Config.Migrations.FromSnakeCase
  ( migrateFromSnakeCase
  , class MigrateFromSnakeCase'
  , migrateFromSnakeCase'
  , class MigrateFromSnakeCaseRow
  , migrateFromSnakeCaseRow
  ) where

import Spago.Core.Prelude

import Data.Array.NonEmpty as NonEmptyArray
import Data.List (List(..))
import Data.String as String
import Data.String.Regex as Regex
import Data.String.Regex.Flags as RegexFlags
import Data.String.Regex.Unsafe as UnsafeRegex
import Data.Symbol (class IsSymbol, reflectSymbol)
import Data.Traversable (sequence)
import Prim.RowList as RL
import Spago.Core.Config (BundlePlatform, BundleType, CensorBuildWarnings, Config, Dependencies, ExtraPackage, SetAddress, StatVerbosity, WarningCensorTest, WarningCensorTestJsonEncoding)
import Spago.Yaml (YamlMigrationStep(..))
import Type.Proxy (Proxy(..))

migrateFromSnakeCase :: List YamlMigrationStep
migrateFromSnakeCase = migrateFromSnakeCase' (Proxy :: Proxy Config)

class MigrateFromSnakeCase' :: Type -> Constraint
class MigrateFromSnakeCase' a where
  migrateFromSnakeCase' :: Proxy a -> List YamlMigrationStep

instance 
  ( RL.RowToList WarningCensorTestJsonEncoding rl
  , MigrateFromSnakeCaseRow rl
  ) => MigrateFromSnakeCase' WarningCensorTest where
  migrateFromSnakeCase' _ = migrateFromSnakeCaseRow (Proxy :: Proxy rl)

-- SetAddress doesn't have any memebers using snake case
instance MigrateFromSnakeCase' SetAddress where
  migrateFromSnakeCase' _ = Nil

-- Dependencies should not be modified
instance MigrateFromSnakeCase' Dependencies where
  migrateFromSnakeCase' _ = Nil

-- Values under this field should not be modified
instance MigrateFromSnakeCase' (Map PackageName ExtraPackage) where
  migrateFromSnakeCase' _ = Nil

-- Doesn't have any members using snake case
instance MigrateFromSnakeCase' StatVerbosity where
  migrateFromSnakeCase' _ = Nil

instance MigrateFromSnakeCase' CensorBuildWarnings where
  migrateFromSnakeCase' _ = migrateFromSnakeCase' (Proxy :: Proxy WarningCensorTest)

instance MigrateFromSnakeCase' String where
  migrateFromSnakeCase' _ = Nil

instance MigrateFromSnakeCase' Boolean where
  migrateFromSnakeCase' _ = Nil

instance MigrateFromSnakeCase' Version where
  migrateFromSnakeCase' _ = Nil

instance MigrateFromSnakeCase' Location where
  migrateFromSnakeCase' _ = 
    migrateFromSnakeCase' (Proxy :: Proxy { githubOwner :: String, githubRepo :: String, subdir :: Maybe String })
    <> migrateFromSnakeCase' (Proxy :: Proxy { gitUrl :: String, subdir :: Maybe String })

-- License is (at the time of writing) a newtyped String
instance MigrateFromSnakeCase' License where
  migrateFromSnakeCase' _ = Nil

-- PackageName is (at the time of writing) a newtyped String
instance MigrateFromSnakeCase' PackageName where
  migrateFromSnakeCase' _ = Nil

instance MigrateFromSnakeCase' BundleType where
  migrateFromSnakeCase' _ = Nil

instance MigrateFromSnakeCase' BundlePlatform where
  migrateFromSnakeCase' _ = Nil

instance 
  ( MigrateFromSnakeCase' a
  ) => MigrateFromSnakeCase' (Maybe a) where
  migrateFromSnakeCase' _ = migrateFromSnakeCase' (Proxy :: Proxy a)

instance 
  ( MigrateFromSnakeCase' a
  ) => MigrateFromSnakeCase' (Array a) where
  migrateFromSnakeCase' _ = migrateFromSnakeCase' (Proxy :: Proxy a)

instance 
  ( RL.RowToList r rl
  , MigrateFromSnakeCaseRow rl
  ) => MigrateFromSnakeCase' { | r } where
  migrateFromSnakeCase' _ = migrateFromSnakeCaseRow (Proxy :: Proxy rl)

class MigrateFromSnakeCaseRow :: RL.RowList Type -> Constraint
class MigrateFromSnakeCaseRow rl where
  migrateFromSnakeCaseRow :: Proxy rl -> List YamlMigrationStep

instance MigrateFromSnakeCaseRow RL.Nil where
  migrateFromSnakeCaseRow _ = Nil

instance 
  ( MigrateFromSnakeCaseRow tail
  , MigrateFromSnakeCase' a
  , IsSymbol sym
  ) => MigrateFromSnakeCaseRow (RL.Cons sym a tail) where
  migrateFromSnakeCaseRow _ = 
    (YamlMigrationStep
      { key: reflectSymbol (Proxy :: Proxy sym)
      , toPrevKey: Just toSnakeCase
      , updateValue: migrateFromSnakeCase' (Proxy :: Proxy a)
      }) : migrateFromSnakeCaseRow (Proxy :: Proxy tail)

toSnakeCase :: String -> String
toSnakeCase key = case Regex.match splitCamelCase key of
  Just parts | Just parts' <- sequence parts ->
    NonEmptyArray.intercalate "_" $ map String.toLower parts'
  _ -> key
  where
  splitCamelCase = UnsafeRegex.unsafeRegex "(?:[A-Z][a-z]*|[a-z]+)" RegexFlags.global