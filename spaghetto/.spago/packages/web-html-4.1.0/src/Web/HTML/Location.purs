module Web.HTML.Location
  ( Location
  , hash
  , setHash
  , host
  , setHost
  , hostname
  , setHostname
  , href
  , setHref
  , origin
  , setOrigin
  , pathname
  , setPathname
  , port
  , setPort
  , protocol
  , setProtocol
  , search
  , setSearch

  , assign
  , replace
  , reload
  ) where

import Prelude

import Effect (Effect)

foreign import data Location :: Type

foreign import hash :: Location -> Effect String
foreign import setHash :: String -> Location -> Effect Unit

foreign import host :: Location -> Effect String
foreign import setHost :: String -> Location -> Effect Unit

foreign import hostname :: Location -> Effect String
foreign import setHostname :: String -> Location -> Effect Unit

foreign import href :: Location -> Effect String
foreign import setHref :: String -> Location -> Effect Unit

foreign import origin :: Location -> Effect String
foreign import setOrigin :: String -> Location -> Effect Unit

foreign import pathname :: Location -> Effect String
foreign import setPathname :: String -> Location -> Effect Unit

foreign import port :: Location -> Effect String
foreign import setPort :: String -> Location -> Effect Unit

foreign import protocol :: Location -> Effect String
foreign import setProtocol :: String -> Location -> Effect Unit

foreign import search :: Location -> Effect String
foreign import setSearch :: String -> Location -> Effect Unit

foreign import assign :: String -> Location -> Effect Unit
foreign import replace :: String -> Location -> Effect Unit
foreign import reload :: Location -> Effect Unit
