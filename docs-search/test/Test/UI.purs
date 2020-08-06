module Test.UI where

import Prelude

import Data.Array as Array
import Data.Foldable (for_)
import Data.Newtype (unwrap)
import Effect (Effect)
import Effect.Aff (Aff, launchAff_)
import Effect.Class (liftEffect)
import Effect.Console as Console
import Foreign (Foreign)
import Foreign as Foreign
import Node.FS.Sync (realpath)
import Test.Unit.Assert as Assert
import Toppokki as T


main :: Effect Unit
main = launchAff_ do
  path <- liftEffect $ realpath "."

  let indexHTML  = T.URL $ "file://" <> path <> "/generated-docs/html/index.html"
      prim       = T.URL $ "file://" <> path <> "/generated-docs/html/Prim.html"
      docsSearch = T.URL $ "file://" <> path <> "/generated-docs/html/Docs.Search.App.html"

  for_ [ indexHTML, prim, docsSearch ] \url -> do
    withPage url \page -> do
      void $ T.pageWaitForSelector (T.Selector "#group-modules__label") { timeout: 10000 } page
    log $ "has module grouping:                  " <> unwrap url

    withPage url \page -> do
      T.keyboardPress (T.KeyboardKey "s") {} page
      void $ T.keyboardType "slice" {} page
      T.keyboardPress (T.KeyboardKey "Enter") {} page
      void $ T.pageWaitForSelector (T.Selector ".result__actions__item") { timeout: 10000 } page
    log $ "has working search field:             " <> unwrap url

    withPage url \page -> do
      T.keyboardPress (T.KeyboardKey "s") {} page
      void $ T.keyboardType "a -> b" {} page
      T.keyboardPress (T.KeyboardKey "Enter") {} page
      void $ T.pageWaitForSelector (T.Selector ".result__actions__item") { timeout: 10000 } page
      arr <- readStringArray <$> T.unsafeEvaluateStringFunction getResultTitlesCode page
      Assert.assert "resulting array contains unsafeCoerce" (Array.elem "unsafeCoerce" arr)
    log $ "is able to find unsafeCoerce by type: " <> unwrap url

    withPage (T.URL $ unwrap url <> "#search:unsafeCoerce") \page -> do
      void $ T.pageWaitForSelector (T.Selector ".result__actions__item") { timeout: 10000 } page
      arr <- readStringArray <$> T.unsafeEvaluateStringFunction getResultTitlesCode page
      Assert.assert "resulting array contains unsafeCoerce" (Array.elem "unsafeCoerce" arr)
    log $ "can read URI hash:                    " <> unwrap url

  where
    log = liftEffect <<< Console.log
    getResultTitlesCode = "[].map.call(document.querySelectorAll('.result__title'), el => el.textContent)"
    readStringArray :: Foreign -> Array String
    readStringArray = Foreign.unsafeFromForeign


withPage :: forall a. T.URL -> (T.Page -> Aff a) -> Aff Unit
withPage url f = do
  browser <- T.launch {}
  page <- T.newPage browser
  T.goto url page
  void $ f page
  T.close browser
