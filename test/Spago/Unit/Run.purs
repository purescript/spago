module Test.Spago.Unit.Run where

import Test.Prelude

import Spago.Command.Run (encodeFileUrlPath)
import Test.Spec (Spec)
import Test.Spec as Spec

spec :: Spec Unit
spec = Spec.describe "Run" do

  Spec.describe "encodeFileUrlPath" do

    Spec.it "encodes spaces" do
      encodeFileUrlPath "/path/with spaces/file" `shouldEqual` "/path/with%20spaces/file"

    Spec.it "encodes apostrophes" do
      encodeFileUrlPath "/Volumes/Tim's Docs/project" `shouldEqual` "/Volumes/Tim%27s%20Docs/project"

    Spec.it "encodes hash symbols" do
      encodeFileUrlPath "/path/test#1/file" `shouldEqual` "/path/test%231/file"

    Spec.it "encodes brackets" do
      encodeFileUrlPath "/path/test[dev]/file" `shouldEqual` "/path/test%5Bdev%5D/file"

    Spec.it "preserves Windows drive letters" do
      encodeFileUrlPath "C:/Users/test" `shouldEqual` "C:/Users/test"
      encodeFileUrlPath "D:/a/spago/output" `shouldEqual` "D:/a/spago/output"

    Spec.it "preserves Windows drive letters with special chars in path" do
      encodeFileUrlPath "C:/Users/Tim's Folder/project" `shouldEqual` "C:/Users/Tim%27s%20Folder/project"

    Spec.it "handles lowercase drive letters" do
      encodeFileUrlPath "c:/users/test" `shouldEqual` "c:/users/test"

    Spec.it "encodes colon in non-drive-letter segments" do
      -- A colon not at the start as a drive letter should be encoded
      encodeFileUrlPath "/path/file:name/test" `shouldEqual` "/path/file%3Aname/test"

    Spec.it "leaves normal paths unchanged" do
      encodeFileUrlPath "/home/user/project/output" `shouldEqual` "/home/user/project/output"
