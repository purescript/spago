module Node.FS.Constants where

import Prelude

import Data.Function.Uncurried (Fn2, runFn2)

-- | the mode parameter passed to `access` and `accessSync`.
foreign import data AccessMode :: Type

-- | the file is visible to the calling process. 
-- | This is useful for determining if a file exists, but says nothing about rwx permissions. Default if no mode is specified.
foreign import f_OK :: AccessMode

-- | the file can be read by the calling process.
foreign import r_OK :: AccessMode

-- | the file can be written by the calling process.
foreign import w_OK :: AccessMode

-- | the file can be executed by the calling process. This has no effect on Windows (will behave like fs.constants.F_OK).
foreign import x_OK :: AccessMode

defaultAccessMode = f_OK :: AccessMode

-- | A constant used in `copyFile`.
foreign import data CopyMode :: Type

-- | If present, the copy operation will fail with an error if the destination path already exists.
foreign import copyFile_EXCL :: CopyMode

-- | If present, the copy operation will attempt to create a copy-on-write reflink. If the underlying platform does not support copy-on-write, then a fallback copy mechanism is used.
foreign import copyFile_FICLONE :: CopyMode

-- |  	If present, the copy operation will attempt to create a copy-on-write reflink. If the underlying platform does not support copy-on-write, then the operation will fail with an error.
foreign import copyFile_FICLONE_FORCE :: CopyMode

defaultCopyMode = copyFile_EXCL :: CopyMode

foreign import appendCopyMode :: Fn2 CopyMode CopyMode CopyMode

instance Semigroup CopyMode where
  append l r = runFn2 appendCopyMode l r

data FileFlags
  = R
  | R_PLUS
  | RS
  | RS_PLUS
  | W
  | WX
  | W_PLUS
  | WX_PLUS
  | A
  | AX
  | A_PLUS
  | AX_PLUS

instance showFileFlags :: Show FileFlags where
  show R = "R"
  show R_PLUS = "R_PLUS"
  show RS = "RS"
  show RS_PLUS = "RS_PLUS"
  show W = "W"
  show WX = "WX"
  show W_PLUS = "W_PLUS"
  show WX_PLUS = "WX_PLUS"
  show A = "A"
  show AX = "AX"
  show A_PLUS = "A_PLUS"
  show AX_PLUS = "AX_PLUS"

instance eqFileFlags :: Eq FileFlags where
  eq x y = show x == show y

-- | Convert a `FileFlags` to a `String` in the format expected by the Node.js
-- | filesystem API.
fileFlagsToNode :: FileFlags -> String
fileFlagsToNode ff = case ff of
  R -> "r"
  R_PLUS -> "r+"
  RS -> "rs"
  RS_PLUS -> "rs+"
  W -> "w"
  WX -> "wx"
  W_PLUS -> "w+"
  WX_PLUS -> "wx+"
  A -> "a"
  AX -> "ax"
  A_PLUS -> "a+"
  AX_PLUS -> "ax+"
