module Node.Path where

-- | Type for strings representing file paths.
import Effect (Effect)

-- | Type for strings representing file paths.
type FilePath = String

-- | Normalize a string path, taking care of `..` and `.`, duplicated slashes,
-- | etc. If the path contains a trailing slash it is preserved. On Windows
-- | backslashes are used.
foreign import normalize :: FilePath -> FilePath

-- | Concatenates multiple path segments together and normalizes the resulting path.
foreign import concat :: Array FilePath -> FilePath

-- | Resolves `to` to an absolute path ([from...], to).
foreign import resolve :: Array FilePath -> FilePath -> Effect FilePath

-- | Solve the relative path from `from` to `to`.
foreign import relative :: FilePath -> FilePath -> FilePath

-- | Return the directory name of a path.
foreign import dirname :: FilePath -> FilePath

-- | Return the last portion of a path.
foreign import basename :: FilePath -> FilePath

-- | Return the last portion of a path, also dropping a specific file extension
-- | if it matches the end of the name.
foreign import basenameWithoutExt :: FilePath -> FilePath -> FilePath

-- | Return the extension of the path, from the last `.` to end of string in the
-- | last portion of the path. If there is no `.` in the last portion of the
-- | path or the first character of it is `.`, then it returns an empty string.
foreign import extname :: FilePath -> FilePath

-- | The platform-specific file separator. `\\` or `/`.
foreign import sep :: String

-- | The platform-specific path delimiter, `;` or `:`.
foreign import delimiter :: String

-- | Parse a path into components.
foreign import parse :: String -> { root :: String, dir :: String, base :: String, ext :: String, name :: String }

-- | Determines whether path is an absolute path
foreign import isAbsolute :: String -> Boolean
