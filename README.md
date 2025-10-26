# Underloaded.Strings — ergonomic text/bytes without `OverloadedStrings`

Small helpers to keep string literals as `String` and convert **only where needed**—so your types stay explicit and type-hole messages stay clear.

> **Why?** `OverloadedStrings` can blur intent and make error messages noisier. This module lets you keep `NoOverloadedStrings` while still writing tidy code.

---

## Quick taste

```haskell
{-# LANGUAGE NoOverloadedStrings #-}
module Main where

import qualified Data.Text           as T
import qualified Data.Text.IO        as TIO
import qualified Data.ByteString     as BS
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString.Lazy as LBS

import Underloaded.Strings

main :: IO ()
main = do
  -- pass a String to a function that wants Text
  TIO.putStrLn # "Hello, Text 👋"

  -- pass a String to a function that wants ByteString
  B8.putStrLn  & "Hello, ByteString"

  -- pass a String to a function that wants Lazy ByteString
  print (LBS.length ~& "lazy bytes")

  -- plain constructors from String
  let nameT  = t  "Nick"
      bodyB  = t2b (t "utf8 ✓")     -- UTF-8 encode Text to ByteString
  print (T.length nameT, BS.length bodyB)
```

No extensions, no ambiguity; just explicit conversions when you call a function.

---

## What you get

```haskell
module Underloaded.Strings
  ( -- common types re-exported
    Text, LazyText, ByteString, LazyByteString

    -- constructors (String -> …)
  , t, b, lb

    -- “apply-with-conversion” operators
  , (#)   -- (Text            -> r) -> String -> r
  , (~#)  -- (LazyText        -> r) -> String -> r
  , (&)   -- (ByteString      -> r) -> String -> r
  , (~&)  -- (LazyByteString  -> r) -> String -> r

    -- conversions
  , b2t, t2b, t2lb
  , lb2t, lb2s, lb2b
  , b2lb, t2s, b2s

    -- small helpers (concatenate [String] then build)
  , bc, lbc, tc, ltc
  ) where
```

### Cheat-sheet

| Task | Use |
|---|---|
| `String -> Text` | `t "..."` or `f # "..."` |
| `String -> LazyText` | `f ~# "..."` |
| `String -> ByteString` | `b "..."` or `f & "..."` |
| `String -> LazyByteString` | `lb "..."` or `f ~& "..."` |
| `ByteString <-> Text (UTF-8)` | `b2t`, `t2b` |
| `LazyByteString <-> Text (UTF-8)` | `lb2t`, `t2lb` |
| `… -> String` | `t2s`, `b2s`, `lb2s` |
| Strict/Lazy bytes bridge | `b2lb`, `lb2b` |
| Glue a list of `String` then build | `tc`, `ltc`, `bc`, `lbc` |

> The operators are **right-associative** (`infixr 0`) so they feel like “pipe a `String` into a function that wants X”.

---

## Examples

### 1) Working with Text APIs (no `OverloadedStrings`)

```haskell
{-# LANGUAGE NoOverloadedStrings #-}
import qualified Data.Text           as T
import qualified Data.Text.IO        as TIO
import Underloaded.Strings

hello :: IO ()
hello = do
  TIO.putStrLn # "Welcome!"
  let title :: T.Text
      title = t "Underloaded"
  print (T.length title)                   -- 11
  print (T.isSuffixOf (t "loaded") title)  -- True
```

### 2) Bytes for I/O and protocols

```haskell
{-# LANGUAGE NoOverloadedStrings #-}
import qualified Data.ByteString.Char8 as B8
import qualified Data.ByteString       as BS
import Underloaded.Strings

wire :: IO ()
wire = do
  -- Build ByteString from String once:
  let payload = b "PING"
  B8.putStrLn payload

  -- Or just feed a String to a ByteString function:
  print (BS.length & "PING")          -- 4
```

### 3) Lazy bytes & JSON (Aeson)

```haskell
{-# LANGUAGE NoOverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables  #-}
import           Data.Aeson (eitherDecode, Value)
import           Underloaded.Strings

demoJSON :: IO ()
demoJSON = do
  -- eitherDecode :: LazyByteString -> Either String a
  print (eitherDecode ~& "{"x":1}" :: Either String Value)
```

### 4) Tiny message builders

```haskell
{-# LANGUAGE NoOverloadedStrings #-}
import qualified Data.Text.IO as TIO
import qualified Data.ByteString.Char8 as B8
import Underloaded.Strings

logLine :: IO ()
logLine = do
  TIO.putStrLn (tc  ["[ok] ", "compiled ", "42 ", "files"])
  B8.putStrLn (bc  ["GET ", "/health", " HTTP/1.1"])
```

---

## Notes & gotchas

- **Encoding choices**
  - `t2b` / `t2lb` and `b2t` / `lb2t` use **UTF-8**.
  - `b` / `lb` (and `b2s` / `lb2s`) use `ByteString.Char8` semantics (byte-wise). For non-ASCII text, prefer `t "…"` then `t2b`/`t2lb`.
  - `b2t` uses `decodeUtf8` (partial): invalid bytes will throw at runtime. If you need tolerant decoding, use `decodeUtf8'` yourself.

- **Operator `(&)` name clash**
  - This module defines `(&)` for `String -> ByteString` application. `Data.Function` also defines `(&)` as reverse application. If you import that, either:
    - hide one: `import Data.Function hiding ((&))`, or
    - import us qualified: `import qualified Underloaded.Strings as U` and write `B8.putStrLn U.& "hi"`.

- **Performance**
  - These helpers are thin wrappers; GHC will inline them. Still, avoid repeated conversions in hot loops—bind once (`let bs = b "..."`) and reuse.

---

## Import

This is just a single module. Drop it into your project and:

```haskell
import Underloaded.Strings
```

(If you later publish a package, the import stays the same.)

---

## License

BSD-3-Clause
