{-|
Module      : Underloaded.Strings
Description : Explicit string constructors & conversions without OverloadedStrings.

This tiny utility module gives you lightweight, explicit helpers for moving
between 'String', strict 'Text', lazy 'Text' ('LazyText'), strict 'ByteString',
and lazy 'ByteString'—without enabling @OverloadedStrings@.

It also includes “apply-with-conversion” operators so you can pass a 'String'
literal directly to a function that expects one of these other types:

@
import qualified Data.Text as Text
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS

Text.length  #  "hello"   -- 5
Text.length ~# "hello"    -- 5 (lazy Text)
BS.length    &  "hello"   -- 5
LBS.length   ~& "hello"   -- 5
@

### Notes

* Functions using @"Data.ByteString.Char8"@ ('B8') are **byte-wise**: they
  treat characters as their low 8 bits. That’s convenient but **lossy** for
  non-ASCII. Prefer UTF-8 conversions when correctness matters.
* UTF-8 decoders here use 'decodeUtf8', which will **throw** a
  'UnicodeException' on invalid input. If you need a total function, prefer the
  primed variants from @"Data.Text.Encoding"@ (e.g. 'decodeUtf8' → @decodeUtf8'@).

-}
module Underloaded.Strings
  ( -- * Types re-exported for convenience
    Text, LazyText, ByteString, LazyByteString

    -- * Constructors from 'String'
  , t     -- ^ 'String' → strict 'Text'
  , lt    -- ^ 'String' → lazy   'LazyText'
  , b     -- ^ 'String' → strict 'ByteString' (Char8 / byte-wise)
  , lb    -- ^ 'String' → lazy   'LazyByteString' (Char8 / byte-wise)

    -- * “Apply-with-conversion” operators
    -- |
    -- Use these to feed a 'String' to a function that expects a particular
    -- text/bytes type, converting on the fly.
  , (#)   -- ^ Apply with 'Text' (strict)
  , (~#)  -- ^ Apply with 'LazyText'
  , (&)   -- ^ Apply with strict 'ByteString' (Char8 / byte-wise)
  , (~&)  -- ^ Apply with lazy   'ByteString' (Char8 / byte-wise)

    -- * Conversions (UTF-8 unless stated)
  , b2t, t2b, t2lb
  , lb2t, lb2s, lb2b
  , b2lb, t2s, b2s

    -- * Small helpers (concat a list of 'String's then convert)
  , bc, lbc, tc, ltc
  ) where

import           Data.ByteString       (ByteString, fromStrict)
import qualified Data.ByteString.Char8 as B8
import           Data.ByteString.Lazy  (LazyByteString)
import qualified Data.ByteString.Lazy  as LBS
import           Data.Text             (Text)
import qualified Data.Text             as T
import qualified Data.Text.Lazy        as TL
import           Data.Text.Encoding    (decodeUtf8, encodeUtf8)

-- | Lazy 'Text' alias, re-exported as 'LazyText'.
type LazyText = TL.Text

-- --------------------------------------------------------------------------
-- Constructors from String
-- --------------------------------------------------------------------------

-- | Pack a 'String' into strict 'Text'.
--
-- >>> T.length (t "hello")
-- 5
t :: String -> Text
t = T.pack

-- | Pack a 'String' into lazy 'LazyText'.
--
-- >>> TL.length (lt "hello")
-- 5
lt :: String -> LazyText
lt = TL.pack

-- | Pack a 'String' into a strict 'ByteString' using @"Data.ByteString.Char8"@.
-- Each Char is truncated to 8 bits (byte-wise). **Lossy for non-ASCII.**
--
-- >>> B8.length (b "abc")
-- 3
b :: String -> ByteString
b = B8.pack

-- | Pack a 'String' into a lazy 'ByteString' (Char8 semantics). **Lossy for non-ASCII.**
--
-- >>> LBS.length (lb "abc")
-- 3
lb :: String -> LazyByteString
lb = LBS.fromStrict . B8.pack

-- --------------------------------------------------------------------------
-- Apply-with-conversion operators
-- --------------------------------------------------------------------------

-- | Apply a function expecting strict 'Text' to a 'String'.
--
-- >>> T.length # "hello"
-- 5
infixr 0 #, ~#, &, ~&
(#) :: (Text -> b) -> String -> b
(#) f s = f (t s)

-- | Apply a function expecting lazy 'LazyText' to a 'String'.
--
-- >>> TL.length ~# "hello"
-- 5
(~#) :: (TL.Text -> b) -> String -> b
(~#) f s = f (lt s)

-- | Apply a function expecting strict 'ByteString' (Char8) to a 'String'.
--
-- >>> B8.length & "hello"
-- 5
(&) :: (ByteString -> b) -> String -> b
(&) f s = f (b s)

-- | Apply a function expecting lazy 'ByteString' (Char8) to a 'String'.
--
-- >>> LBS.length ~& "hello"
-- 5
(~&) :: (LazyByteString -> b) -> String -> b
(~&) f s = f (lb s)

-- --------------------------------------------------------------------------
-- Conversions
-- --------------------------------------------------------------------------

-- | Strict 'ByteString' (assumed UTF-8) → strict 'Text'.
-- Throws 'UnicodeException' on invalid UTF-8.
--
-- >>> b2t (encodeUtf8 (t "hi"))
-- "hi"
b2t :: ByteString -> Text
b2t = decodeUtf8

-- | Strict 'Text' → strict 'ByteString' (UTF-8).
--
-- >>> t2b (t "hi") == encodeUtf8 (t "hi")
-- True
t2b :: Text -> ByteString
t2b = encodeUtf8

-- | Strict 'Text' → 'String'.
--
-- >>> t2s (t "ok")
-- "ok"
t2s :: Text -> String
t2s = T.unpack

-- | Strict 'Text' → lazy 'ByteString' (UTF-8).
--
-- >>> LBS.toStrict (t2lb (t "x")) == encodeUtf8 (t "x")
-- True
t2lb :: Text -> LazyByteString
t2lb = LBS.fromStrict . encodeUtf8

-- | Lazy 'ByteString' (assumed UTF-8) → strict 'Text'.
-- Throws 'UnicodeException' on invalid UTF-8.
--
-- >>> lb2t (t2lb (t "yo"))
-- "yo"
lb2t :: LazyByteString -> Text
lb2t = decodeUtf8 . LBS.toStrict

-- | Lazy 'ByteString' (Char8 semantics) → 'String' (byte-wise / lossy).
--
-- >>> lb2s (lb "abc")
-- "abc"
lb2s :: LazyByteString -> String
lb2s = B8.unpack . LBS.toStrict

-- | Strict 'ByteString' (Char8 semantics) → 'String' (byte-wise / lossy).
--
-- >>> b2s (b "abc")
-- "abc"
b2s :: ByteString -> String
b2s = B8.unpack

-- | Lazy → strict 'ByteString' (no re-encoding).
--
-- >>> lb2b (lb "a") == b "a"
-- True
lb2b :: LazyByteString -> ByteString
lb2b = LBS.toStrict

-- | Strict → lazy 'ByteString' (no re-encoding).
--
-- >>> LBS.toStrict (b2lb (b "a")) == b "a"
-- True
b2lb :: ByteString -> LazyByteString
b2lb = fromStrict

-- --------------------------------------------------------------------------
-- Small helpers
-- --------------------------------------------------------------------------

-- | Concatenate a list of 'String's and build a strict 'ByteString' (Char8).
-- Handy for tiny messages. **Lossy for non-ASCII.**
--
-- >>> bc ["he","llo"] == b "hello"
-- True
bc :: [String] -> ByteString
bc = b . concat

-- | Concatenate a list of 'String's and build a lazy 'ByteString' (Char8).
-- **Lossy for non-ASCII.**
--
-- >>> lbc ["he","llo"] == lb "hello"
-- True
lbc :: [String] -> LazyByteString
lbc = lb . concat

-- | Concatenate a list of 'String's and build strict 'Text'.
--
-- >>> tc ["a","b","c"]
-- "abc"
tc :: [String] -> Text
tc = t . concat

-- | Concatenate a list of 'String's and build lazy 'Text'.
--
-- >>> TL.toStrict (ltc ["a","b","c"])
-- "abc"
ltc :: [String] -> TL.Text
ltc = lt . concat
