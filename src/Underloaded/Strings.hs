module Underloaded.Strings
  ( Text, LazyText, ByteString, LazyByteString
    -- constructors
  , t, b, lb 
    -- constructor operators 
  , (#), (~#), (&), (~&)
    -- conversions
  , b2t, t2b, t2lb
  , lb2t, lb2s, lb2b
  , b2lb, t2s, b2s
    -- small helpers
  , bc, lbc, tc, ltc
  ) where

import           Data.ByteString      (ByteString, fromStrict)
import qualified Data.ByteString.Char8 as B8
import           Data.ByteString.Lazy (LazyByteString)
import qualified Data.ByteString.Lazy as LBS
import           Data.Text            (Text)
import qualified Data.Text            as T
import qualified Data.Text.Lazy       as TL
import           Data.Text.Encoding   (decodeUtf8, encodeUtf8)

type LazyText = TL.Text

-- constructors from String
t   :: String -> Text
t   = T.pack

lt  :: String -> LazyText
lt  = TL.pack

b  :: String -> ByteString
b  = B8.pack

lb :: String -> LazyByteString
lb = LBS.fromStrict . B8.pack

-- apply-with-conversion operators: (expects f, feeds it a converted String)
infixr 0 #, ~#, &, ~&

(#)  :: (Text        -> b) -> String -> b
(#)  f s = f (t  s)

(~#) :: (TL.Text     -> b) -> String -> b
(~#) f s = f (lt s)

(&)  :: (ByteString  -> b) -> String -> b
(&)  f s = f (b s)

(~&) :: (LazyByteString -> b) -> String -> b
(~&) f s = f (lb s)

-- conversions
b2t  :: ByteString -> Text
b2t  = decodeUtf8

t2b  :: Text -> ByteString
t2b  = encodeUtf8

t2s  :: Text -> String 
t2s  = T.unpack

t2lb :: Text -> LazyByteString
t2lb = LBS.fromStrict . encodeUtf8

lb2t :: LazyByteString -> Text
lb2t = decodeUtf8 . LBS.toStrict

lb2s :: LazyByteString -> String
lb2s = B8.unpack . LBS.toStrict

b2s  :: ByteString -> String 
b2s  = B8.unpack 

lb2b :: LazyByteString -> ByteString
lb2b = LBS.toStrict

b2lb :: ByteString -> LazyByteString
b2lb = fromStrict

-- list-of-String to lazy bytes (handy for tiny messages)
bc  :: [String] -> ByteString
bc  = b . concat

lbc :: [String] -> LazyByteString
lbc = lb . concat

tc  :: [String] -> Text
tc  = t . concat

ltc :: [String] -> TL.Text
ltc = lt . concat
