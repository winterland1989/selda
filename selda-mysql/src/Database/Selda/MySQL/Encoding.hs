{-# LANGUAGE GADTs, BangPatterns, OverloadedStrings #-}
-- | Encoding/decoding for MySQL.
module Database.Selda.MySQL.Encoding
  ( toSqlValue, fromSqlValue
  , readInt
  ) where
import qualified Data.ByteString as BS
import Data.ByteString.Builder
import Data.ByteString.Char8 (unpack)
import qualified Data.ByteString.Lazy as LBS
import Data.Text.Encoding
import Database.Selda.Backend (Lit (..), SqlValue (..))
import Unsafe.Coerce

-- | OIDs for all types used by Selda.
boolType, intType, textType, doubleType, dateType, timeType, timestampType :: Oid
boolType      = Oid 16
intType       = Oid 20
textType      = Oid 25
doubleType    = Oid 701
dateType      = Oid 1082
timeType      = Oid 1083
timestampType = Oid 1114

-- | Convert a parameter into an postgres parameter triple.
fromSqlValue :: Lit a -> Maybe (Oid, BS.ByteString, Format)
fromSqlValue (LBool b)     = Just (boolType, toBS $ if b then word8 1 else word8 0, Binary)
fromSqlValue (LInt n)      = Just (intType, toBS $ int64BE (fromIntegral n), Binary)
fromSqlValue (LDouble f)   = Just (doubleType, toBS $ int64BE (unsafeCoerce f), Binary)
fromSqlValue (LText s)     = Just (textType, encodeUtf8 s, Text)
fromSqlValue (LDateTime s) = Just (timestampType, encodeUtf8 s, Text)
fromSqlValue (LTime s)     = Just (timeType, encodeUtf8 s, Text)
fromSqlValue (LDate s)     = Just (dateType, encodeUtf8 s, Text)
fromSqlValue (LNull)       = Nothing
fromSqlValue (LJust x)     = fromSqlValue x
fromSqlValue (LCustom l)   = fromSqlValue l

-- | Convert the given postgres return value and type to an @SqlValue@.
toSqlValue :: Oid -> BS.ByteString -> SqlValue
toSqlValue t val
  | t == boolType    = SqlBool $ readBool val
  | t == intType     = SqlInt $ readInt val
  | t == doubleType  = SqlFloat $ read (unpack val)
  | t `elem` textish = SqlString (decodeUtf8 val)
  | otherwise        = error $ "BUG: result with unknown type oid: " ++ show t
  where
    textish = [textType, timestampType, timeType, dateType]
    readBool "f"     = False
    readBool "0"     = False
    readBool "false" = False
    readBool "n"     = False
    readBool "no"    = False
    readBool "off"   = False
    readBool _       = True

-- | Read an integer from a strict bytestring.
--   Assumes that the bytestring does, in fact, contain an integer.
readInt :: BS.ByteString -> Int
readInt s
  | BS.head s == asciiDash = negate $! go 1 0
  | otherwise              = go 0 0
  where
    !len = BS.length s
    !asciiZero = 48
    !asciiDash = 45
    go !i !acc
      | len > i   = go (i+1) (acc * 10 + fromIntegral (BS.index s i - asciiZero))
      | otherwise = acc

-- | Reify a builder to a strict bytestring.
toBS :: Builder -> BS.ByteString
toBS = unChunk . toLazyByteString

-- | Convert a lazy bytestring to a strict one.
--   Avoids the copying overhead of 'LBS.toStrict' when there's only a single
--   chunk, which should always be the case when serializing single parameters.
unChunk :: LBS.ByteString -> BS.ByteString
unChunk bs =
  case LBS.toChunks bs of
    [bs'] -> bs'
    bss   -> BS.concat bss
