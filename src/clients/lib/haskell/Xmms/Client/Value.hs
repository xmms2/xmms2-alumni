module Xmms.Client.Value where

import Control.Monad (replicateM, liftM)
import Data.Binary
import Data.Word
import Data.Int
 
data Value = NoneValue
           | IntValue Int32
           | StringValue String
           | ListValue [Value]
           | DictValue [(String, Value)]
           deriving (Show)

putWord32 :: Word32 -> Put
putWord32 = put

getWord32 :: Get Word32
getWord32 = get

-- Write a raw string (ie without the type tag)
myPutRawStr :: String -> Put
myPutRawStr s = putWord32 (fromIntegral (succ (length s))) >> mapM_ put s >> put '\0'

-- Read a raw string (ie without the type tag)
myGetRawStr :: Get String
myGetRawStr = do
    length <- getWord32

    chars <- replicateM (fromIntegral length :: Int) get
    return (init chars)

-- Write a dictionary tuple (raw string and value)
myPutDictTuple :: (String, Value) -> Put
myPutDictTuple (key, value) = myPutRawStr key >> put value

-- Read a dictionary tuple (raw string and value)
myGetDictTuple :: Get (String, Value)
myGetDictTuple = do
    key <- myGetRawStr
    value <- get :: Get Value
    return (key, value)

instance Binary Value where
    put (IntValue i) = putWord32 2 >> put i
    put (StringValue s) = putWord32 3 >> myPutRawStr s
    put (ListValue items) = putWord32 6 >> putWord32 (fromIntegral (length items)) >> mapM_ put items
    put (DictValue tuples) = putWord32 7 >> putWord32 (fromIntegral (length tuples)) >> mapM_ myPutDictTuple tuples

    get = do
        t <- getWord32
        case t of
            0 -> return NoneValue
            2 -> liftM IntValue get
            3 -> do
                liftM StringValue myGetRawStr
            6 -> do
                length <- getWord32

                items <- replicateM (fromIntegral length :: Int) get
                return (ListValue items)
            7 -> do
                length <- getWord32

                tuples <- replicateM (fromIntegral length :: Int) myGetDictTuple
                return (DictValue tuples)

            otherwise -> error "unhandled Value type"


