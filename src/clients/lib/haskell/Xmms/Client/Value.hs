--  XMMS2 - X Music Multiplexer System
--  Copyright (C) 2003-2009 XMMS2 Team
--
--  PLUGINS ARE NOT CONSIDERED TO BE DERIVED WORK !!!
--
--  This library is free software; you can redistribute it and/or
--  modify it under the terms of the GNU Lesser General Public
--  License as published by the Free Software Foundation; either
--  version 2.1 of the License, or (at your option) any later version.
--
--  This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
--  Lesser General Public License for more details.

module Xmms.Client.Value (
      Value(..)
) where

import Control.Monad (replicateM, liftM, liftM2)
import Data.Binary
import Data.Word
import Data.Int

import Xmms.Client.Collection
 
data Value = NoneValue
           | IntValue Int32
           | StringValue String
           | ListValue [Value]
           | DictValue [(String, Value)]
           | CollValue Collection
           | BinValue [Word8]
           deriving (Show)

putWord32 :: Word32 -> Put
putWord32 = put

getWord32 :: Get Word32
getWord32 = get

-- Write a raw string (ie without the type tag)
myPutRawStr :: String -> Put
myPutRawStr s =
       putWord32 (fromIntegral (succ (length s)))
    >> mapM_ put s
    >> put '\0'

-- Read a raw string (ie without the type tag)
myGetRawStr :: Get String
myGetRawStr = liftM init (getWord32 >>= flip replicateM get . fromIntegral)

-- Write a dictionary tuple (raw string and value)
myPutDictTuple :: (String, Value) -> Put
myPutDictTuple (key, value) = myPutRawStr key >> put value

-- Read a dictionary tuple (raw string and value)
myGetDictTuple :: Get (String, Value)
myGetDictTuple = liftM2 (,) myGetRawStr get

-- Write a string dictionary tuple (raw string key and raw string value)
myPutStringDictTuple :: (String, String) -> Put
myPutStringDictTuple (key, value) =
       myPutRawStr key
    >> myPutRawStr value

-- Read a string dictionary tuple (raw string key and raw string value)
myGetStringDictTuple :: Get (String, String)
myGetStringDictTuple = liftM2 (,) myGetRawStr myGetRawStr

putInt i =
       putWord32 2
    >> put i

putString s =
       putWord32 3
    >> myPutRawStr s

-- Write a collection's operands (unless it's a reference).
putCollectionOperands :: Collection -> Put
putCollectionOperands (Collection Reference _ _ _) = putWord32 0
putCollectionOperands (Collection _ _ _ operands) =
       putWord32 (fromIntegral (length operands))
    >> mapM_ putCollection operands

-- Write a collection.
putCollection :: Collection -> Put
putCollection c =
       putWord32 4
    >> putWord32 (fromIntegral (fromEnum ttype))

    >> putWord32 (fromIntegral (length attributes))
    >> mapM_ myPutStringDictTuple attributes

    >> putWord32 (fromIntegral (length idlist))
    >> mapM_ putWord32 idlist

    >> putCollectionOperands c
    where ttype = collectionType c
          attributes = collectionAttributes c
          idlist = collectionIdlist c

unpackColl :: Value -> Collection
unpackColl (CollValue col) = col

-- Read a collection.
getCollection :: Get Value
getCollection = do
    ttype <- getWord32
    length <- getWord32
    attributes <- replicateM (fromIntegral length) myGetStringDictTuple
    length <- getWord32
    idlist <- replicateM (fromIntegral length) getWord32
    length <- getWord32
    operands <- replicateM (fromIntegral length) get

    return (CollValue (Collection (toEnum (fromIntegral ttype)) attributes idlist (map unpackColl operands)))

putBinary :: [Word8] -> Put
putBinary items =
       putWord32 5
    >> putWord32 (fromIntegral (length items))
    >> mapM_ put items

putList :: [Value] -> Put
putList items =
       putWord32 6
    >> putWord32 (fromIntegral (length items))
    >> mapM_ put items

getList :: Get Value
getList = liftM ListValue (getWord32 >>= flip replicateM get . fromIntegral)

putDictionary :: [(String, Value)] -> Put
putDictionary tuples =
       putWord32 7
    >> putWord32 (fromIntegral (length tuples))
    >> mapM_ myPutDictTuple tuples

getDictionary :: Get Value
getDictionary = liftM DictValue (getWord32 >>= flip replicateM myGetDictTuple . fromIntegral)

instance Binary Value where
    put (IntValue i) = putInt i
    put (StringValue s) = putString s
    put (CollValue c) = putCollection c
    put (BinValue b) = putBinary b
    put (ListValue items) = putList items
    put (DictValue tuples) = putDictionary tuples

    get = do
        t <- getWord32
        case t of
            0 -> return NoneValue
            2 -> liftM IntValue get
            3 -> liftM StringValue myGetRawStr
            4 -> getCollection
            6 -> getList
            7 -> getDictionary
            otherwise -> error "unhandled Value type"
