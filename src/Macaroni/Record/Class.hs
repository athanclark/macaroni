{-# LANGUAGE
    MultiParamTypeClasses
  , FunctionalDependencies
  , FlexibleInstances
  , FlexibleContexts
  , ConstraintKinds
  , TypeFamilies
  #-}


module Macaroni.Record.Class where

import Data.Text (Text)
import Data.Aeson (ToJSON, FromJSON)
import Database.Persist.Sql (SqlBackend)
import Database.Persist.Class (PersistEntity)
import Database.KingFisher.Class (CRUDStorable, BasicCRUDStorable, GetStorable, GetAllStorable)
-- NOTE Macaroni will store the _key_ of the data view, which represents _total_
-- content of the symbol
import Control.Monad.Reader (ReaderT)


class
  ( ToJSON allRecords
  , FromJSON allRecords
  , PersistEntity allRecords
  -- , PersistEntityBackend allRecords ~ backend
  -- , PersistCore backend
  ) => MacaroniRecordSet allRecords where


class DecodeRecord allRecords record | record -> allRecords where
  decodeRecord :: allRecords -> Either Text record

-- | Identity instance -- NOTE overlapping?
instance DecodeRecord a a where
  decodeRecord = pure

class EncodeRecord allRecords record | record -> allRecords where
  encodeRecord :: record -> allRecords

instance EncodeRecord a a where
  encodeRecord = id



class
  ( BasicCRUDStorable key (BasicSetError symbol) (BasicDeleteError symbol)
      getRecord setRecord newRecord symbol (ReaderT SqlBackend IO)
  , GetStorable key (BasicSynopsis symbol) symbol (ReaderT SqlBackend IO)
  , GetAllStorable () (BasicSynopsis symbol) symbol (ReaderT SqlBackend IO)
  ) => MacaroniBasicRecord key getRecord setRecord newRecord symbol
       | getRecord -> key symbol
       , setRecord -> key symbol
       , newRecord -> key symbol where
  type BasicSynopsis symbol :: *
  type BasicSetError symbol :: *
  type BasicDeleteError symbol :: *



class
  ( CRUDStorable
    (parentKey, key) getRecord
    key newRecord
    (parentKey, key) (ParentedSetError symbol) setRecord
    (parentKey, key) (ParentedDeleteError symbol)
    symbol (ReaderT SqlBackend IO)
  , GetStorable (parentKey, key) (ParentedSynopsis symbol) symbol (ReaderT SqlBackend IO)
  , GetAllStorable parentKey (ParentedSynopsis symbol) symbol (ReaderT SqlBackend IO)
  ) => MacaroniParentedRecord parentKey key getRecord setRecord newRecord symbol
       | getRecord -> key symbol
       , setRecord -> key symbol
       , newRecord -> key symbol where
  type ParentedSynopsis symbol :: *
  type ParentedSetError symbol :: *
  type ParentedDeleteError symbol :: *
