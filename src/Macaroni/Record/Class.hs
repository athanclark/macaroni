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
  ) => MacaroniRecordSet allRecords userId | allRecords -> userId where
  -- store

class DecodeRecord allRecords record | record -> allRecords where
  decodeRecord :: allRecords -> Either Text record

-- | Identity instance -- NOTE overlapping?
instance DecodeRecord a a where
  decodeRecord = pure

class EncodeRecord allRecords record | record -> allRecords where
  encodeRecord :: record -> allRecords

instance EncodeRecord a a where
  encodeRecord = id

{-
NOTE: Storing drafts seems tricky with these semantics:
- getRecord - retreiving a data view from _integrated_ content, per symbol
- setRecord - stores _as a revision draft_?
- newRecord - stores _as a conception draft_? <- revisions to newRecords by others?
  -- competing implementations: au natural
-}



class
  ( BasicCRUDStorable (BasicKey symbol) (BasicSetError symbol) (BasicDeleteError symbol)
      (BasicGetRecord symbol)
      (BasicSetRecord symbol)
      (BasicNewRecord symbol)
      symbol (ReaderT SqlBackend IO)
  , GetStorable (BasicKey symbol) (BasicSynopsis symbol) symbol (ReaderT SqlBackend IO)
  , GetAllStorable () (BasicSynopsis symbol) symbol (ReaderT SqlBackend IO)
  ) => MacaroniBasicRecord symbol where
  type BasicKey symbol :: *
  type BasicGetRecord symbol :: *
  type BasicSetRecord symbol :: *
  type BasicNewRecord symbol :: *
  type BasicSynopsis symbol :: *
  type BasicSetError symbol :: *
  type BasicDeleteError symbol :: *



class
  ( CRUDStorable
    (ParentedParentKey symbol, ParentedKey symbol) (ParentedGetRecord symbol)
    (ParentedKey symbol) (ParentedNewRecord symbol)
    (ParentedParentKey symbol, ParentedKey symbol) (ParentedSetError symbol) (ParentedSetRecord symbol)
    (ParentedParentKey symbol, ParentedKey symbol) (ParentedDeleteError symbol)
    symbol (ReaderT SqlBackend IO)
  , GetStorable (ParentedParentKey symbol, ParentedKey symbol) (ParentedSynopsis symbol) symbol (ReaderT SqlBackend IO)
  , GetAllStorable (ParentedParentKey symbol) (ParentedSynopsis symbol) symbol (ReaderT SqlBackend IO)
  ) => MacaroniParentedRecord symbol where
  type ParentedParentKey symbol :: *
  type ParentedKey symbol :: *
  type ParentedGetRecord symbol :: *
  type ParentedSetRecord symbol :: *
  type ParentedNewRecord symbol :: *
  type ParentedSynopsis symbol :: *
  type ParentedSetError symbol :: *
  type ParentedDeleteError symbol :: *
