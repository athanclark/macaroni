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
import Database.KingFisher.Class
  (GetStorable, SetStorable, GetAllStorable, NewStorable, DeleteStorable)
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
  ( GetStorable key getRecord symbol (ReaderT SqlBackend IO)
  , GetStorable key (Synopsis symbol) symbol (ReaderT SqlBackend IO)
  , GetAllStorable () (Synopsis symbol) symbol (ReaderT SqlBackend IO)
  , NewStorable key newRecord symbol (ReaderT SqlBackend IO)
  , SetStorable key (SetError symbol) setRecord symbol (ReaderT SqlBackend IO)
  , DeleteStorable key (DeleteError symbol) symbol (ReaderT SqlBackend IO)
  ) => MacaroniBasicRecord key getRecord setRecord newRecord symbol
       | getRecord -> key symbol
       , setRecord -> key symbol
       , newRecord -> key symbol where
  type Synopsis symbol :: *
  type SetError symbol :: *
  type DeleteError symbol :: *
