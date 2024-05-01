{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DefaultSignatures #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RoleAnnotations #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}

module Database.Oracle.Simple.FromRow where

import Control.Exception hiding (TypeError)
import Control.Monad (unless, when)
import Control.Monad.IO.Class (liftIO)
import Control.Monad.Reader (ReaderT, asks)
import Data.Coerce
import Data.Functor.Identity (Identity)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Proxy (Proxy(..))
import Data.Word (Word32)
import Foreign.C.String (peekCStringLen)
import Foreign.C.Types (CInt(..))
import Foreign.Marshal (alloca)
import Foreign.Ptr (Ptr)
import Foreign.Storable (peek)
import GHC.Generics
import GHC.TypeLits

import Database.Oracle.Simple.FromField
import Database.Oracle.Simple.Internal

class FromRow a where
  fromRow :: RowParser a
  default fromRow :: (GFromRow (Rep a), Generic a) => RowParser a
  fromRow = to <$> gFromRow

instance FromField a => FromRow (Only a)

instance FromField a => FromRow (Identity a)

instance (FromField a, FromField b) => FromRow (a, b)

instance (FromField a, FromField b, FromField c) => FromRow (a, b, c)

instance (FromField a, FromField b, FromField c, FromField d) => FromRow (a, b, c, d)

instance (FromField a, FromField b, FromField c, FromField d, FromField e) => FromRow (a, b, c, d, e)

instance (FromField a, FromField b, FromField c, FromField d, FromField e, FromField f) => FromRow (a, b, c, d, e, f)

instance
  (FromField a, FromField b, FromField c, FromField d, FromField e, FromField f, FromField g)
  => FromRow (a, b, c, d, e, f, g)

instance
  (FromField a, FromField b, FromField c, FromField d, FromField e, FromField f, FromField g, FromField h)
  => FromRow (a, b, c, d, e, f, g, h)

instance
  (FromField a, FromField b, FromField c, FromField d, FromField e, FromField f, FromField g, FromField h, FromField i)
  => FromRow (a, b, c, d, e, f, g, h, i)

instance
  ( FromField a
  , FromField b
  , FromField c
  , FromField d
  , FromField e
  , FromField f
  , FromField g
  , FromField h
  , FromField i
  , FromField j
  )
  => FromRow (a, b, c, d, e, f, g, h, i, j)

class GFromRow f where
  gFromRow :: RowParser (f a)

instance (GFromRow m) => GFromRow (D1 i m) where
  gFromRow = M1 <$> gFromRow

instance (GFromRow m) => GFromRow (C1 i m) where
  gFromRow = M1 <$> gFromRow

instance (GFromRow m) => GFromRow (S1 i m) where
  gFromRow = M1 <$> gFromRow

instance (GFromRow l, GFromRow r) => GFromRow (l :*: r) where
  gFromRow = (:*:) <$> gFromRow <*> gFromRow

instance (() ~ TypeError ('Text "Sum types not supported")) => GFromRow (l :+: r) where
  gFromRow = error "Sum types not supported"

instance (FromField a) => GFromRow (K1 i a) where
  gFromRow = K1 <$> readField

-- We are intentionally not using StateT because we want to keep
-- the role of @a@ as "representational".
newtype RowParser a = RowParser {runRowParser :: DPIStmt -> Word32 -> IO (a, Word32) }

instance Functor RowParser where
  fmap f g = RowParser $ \stmt st -> do
    (a, st') <- runRowParser g stmt st
    pure (f a, st')

instance Applicative RowParser where
  pure a = RowParser $ \_ s -> pure (a, s)
  fn <*> g = RowParser $ \dpiStmt st -> do
    (f, st') <- runRowParser fn dpiStmt st
    (a, st'') <- runRowParser g dpiStmt st'
    pure (f a, st'')

instance Monad RowParser where
  return = pure
  f >>= g = RowParser $ \dpiStmt s -> do
    (f', s') <- runRowParser f dpiStmt s
    runRowParser (g f') dpiStmt s'

-- | Retrieve the currently fetched row.
getRow :: forall a. (FromRow a) => DPIStmt -> IO a
getRow stmt = fst <$> runRowParser fromRow stmt 0

-- | Derive a @RowParser@ for a field at the specified column position.
readField :: (FromField a) => RowParser a
readField = fieldWith fromField

-- | Derive a 'RowParser' for a field at the specified column position
-- using the supplied 'FieldParser'.
fieldWith :: forall a. (FromField a) => FieldParser a -> RowParser a
fieldWith FieldParser{..} = RowParser $ \dpiStmt c -> do
  let pos = c + 1
  liftIO $ do
    (gotType, dataBuf) <- getQueryValue dpiStmt (fromIntegral pos)
    let typ = fromDPINativeType (Proxy @a)
    unless (gotType == typ) $
      throwIO $
        TypeMismatch typ gotType (Column pos)
    (,) <$> readDPIDataBuffer dataBuf <*> pure pos

data RowParseError
  = TypeMismatchError TypeMismatch
  | MissingFieldError MissingField
  deriving anyclass (Exception)
  deriving stock (Show)

-- | We encountered a type that we were not expecting.
data TypeMismatch
  = TypeMismatch
  { expectedType :: DPINativeType
  -- ^ The DPI native type we were expecting
  , gotType :: DPINativeType
  -- ^ The DPI native type we got
  , column :: Column
  -- ^ Column position where type mismatch was encountered (1-indexed)
  }
  deriving (Show)

instance Exception TypeMismatch where
  displayException (TypeMismatch{..}) =
    "Row parse error due to type mismatch: At column "
      <> show column
      <> ", expected "
      <> show expectedType
      <> " but got "
      <> show gotType
      <> "."

-- | We encountered a missing column referenced by name
data MissingField
  = MissingField
  { expectedFieldType :: DPINativeType
  -- ^ The DPI native type we were expecting
  , fieldName :: String
  -- ^ The name of the record field missing from the result set.
  }
  deriving (Show)

instance Exception MissingField where
  displayException (MissingField{..}) =
    "Row parse error due to missing field: \""
      <> fieldName
      <> "\" not found in the row columns."

type ColumnMap = Map String Column

newtype ParseByName a = ParseByName { unParseByName :: a }

instance (Generic a, GFromNamedRow (Rep a)) => FromRow (ParseByName a) where
  fromRow = RowParser $ \stmt col -> do
    tbl <- buildLookupTable stmt
    runRowParser (ParseByName . to <$> gFromNamedRow tbl) stmt col

class GFromNamedRow f where
  gFromNamedRow :: ColumnMap -> RowParser (f a)

instance (GFromNamedRow m) => GFromNamedRow (D1 i m) where
  gFromNamedRow cm = M1 <$> gFromNamedRow cm

instance (GFromNamedRow m) => GFromNamedRow (C1 i m) where
  gFromNamedRow cm = M1 <$> gFromNamedRow cm

instance (GFromNamedRow l, GFromNamedRow r) => GFromNamedRow (l :*: r) where
  gFromNamedRow cm = (:*:) <$> gFromNamedRow cm <*> gFromNamedRow cm

instance (() ~ TypeError ('Text "Sum types not supported")) => GFromNamedRow (l :+: r) where
  gFromNamedRow _ = error "Sum types not supported"

instance (Selector s, FromField a) => GFromNamedRow (S1 s (K1 i a)) where
  gFromNamedRow cm = M1 . K1 <$> fieldNameWith cm (selName (undefined :: M1 _i s _f _p)) (fromField @a)

buildLookupTable :: DPIStmt -> IO (Map String Column)
buildLookupTable stmt = do
  maxColumn <- getColumnCount stmt
  let
    mkNameEntry columnNumber = do
      name <- getColumnName stmt columnNumber
      pure (name, columnNumber)
  M.fromList <$> traverse mkNameEntry [Column 1 .. Column maxColumn]

getColumnName :: DPIStmt -> Column -> IO String
getColumnName st (Column c) = do
  DPIQueryInfo n nl <- getQueryInfo st (fromIntegral c)
  peekCStringLen (n, fromIntegral nl)

fieldNameWith :: forall a. (FromField a) => ColumnMap -> String -> FieldParser a -> RowParser a
fieldNameWith tbl name FieldParser{..} = RowParser $ \dpiStmt st -> do
  let typ = fromDPINativeType (Proxy @a)
  case M.lookup name tbl of
    Nothing -> throwIO $ MissingField typ name
    Just (Column pos) -> do
      (gotType, dataBuf) <- getQueryValue dpiStmt (fromIntegral pos)
      unless (gotType == typ) $
        throwIO $ TypeMismatch typ gotType (Column pos)
      (,) <$> readDPIDataBuffer dataBuf <*> pure st
