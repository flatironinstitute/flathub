{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Field
  ( DynamicPathComponent(..)
  , DynamicPath
  , Attachment(..)
  , FieldFlag(..)
  , FieldSub(..)
  , fieldDisp
  , Field, FieldGroup
  , Fields, FieldGroups
  , parseFieldGroup
  , expandField, expandFields, expandAllFields
  , deleteField
  , fieldsDepth
  , FieldValue
  , parseFieldValue
  , fieldJValue, fieldJValues
  , idField
  , fieldsCSV
  , numpyFieldSize
  , numpyDtype
  ) where

import           Control.Applicative (Alternative, empty, (<|>))
import           Control.Monad (guard, join, when, msum)
import qualified Data.Aeson as J
import qualified Data.Aeson.Types as J
import qualified Data.ByteString as BS
import qualified Data.ByteString.Builder as B
import           Data.Char (isAlphaNum)
import           Data.Default (Default(def))
import           Data.Foldable (fold)
import           Data.Functor.Identity (Identity)
import qualified Data.HashMap.Strict as HM
import           Data.Maybe (isJust, maybeToList)
import           Data.Proxy (Proxy(Proxy))
import           Data.Scientific (Scientific)
import           Data.Semigroup (Max(getMax))
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V

import Monoid
import Output.CSV (csvTextRow)
import Type

data FieldFlag
  = FieldHidden
  | FieldNormal
  | FieldTop
  | FieldRequired
  deriving (Eq, Ord, Enum, Show)

instance J.ToJSON FieldFlag where
  toJSON FieldRequired = J.String "required"
  toJSON FieldTop = J.String "top"
  toJSON FieldNormal = J.Null
  toJSON FieldHidden = J.String "hidden"

instance J.FromJSON FieldFlag where
  parseJSON (J.String "required") = return FieldRequired
  parseJSON (J.String "top") = return FieldTop
  parseJSON (J.String "hidden") = return FieldHidden
  parseJSON (J.String "disp") = return FieldNormal
  parseJSON J.Null = return FieldNormal
  parseJSON j = J.typeMismatch "FieldFlag" j

data DynamicPathComponent
  = DynamicPathLiteral FilePath
  | DynamicPathField T.Text
  | DynamicPathSubstitute T.Text T.Text T.Text
  deriving (Show, Eq)

-- |a path that can be constructed from a data row by filling in field values
type DynamicPath = [DynamicPathComponent]

instance J.FromJSON DynamicPathComponent where
  parseJSON (J.String s) = return $ DynamicPathLiteral $ T.unpack s
  parseJSON j = J.withObject "dynamic path" po j where
    po o = msum
      [ DynamicPathSubstitute <$> o J..: "field" <*> o J..: "find" <*> o J..: "replace"
      , DynamicPathField <$> o J..: "field"
      , DynamicPathLiteral <$> o J..: "text"
      ]
  parseJSONList (J.String s) = return $ pl s where
    pl (T.break ('$'==) -> (a, v)) =
      DynamicPathLiteral (T.unpack a) : maybe [] (pv . snd) (T.uncons v)
    pv t = case T.uncons t of
      Nothing -> [DynamicPathLiteral "$"]
      Just ('{',T.break ('}'==) -> (n,T.uncons -> Just ('}',l))) ->
        (case T.break ('/'==) n of
          (n',T.uncons -> Just ('/', T.break ('/'==) -> (a, T.uncons -> Just ('/',b)))) -> DynamicPathSubstitute n' a b
          _ -> DynamicPathField n) : pl l
      _ -> DynamicPathField n : pl l where (n, l) = T.span (\c -> isAlphaNum c || c == '_') t
  parseJSONList (J.Array a) = mapM J.parseJSON $ V.toList a
  parseJSONList j = J.typeMismatch "dynamic path" j

data Attachment = Attachment
  { attachmentPath, attachmentName :: DynamicPath }
  deriving (Show, Eq)

instance J.FromJSON Attachment where
  parseJSON (J.Object o) = Attachment
    <$> o J..: "path"
    <*> o J..: "name"
  parseJSON j@(J.String s) = Attachment
    <$> J.parseJSON j
    <*> J.parseJSON (J.String $ snd $ T.breakOnEnd "/" s)
  -- parseJSON j@(J.Array a) = TODO
  parseJSON j = J.typeMismatch "attachment" j

data FieldSub t m = Field
  { fieldName :: T.Text
  , fieldType :: TypeValue t
  , fieldEnum :: Maybe (V.Vector T.Text)
  , fieldTitle :: T.Text
  , fieldDescr :: Maybe T.Text
  , fieldUnits :: Maybe T.Text
  , fieldFlag :: FieldFlag
  , fieldTerms :: Bool
  , fieldSub :: m (FieldsSub t m)
  , fieldDict :: Maybe T.Text
  , fieldScale :: Maybe Scientific -- ^scale factor, to display scale*x instead
  , fieldReversed :: Bool -- ^reverse axis on plotting
  , fieldWildcard :: Bool -- ^allow wildcard text filter
  , fieldSize :: Word -- ^string length
  , fieldIngest :: Maybe T.Text
  , fieldMissing :: [BS.ByteString]
  , fieldAttachment :: Maybe Attachment
  }

fieldDisp :: FieldSub t m -> Bool
fieldDisp = (FieldHidden <) . fieldFlag

type FieldGroup = FieldSub Proxy Maybe
type Field = FieldSub Proxy Proxy
type FieldValue = FieldSub Identity Proxy

type FieldsSub t m = V.Vector (FieldSub t m)
type FieldGroups = FieldsSub Proxy Maybe
type Fields = [Field]

instance Alternative m => Default (FieldSub Proxy m) where
  def = Field
    { fieldName = T.empty
    , fieldType = def
    , fieldEnum = Nothing
    , fieldTitle = T.empty
    , fieldDescr = Nothing
    , fieldUnits = Nothing
    , fieldFlag = FieldNormal
    , fieldTerms = False
    , fieldSub = empty
    , fieldDict = Nothing
    , fieldScale = Nothing
    , fieldReversed = False
    , fieldIngest = Nothing
    , fieldMissing = []
    , fieldAttachment = Nothing
    , fieldWildcard = False
    , fieldSize = 8
    }

numpyFieldSize :: Field -> Word
numpyFieldSize Field{ fieldType = Keyword _, fieldSize = n } = n
numpyFieldSize Field{ fieldType = t } = numpyTypeSize t

numpyDtype :: Field -> String
numpyDtype Field{ fieldType = Boolean _ } = "?"
numpyDtype Field{ fieldType = ULong _ } = "<u8"
numpyDtype f = '<' : baseType ('f','i','?','S','V') (fieldType f) : show (numpyFieldSize f)

instance J.ToJSON Field where
  toJSON f@Field{..} = J.object $
    [ "name" J..= fieldName
    , "type" J..= fieldType
    , "title" J..= fieldTitle
    , "disp" J..= (fieldFlag > FieldHidden)
    , "base" J..= baseType ('f','i','b','s','v') fieldType
    , "dtype" J..= numpyDtype f
    ] ++ concatMap maybeToList
    [ ("enum" J..=) <$> fieldEnum
    , ("descr" J..=) <$> fieldDescr
    , ("units" J..=) <$> fieldUnits
    , ("flag" J..=) <$> case fieldFlag of
        FieldTop -> Just False
        FieldRequired -> Just True
        _ -> Nothing
    , ("terms" J..= fieldTerms) <$ guard fieldTerms
    , ("wildcard" J..= fieldWildcard) <$ guard fieldWildcard
    , ("dict" J..=) <$> fieldDict
    , ("scale" J..=) <$> fieldScale
    , ("reversed" J..= fieldReversed) <$ guard fieldReversed
    , ("attachment" J..= True) <$ fieldAttachment
    ]

parseFieldGroup :: HM.HashMap T.Text FieldGroup -> J.Value -> J.Parser FieldGroup
parseFieldGroup dict = parseFieldDefs def where
  parseFieldDefs :: FieldGroup -> J.Value -> J.Parser FieldGroup
  parseFieldDefs defd = J.withObject "field" $ \f -> do
    fieldDict <- f J..:? "dict"
    d <- maybe (return defd)
      (\n -> maybe (fail $ "Unknown dict key: " ++ show n) return $ HM.lookup n dict)
      fieldDict
    fieldName <- f J..:? "name" J..!= fieldName d
    when (T.any ('.' ==) fieldName) $ fail $ "Invalid field name: " ++ show fieldName
    fieldType <- f J..:! "type" J..!= fieldType d
    fieldEnum <- maybe (fieldEnum d <|> V.fromList ["false","true"] <$ guard (typeIsBoolean fieldType)) join
      <$> f J..:! "enum"
    fieldTitle <- f J..:! "title" J..!= if T.null (fieldTitle d) then fieldName else fieldTitle d
    fieldDescr <- (<|> fieldDescr d) <$> f J..:? "descr"
    fieldUnits <- (<|> fieldUnits d) <$> f J..:? "units"
    fieldFlag <- f J..:? "flag" J..!= fieldFlag d
    fieldScale <- f J..:! "scale"
    fieldReversed <- f J..:? "reversed" J..!= fieldReversed d
    fieldIngest <- f J..:! "ingest"
    fieldMissing <- map TE.encodeUtf8 <$> case HM.lookup "missing" f of
      Nothing -> return []
      Just J.Null -> return []
      Just (J.String s) -> return [s]
      Just (J.Array l) -> mapM J.parseJSON $ V.toList l
      Just j -> J.typeMismatch "missing string" j
    fieldAttachment <- f J..:! "attachment"
    fieldTerms <- f J..:? "terms" J..!= (isJust fieldEnum || typeIsString fieldType)
    fieldWildcard <- f J..:? "wildcard" J..!= fieldWildcard d
    fieldSize <- f J..:? "size" J..!= fieldSize d
    fieldSub <- (<|> fieldSub d) <$> J.explicitParseFieldMaybe' (J.withArray "subfields" $ V.mapM $
        parseFieldDefs defd
          { fieldType = fieldType
          , fieldEnum = fieldEnum
          , fieldFlag = fieldFlag
          , fieldMissing = fieldMissing
          })
      f "sub"
    return Field{..}

instance J.FromJSON FieldGroup where
  parseJSON = parseFieldGroup mempty

instance Semigroup (FieldSub t m) where
  (<>) = subField

instance Alternative m => Monoid (FieldSub Proxy m) where
  mempty = def
  mappend = subField

subField :: FieldSub s n -> FieldSub t m -> FieldSub t m
subField f s = s
  { fieldName = merge '_' (fieldName f) (fieldName s)
  , fieldTitle = merge ' ' (fieldTitle f) (fieldTitle s)
  , fieldDescr = joinMaybeWith (\x -> (x <>) . T.cons '\n') (fieldDescr f) (fieldDescr s)
  , fieldUnits = fieldUnits s <|> fieldUnits f
  } where
  merge c a b
    | T.null a = b
    | T.null b = a
    | a == b = b
    | otherwise = a <> T.cons c b

expandField :: FieldGroup -> Fields
expandField f@Field{ fieldSub = Nothing } = return f{ fieldSub = Proxy }
expandField f@Field{ fieldSub = Just l } =
  foldMap (expandField . mappend f) l

expandFields :: FieldGroups -> Fields
expandFields = foldMap expandField

expandAllFields :: FieldGroups -> HM.HashMap T.Text FieldGroup
expandAllFields = foldMap expandAllField where
  expandAllField :: FieldGroup -> HM.HashMap T.Text FieldGroup
  expandAllField f = HM.singleton (fieldName f) f <> foldMap (foldMap (expandAllField . mappend f)) (fieldSub f)

deleteField :: T.Text -> FieldGroups -> FieldGroups
deleteField n = dfs mempty where
  df p f@Field{ fieldSub = Nothing }
    | n == fieldName (p <> f) = Nothing
    | otherwise = Just f
  df p f@Field{ fieldSub = Just l } = Just f{ fieldSub = Just $ dfs (p <> f) l }
  dfs = V.mapMaybe . df

fieldsDepth :: FieldGroups -> Word
fieldsDepth = getMax . depth where
  depth = succ . foldMap (foldMap depth . fieldSub)

parseFieldValue :: Field -> T.Text -> Maybe FieldValue
parseFieldValue f = fmap sv . pv f where
  pv Field{ fieldType = (Byte _), fieldEnum = Just l } s | Just i <- V.elemIndex s l = Just $ Byte $ fromIntegral i
  pv Field{ fieldType = t } s = sequenceTypeValue $ parseTypeValue t s
  sv v = f
    { fieldType = v
    , fieldSub = Proxy
    }

fieldJValue :: FieldValue -> J.Series
fieldJValue f = fieldName f J..= fieldType f

fieldJValues :: [FieldValue] -> J.Series
fieldJValues = foldMap fieldJValue

-- |pseudo field representing ES _id
idField :: Field
idField = def{ fieldName = "_id", fieldType = Keyword Proxy, fieldTitle = "_id", fieldFlag = FieldHidden }

fieldsCSV :: Fields -> B.Builder
fieldsCSV l = csvTextRow ["variable", "name", "type", "units", "description", "values","dict","scale"] <> foldMap fieldCSV l where
  fieldCSV :: Field -> B.Builder
  fieldCSV Field{..} = csvTextRow [fieldName, fieldTitle, T.pack $ show fieldType, fold fieldUnits, fold fieldDescr, foldMap (T.intercalate "," . V.toList) fieldEnum, fold fieldDict, foldMap (T.pack . show) fieldScale]
