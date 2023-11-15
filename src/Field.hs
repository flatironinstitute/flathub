{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Field
  ( DynamicPathComponent(..)
  , DynamicPath
  , Attachment(..)
  , FieldFlag(..)
  , Field(..)
  , fieldDisp
  , Fields
  , expandField, expandFields, expandAllFields
  , deleteField
  , fieldsDepth
  , parseField
  , FieldTypeValue
  , fieldDesc
  , fieldValue
  , pattern FieldValue
  , fieldValueProxy
  , FieldValue
  , setFieldValue
  , setFieldValueUnsafe
  , makeFieldValueM
  , makeFieldValue
  , parseValueForField
  , parseFieldValue
  , fieldJValue, fieldJValues
  , idField
  , docField
  , numpyFieldSize
  , numpyDtype
  , Count
  , FieldStats(..)
  ) where

import           Control.Applicative ((<|>))
import           Control.Arrow (first)
import           Control.Monad (guard, join, when, msum, mfilter)
import qualified Data.Aeson as J
import qualified Data.Aeson.Encoding as JE
import qualified Data.Aeson.Types as J
import qualified Data.Aeson.Key as JK
import qualified Data.Aeson.KeyMap as JM
import qualified Data.ByteString as BS
import           Data.Char (isAlphaNum)
import           Data.Default (Default(def))
import           Data.Functor.Identity (Identity(Identity))
import qualified Data.HashMap.Strict as HM
import           Data.Maybe (isJust, maybeToList)
import           Data.Proxy (Proxy(Proxy))
import           Data.Scientific (Scientific)
import           Data.Semigroup (Max(getMax))
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Vector as V

import Monoid
import Type
import qualified KeyedMap as KM

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

data Field = Field
  { fieldName :: T.Text
  , fieldType :: Type
  , fieldTitle :: T.Text
  , fieldDescr :: Maybe T.Text
  , fieldUnits :: Maybe T.Text
  , fieldFlag :: FieldFlag
  , fieldStore :: Maybe Bool -- ^true: store only, false: index only, Nothing: index+store (store = and, index = not or)
  , fieldEnum :: Maybe (V.Vector T.Text) -- ^enumeration values (for integral fields)
  , fieldTerms :: Bool -- ^treat as catagorical (implied if enum or string)
  , fieldDict :: Maybe T.Text -- ^link to field dictionary
  , fieldScale :: Maybe Scientific -- ^scale factor, to display scale*x instead
  , fieldReversed :: Bool -- ^reverse axis on plotting
  , fieldWildcard :: Bool -- ^allow wildcard text filter
  , fieldSize :: Word -- ^string length (maximum)
  , fieldLength :: Word -- ^array length (maximum)
  , fieldIngest :: Maybe T.Text -- ^special handling on ingest (interpretation depends on format)
  , fieldMissing :: [BS.ByteString] -- ^values to treat as missing on ingest
  , fieldAttachment :: Maybe Attachment
  , fieldCondition :: [(T.Text, J.Value)]
  , fieldStats :: Maybe (TypeValue FieldStats)
  , fieldDefault :: Maybe Value
  , fieldSub :: Maybe Fields
  }

fieldDisp :: Field -> Bool
fieldDisp = (FieldHidden <) . fieldFlag

type Fields = V.Vector Field

instance KM.Keyed Field where
  type Key Field = T.Text
  key = fieldName

instance Default Field where
  def = Field
    { fieldName = T.empty
    , fieldType = def
    , fieldEnum = Nothing
    , fieldTitle = T.empty
    , fieldDescr = Nothing
    , fieldUnits = Nothing
    , fieldFlag = FieldNormal
    , fieldStore = Just False
    , fieldTerms = False
    , fieldSub = Nothing
    , fieldDict = Nothing
    , fieldScale = Nothing
    , fieldReversed = False
    , fieldIngest = Nothing
    , fieldMissing = []
    , fieldAttachment = Nothing
    , fieldWildcard = False
    , fieldSize = 8
    , fieldLength = 1
    , fieldCondition = []
    , fieldStats = Nothing
    , fieldDefault = Nothing
    }

numpyFieldSize :: Field -> Word
numpyFieldSize f@Field{ fieldType = Keyword _ } = fieldSize f
numpyFieldSize Field{ fieldType = t } = numpyTypeSize t

numpyDtype :: Field -> String
numpyDtype Field{ fieldType = Boolean _ } = "?"
numpyDtype f@Field{ fieldType = ULong _ } = 'u' : show (numpyFieldSize f)
numpyDtype f = baseType ('f','i','?','S','V') (fieldType f) : show (numpyFieldSize f)

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
    , ("condition" J..= fieldCondition) <$ guard (not $ null fieldCondition)
    , ("store" J..= fieldStore) <$ guard (or fieldStore)
    ]

parseField :: HM.HashMap T.Text Field -> J.Object -> J.Value -> J.Parser Field
parseField dict stats = parseFieldDefs def where
  parseFieldDefs :: Field -> J.Value -> J.Parser Field
  parseFieldDefs pf = J.withObject "field" $ \f -> do
    fieldDict <- f J..:? "dict"
    df <- mapM
      (\n -> maybe (fail $ "Unknown dict key: " ++ show n) return $ HM.lookup n dict)
      fieldDict
    let dv :: J.FromJSON a => (Field -> a) -> JK.Key -> J.Parser a
        dv a n = f J..:! n J..!= maybe (a pf) a df
        mvd :: J.FromJSON a => (Field -> Maybe a) -> JK.Key -> Maybe a -> J.Parser (Maybe a)
        mvd a n d = maybe ((a =<< df) <|> a pf <|> d) join <$> f J..:! n
        mv a n = mvd a n Nothing
    name <- f J..: "name"
    when (T.any ('.' ==) name) $ fail $ "Invalid field name: " ++ show name
    fieldName <- return $ joinWith '_' (fieldName pf) name
    fieldType <- dv fieldType "type"
    fieldEnum <- mvd fieldEnum "enum" $ guarded (typeIsBoolean fieldType) $ V.fromList ["false","true"]
    fieldTitle <- f J..:! "title" J..!= maybe name fieldTitle df
    fieldDescr <- mv fieldDescr "descr"
    fieldUnits <- mv fieldUnits "units"
    fieldStore <- dv fieldStore "store"
    fieldFlag <- dv fieldFlag "flag"
    fieldScale <- f J..:? "scale"
    fieldReversed <- dv fieldReversed "reversed"
    fieldIngest <- f J..:? "ingest"
    fieldMissing <- case JM.lookup "missing" f of
      Nothing -> return $ fieldMissing pf
      Just J.Null -> return []
      Just (J.String s) -> return $ [TE.encodeUtf8 s]
      Just (J.Array l) -> mapM (fmap TE.encodeUtf8 . J.parseJSON) $ V.toList l
      Just j -> J.typeMismatch "missing string" j
    fieldAttachment <- f J..:? "attachment"
    fieldTerms <- f J..:! "terms" J..!= (isJust fieldEnum || typeIsString fieldType)
    fieldWildcard <- dv fieldWildcard "wildcard"
    fieldSize <- dv fieldSize "size"
    fieldLength <- dv fieldLength "length"
    fieldCondition <- map (first JK.toText) . JM.toList <$> f J..:? "condition" J..!= JM.empty
    fieldStats <- traverse (\j -> traverseTypeValue (\Proxy -> J.parseJSON1 j) fieldType)
      $ JM.lookup (JK.fromText fieldName) stats 
    fieldDefault <- J.explicitParseFieldMaybe (\j -> traverseTypeValue (\Proxy -> J.parseJSON1 j) fieldType) f "default"
    let rf = Field{ fieldSub = Nothing, .. }
    fieldSub <- J.explicitParseFieldMaybe' (J.withArray "subfields" $ V.mapM $
        parseFieldDefs rf -- don't inherit title/descr (see subField)
          { fieldTitle = T.empty
          , fieldDescr = Nothing
          })
      f "sub"
    return rf{ fieldSub = fieldSub }

instance J.FromJSON Field where
  parseJSON = parseField mempty mempty

subField :: Field -> Field -> Field
subField f s = s
  { fieldTitle = joinWith ' ' (fieldTitle f) (fieldTitle s)
  , fieldDescr = joinMaybeWith (\x -> (x <>) . T.cons '\n') (fieldDescr f) (fieldDescr s)
  }

joinWith :: Char -> T.Text -> T.Text -> T.Text
joinWith c a b
  | T.null a = b
  | T.null b = a
  | a == b = b
  | otherwise = a <> T.cons c b

expandField :: Field -> V.Vector Field
expandField f@Field{ fieldSub = Nothing } = V.singleton f
expandField f@Field{ fieldSub = Just l } =
  foldMap (expandField . subField f) l

expandFields :: Fields -> Fields
expandFields = V.concatMap expandField

-- |Like 'expandFields' but keep parent fields
expandAllFields :: Fields -> HM.HashMap T.Text Field
expandAllFields = foldMap expandAllField where
  expandAllField :: Field -> HM.HashMap T.Text Field
  expandAllField f = HM.singleton (fieldName f) f <> foldMap (foldMap (expandAllField . subField f)) (fieldSub f)

filterFields :: (Field -> Bool) -> Fields -> Fields
filterFields n = dfs where
  df f@Field{ fieldSub = Nothing } = mfilter n $ Just f
  df f@Field{ fieldSub = Just l } = Just f{ fieldSub = Just $ dfs l }
  dfs = V.mapMaybe df

deleteField :: T.Text -> Fields -> Fields
deleteField n = filterFields ((n /=) . fieldName)

fieldsDepth :: Fields -> Word
fieldsDepth = getMax . depth where
  depth = succ . foldMap (foldMap depth . fieldSub)

-- |pseudo fields representing ES _id, _doc
idField, docField :: Field
idField = def
  { fieldName = "_id"
  , fieldTitle = "_id"
  , fieldDescr = Just "Implicit unique internal document id for row"
  , fieldFlag = FieldHidden
  , fieldType = Keyword Proxy }
docField = def
  { fieldName = "_doc"
  , fieldTitle = "_doc"
  , fieldDescr = Just "Implicit pseudo-field used only for sorting documents by internal order"
  , fieldFlag = FieldHidden
  , fieldType = Void Proxy }

data FieldTypeValue t = FieldValue
  { fieldDesc :: Field
  , fieldValue :: TypeValue t -- ^must have same type as fieldType
  }

type FieldValue = FieldTypeValue Identity

instance KM.Keyed (FieldTypeValue t) where
  type Key (FieldTypeValue t) = T.Text
  key = fieldName . fieldDesc

fieldValueProxy :: Field -> FieldTypeValue Proxy
fieldValueProxy f = FieldValue f (fieldType f)

setFieldValueUnsafe :: Field -> TypeValue t -> FieldTypeValue t
setFieldValueUnsafe = FieldValue

setFieldValue :: Functor t => Field -> TypeValue t -> FieldTypeValue t
setFieldValue f = setFieldValueUnsafe f . coerceTypeValue (fieldType f)

makeFieldValue :: Field -> (forall a . Typed a => f a) -> FieldTypeValue f
makeFieldValue f t = FieldValue f $ makeTypeValue (fieldType f) t

makeFieldValueM :: Functor m => Field -> (forall a . Typed a => m (f a)) -> m (FieldTypeValue f)
makeFieldValueM f t = FieldValue f <$> makeTypeValueM (fieldType f) t

-- not very efficient, but only used in query parsing
parseValueForField :: Typed a => Field -> BS.ByteString -> Maybe a
parseValueForField Field{ fieldEnum = Just l } s
  | Just i <- V.findIndex ((s ==) . TE.encodeUtf8) l = Just $ fromInt i
parseValueForField _ s = readValue s

parseFieldValue :: Field -> BS.ByteString -> Maybe FieldValue
parseFieldValue f s = makeFieldValueM f (Identity <$> parseValueForField f s)

fieldJValue :: J.KeyValue j => FieldValue -> j
fieldJValue (FieldValue f v) = JK.fromText (fieldName f) J..= v

fieldJValues :: [FieldValue] -> J.Series
fieldJValues = foldMap fieldJValue

type Count = Word

data FieldStats a
  = FieldStats{ statsMin, statsMax, statsAvg :: Maybe Scientific, statsCount :: Count }
  | FieldTerms{ termsBuckets :: [(a, Count)], termsCount :: Count }

instance Functor FieldStats where
  fmap _ (FieldStats n x a c) = FieldStats n x a c
  fmap f (FieldTerms b c) = FieldTerms (map (first f) b) c

instance J.ToJSON1 FieldStats where
  liftToJSON _ _ FieldStats{..} = J.object
    [ "count" J..= statsCount
    , "min" J..= statsMin
    , "max" J..= statsMax
    , "avg" J..= statsAvg
    ]
  liftToJSON tj _ FieldTerms{..} = J.object
    [ "terms" J..= map (\(v, c) -> J.object
      [ "value" J..= tj v
      , "count" J..= c
      ]) termsBuckets
    , "others" J..= termsCount
    ]
  liftToEncoding _ _ FieldStats{..} = J.pairs
    $  "count" J..= statsCount
    <> "min" J..= statsMin
    <> "max" J..= statsMax
    <> "avg" J..= statsAvg
  liftToEncoding te _ FieldTerms{..} = J.pairs
    $  "terms" `JE.pair` JE.list (\(v, c) -> J.pairs
      $  "value" `JE.pair` te v
      <> "count" J..= c
      ) termsBuckets
    <> "others" J..= termsCount

instance J.FromJSON1 FieldStats where
  liftParseJSON pj _ = J.withObject "FieldStats" $ \o ->
    (FieldStats
      <$> o J..: "min"
      <*> o J..: "max"
      <*> o J..: "avg"
      <*> o J..: "count")
    <|> (FieldTerms
      <$> (mapM pb =<< o J..: "terms")
      <*> o J..: "others") where
    pb = J.withObject "FieldTerms.bucket" $ \b -> (,)
      <$> (pj =<< b J..: "value")
      <*> b J..: "count"
