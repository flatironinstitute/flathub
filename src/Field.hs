{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
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
  , FieldDesc(..)
  , FieldSub(..)
  , fieldName
  , fieldEnum
  , fieldTitle
  , fieldDescr
  , fieldUnits
  , fieldFlag
  , fieldStore
  , fieldTerms
  , fieldDict
  , fieldScale
  , fieldReversed
  , fieldWildcard
  , fieldSize
  , fieldIngest
  , fieldMissing
  , fieldAttachment
  , fieldSub
  , fieldDisp
  , Field, FieldGroup
  , Fields, FieldGroups
  , setFieldValue
  , updateFieldValueM
  , parseFieldGroup
  , expandField, expandFields, expandAllFields
  , deleteField
  , fieldsDepth
  , FieldValue
  , parseFieldValue
  , fieldJValue, fieldJValues
  , idField
  , docField
  , fieldsCSV
  , numpyFieldSize
  , numpyDtype
  , Count
  , FieldStats(..)
  ) where

import           Control.Applicative (Alternative, empty, (<|>))
import           Control.Arrow (first)
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

data FieldDesc s = FieldDesc
  { fieldDescName :: T.Text
  , fieldDescTitle :: T.Text
  , fieldDescDescr :: Maybe T.Text
  , fieldDescUnits :: Maybe T.Text
  , fieldDescFlag :: FieldFlag
  , fieldDescStore :: Maybe Bool -- ^true: store only, false: index only, Nothing: index+store (store = all, index = not any)
  , fieldDescEnum :: Maybe (V.Vector T.Text) -- ^enumeration values (for integral fields)
  , fieldDescTerms :: Bool -- ^treat as catagorical (implied if enum or string)
  , fieldDescDict :: Maybe T.Text -- ^link to field dictionary
  , fieldDescScale :: Maybe Scientific -- ^scale factor, to display scale*x instead
  , fieldDescReversed :: Bool -- ^reverse axis on plotting
  , fieldDescWildcard :: Bool -- ^allow wildcard text filter
  , fieldDescSize :: Word -- ^string length
  , fieldDescIngest :: Maybe T.Text -- ^special handling on ingest (interpretation depends on format)
  , fieldDescMissing :: [BS.ByteString] -- ^values to treat as missing on ingest
  , fieldDescAttachment :: Maybe Attachment
  , fieldDescSub :: s (FieldsSub Proxy s)
  }

data FieldSub t s = Field
  { fieldDesc :: !(FieldDesc s)
  , fieldType :: TypeValue t
  }

fieldName :: FieldSub t s -> T.Text
fieldName = fieldDescName . fieldDesc
fieldEnum :: FieldSub t s -> Maybe (V.Vector T.Text)
fieldEnum = fieldDescEnum . fieldDesc
fieldTitle :: FieldSub t s -> T.Text
fieldTitle = fieldDescTitle . fieldDesc
fieldDescr :: FieldSub t s -> Maybe T.Text
fieldDescr = fieldDescDescr . fieldDesc
fieldUnits :: FieldSub t s -> Maybe T.Text
fieldUnits = fieldDescUnits . fieldDesc
fieldFlag :: FieldSub t s -> FieldFlag
fieldFlag = fieldDescFlag . fieldDesc
fieldStore :: FieldSub t s -> Maybe Bool
fieldStore = fieldDescStore . fieldDesc
fieldTerms :: FieldSub t s -> Bool
fieldTerms = fieldDescTerms . fieldDesc
fieldDict :: FieldSub t s -> Maybe T.Text
fieldDict = fieldDescDict . fieldDesc
fieldScale :: FieldSub t s -> Maybe Scientific
fieldScale = fieldDescScale . fieldDesc
fieldReversed :: FieldSub t s -> Bool
fieldReversed = fieldDescReversed . fieldDesc
fieldWildcard :: FieldSub t s -> Bool
fieldWildcard = fieldDescWildcard . fieldDesc
fieldSize :: FieldSub t s -> Word
fieldSize = fieldDescSize . fieldDesc
fieldIngest :: FieldSub t s -> Maybe T.Text
fieldIngest = fieldDescIngest . fieldDesc
fieldMissing :: FieldSub t s -> [BS.ByteString]
fieldMissing = fieldDescMissing . fieldDesc
fieldAttachment :: FieldSub t s -> Maybe Attachment
fieldAttachment = fieldDescAttachment . fieldDesc
fieldSub :: FieldSub t s -> s (FieldsSub Proxy s)
fieldSub = fieldDescSub . fieldDesc

fieldDisp :: FieldSub t m -> Bool
fieldDisp = (FieldHidden <) . fieldFlag

type FieldGroup = FieldSub Proxy Maybe
type Field = FieldSub Proxy Proxy
type FieldValue = FieldSub Identity Proxy

type FieldsSub t m = V.Vector (FieldSub t m)
type FieldGroups = FieldsSub Proxy Maybe
type Fields = [Field]

instance Alternative s => Default (FieldDesc s) where
  def = FieldDesc
    { fieldDescName = T.empty
    , fieldDescEnum = Nothing
    , fieldDescTitle = T.empty
    , fieldDescDescr = Nothing
    , fieldDescUnits = Nothing
    , fieldDescFlag = FieldNormal
    , fieldDescStore = Just False
    , fieldDescTerms = False
    , fieldDescSub = empty
    , fieldDescDict = Nothing
    , fieldDescScale = Nothing
    , fieldDescReversed = False
    , fieldDescIngest = Nothing
    , fieldDescMissing = []
    , fieldDescAttachment = Nothing
    , fieldDescWildcard = False
    , fieldDescSize = 8
    }

instance Alternative s => Default (FieldSub Proxy s) where
  def = Field
    { fieldDesc = def
    , fieldType = def
    }

instance KM.Keyed (FieldSub t m) where
  type Key (FieldSub t m) = T.Text
  key = fieldName

setFieldValueUnsafe :: FieldSub t Proxy -> TypeValue f -> FieldSub f Proxy
setFieldValueUnsafe f t = f{ fieldType = t }

setFieldValue :: (Functor t, Functor f) => FieldSub t Proxy -> TypeValue f -> FieldSub f Proxy
setFieldValue f = setFieldValueUnsafe f . coerceTypeValue (fieldType f)

updateFieldValueM :: (Functor t, Functor f, Monad m) => FieldSub t Proxy -> (forall a . Typed a => t a -> m (f a)) -> m (FieldSub f Proxy)
updateFieldValueM f t = setFieldValueUnsafe f <$> traverseTypeValue t (fieldType f)

numpyFieldSize :: Field -> Word
numpyFieldSize Field{ fieldType = Keyword _, fieldDesc = d } = fieldDescSize d
numpyFieldSize Field{ fieldType = t } = numpyTypeSize t

numpyDtype :: Field -> String
numpyDtype Field{ fieldType = Boolean _ } = "?"
numpyDtype Field{ fieldType = ULong _ } = "<u8"
numpyDtype f = '<' : baseType ('f','i','?','S','V') (fieldType f) : show (numpyFieldSize f)

instance J.ToJSON Field where
  toJSON f@Field{ fieldDesc = FieldDesc{..}, ..} = J.object $
    [ "name" J..= fieldDescName
    , "type" J..= fieldType
    , "title" J..= fieldDescTitle
    , "disp" J..= (fieldDescFlag > FieldHidden)
    , "base" J..= baseType ('f','i','b','s','v') fieldType
    , "dtype" J..= numpyDtype f
    ] ++ concatMap maybeToList
    [ ("enum" J..=) <$> fieldDescEnum
    , ("descr" J..=) <$> fieldDescDescr
    , ("units" J..=) <$> fieldDescUnits
    , ("flag" J..=) <$> case fieldDescFlag of
        FieldTop -> Just False
        FieldRequired -> Just True
        _ -> Nothing
    , ("terms" J..= fieldDescTerms) <$ guard fieldDescTerms
    , ("wildcard" J..= fieldDescWildcard) <$ guard fieldDescWildcard
    , ("dict" J..=) <$> fieldDescDict
    , ("scale" J..=) <$> fieldDescScale
    , ("reversed" J..= fieldDescReversed) <$ guard fieldDescReversed
    , ("attachment" J..= True) <$ fieldDescAttachment
    ]

parseFieldGroup :: HM.HashMap T.Text FieldGroup -> J.Value -> J.Parser FieldGroup
parseFieldGroup dict = parseFieldDefs def where
  parseFieldDefs :: FieldGroup -> J.Value -> J.Parser FieldGroup
  parseFieldDefs defd = J.withObject "field" $ \f -> do
    fieldDescDict <- f J..:? "dict"
    d <- maybe (return defd)
      (\n -> maybe (fail $ "Unknown dict key: " ++ show n) return $ HM.lookup n dict)
      fieldDescDict
    fieldDescName <- f J..:! "name" J..!= fieldName d
    when (T.any ('.' ==) fieldDescName) $ fail $ "Invalid field name: " ++ show fieldDescName
    fieldType <- f J..:! "type" J..!= fieldType d
    fieldDescEnum <- maybe (fieldEnum d <|> V.fromList ["false","true"] <$ guard (typeIsBoolean fieldType)) join
      <$> f J..:! "enum"
    fieldDescTitle <- f J..:! "title" J..!= if T.null (fieldTitle d) then fieldDescName else fieldTitle d
    fieldDescDescr <- (<|> fieldDescr d) <$> f J..:? "descr"
    fieldDescUnits <- (<|> fieldUnits d) <$> f J..:? "units"
    fieldDescStore <- f J..:! "store" J..!= fieldStore d
    fieldDescFlag <- f J..:! "flag" J..!= fieldFlag d
    fieldDescScale <- f J..:? "scale"
    fieldDescReversed <- f J..:? "reversed" J..!= fieldReversed d
    fieldDescIngest <- f J..:? "ingest"
    fieldDescMissing <- map TE.encodeUtf8 <$> case HM.lookup "missing" f of
      Nothing -> return []
      Just J.Null -> return []
      Just (J.String s) -> return [s]
      Just (J.Array l) -> mapM J.parseJSON $ V.toList l
      Just j -> J.typeMismatch "missing string" j
    fieldDescAttachment <- f J..:? "attachment"
    fieldDescTerms <- f J..:! "terms" J..!= (isJust fieldDescEnum || typeIsString fieldType)
    fieldDescWildcard <- f J..:! "wildcard" J..!= fieldWildcard d
    fieldDescSize <- f J..:? "size" J..!= fieldSize d
    fieldDescSub <- (<|> fieldSub d) <$> J.explicitParseFieldMaybe' (J.withArray "subfields" $ V.mapM $
        parseFieldDefs defd
          { fieldType = fieldType
          , fieldDesc = (fieldDesc defd)
            { fieldDescEnum = fieldDescEnum
            , fieldDescFlag = fieldDescFlag
            , fieldDescMissing = fieldDescMissing
            }
          })
      f "sub"
    return Field{ fieldDesc = FieldDesc{..}, ..}

instance J.FromJSON FieldGroup where
  parseJSON = parseFieldGroup mempty

instance Semigroup (FieldSub t m) where
  (<>) = subField

instance Alternative m => Monoid (FieldSub Proxy m) where
  mempty = def
  mappend = subField

subField :: FieldSub s n -> FieldSub t m -> FieldSub t m
subField f s = s
  { fieldDesc = (fieldDesc s)
    { fieldDescName = merge '_' (fieldName f) (fieldName s)
    , fieldDescTitle = merge ' ' (fieldTitle f) (fieldTitle s)
    , fieldDescDescr = joinMaybeWith (\x -> (x <>) . T.cons '\n') (fieldDescr f) (fieldDescr s)
    , fieldDescUnits = fieldUnits s <|> fieldUnits f
    }
  } where
  merge c a b
    | T.null a = b
    | T.null b = a
    | a == b = b
    | otherwise = a <> T.cons c b

expandField :: FieldGroup -> Fields
expandField f@Field{ fieldDesc = FieldDesc{ fieldDescSub = Nothing } } = return f{ fieldDesc = (fieldDesc f){ fieldDescSub = Proxy } }
expandField f@Field{ fieldDesc = FieldDesc{ fieldDescSub = Just l } } =
  foldMap (expandField . mappend f) l

expandFields :: FieldGroups -> Fields
expandFields = foldMap expandField

expandAllFields :: FieldGroups -> HM.HashMap T.Text FieldGroup
expandAllFields = foldMap expandAllField where
  expandAllField :: FieldGroup -> HM.HashMap T.Text FieldGroup
  expandAllField f = HM.singleton (fieldName f) f <> foldMap (foldMap (expandAllField . mappend f)) (fieldSub f)

deleteField :: T.Text -> FieldGroups -> FieldGroups
deleteField n = dfs mempty where
  df p f@Field{ fieldDesc = FieldDesc{ fieldDescSub = Nothing } }
    | n == fieldName (p <> f) = Nothing
    | otherwise = Just f
  df p f@Field{ fieldDesc = FieldDesc{ fieldDescSub = Just l } } =
    Just f{ fieldDesc = (fieldDesc f){ fieldDescSub = Just $ dfs (p <> f) l } }
  dfs = V.mapMaybe . df

fieldsDepth :: FieldGroups -> Word
fieldsDepth = getMax . depth where
  depth = succ . foldMap (foldMap depth . fieldSub)

parseFieldValue :: Field -> T.Text -> Maybe FieldValue
parseFieldValue f = fmap (setFieldValueUnsafe f) . pv f where
  pv Field{ fieldType = (Byte _), fieldDesc = FieldDesc{ fieldDescEnum = Just l } } s
    | Just i <- V.elemIndex s l = Just $ Byte $ fromIntegral i
  pv Field{ fieldType = t } s = sequenceValue $ parseTypeValue t s

fieldJValue :: FieldValue -> J.Series
fieldJValue f = fieldName f J..= fieldType f

fieldJValues :: [FieldValue] -> J.Series
fieldJValues = foldMap fieldJValue

-- |pseudo field representing ES _id, _doc
idField, docField :: Field
idField = Field{ fieldDesc = def{ fieldDescName = "_id", fieldDescTitle = "_id", fieldDescFlag = FieldHidden }, fieldType = Keyword Proxy }
docField = Field{ fieldDesc = def{ fieldDescName = "_doc", fieldDescTitle = "_doc", fieldDescFlag = FieldHidden }, fieldType = Keyword Proxy }

type Count = Word

data FieldStats a
  = FieldStats{ statsMin, statsMax, statsAvg :: Maybe Scientific, statsCount :: Count }
  | FieldTerms{ termsBuckets :: [(a, Count)], termsCount :: Count }

instance Functor FieldStats where
  fmap _ (FieldStats n x a c) = FieldStats n x a c
  fmap f (FieldTerms b c) = FieldTerms (map (first f) b) c

fieldsCSV :: Fields -> B.Builder
fieldsCSV l = csvTextRow ["variable", "name", "type", "units", "description", "values","dict","scale"] <> foldMap fieldCSV l where
  fieldCSV :: Field -> B.Builder
  fieldCSV f = csvTextRow
    [ fieldName f
    , fieldTitle f
    , T.pack $ show $ fieldType f
    , fold $ fieldUnits f
    , fold $ fieldDescr f
    , foldMap (T.intercalate "," . V.toList) $ fieldEnum f
    , fold $ fieldDict f
    , foldMap (T.pack . show) $ fieldScale f
    ]
