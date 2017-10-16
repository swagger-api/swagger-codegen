{-# OPTIONS_GHC -fno-warn-unused-imports #-}

module Instances where

import Control.Monad
import Data.Char (isSpace)
import Data.List (sort)
import Test.QuickCheck
import qualified Data.Aeson as A
import qualified Data.ByteString.Lazy as BL
import qualified Data.HashMap.Strict as HM
import qualified Data.Set as Set
import qualified Data.Text as T
import qualified Data.Time as TI
import qualified Data.Vector as V

import ApproxEq
import SwaggerPetstore.Model

instance Arbitrary T.Text where
  arbitrary = T.pack <$> arbitrary

instance Arbitrary TI.Day where
  arbitrary = TI.ModifiedJulianDay . (2000 +) <$> arbitrary
  shrink = (TI.ModifiedJulianDay <$>) . shrink . TI.toModifiedJulianDay

instance Arbitrary TI.UTCTime where
  arbitrary =
    TI.UTCTime <$> arbitrary <*> (TI.secondsToDiffTime <$> choose (0, 86401))

instance Arbitrary BL.ByteString where
    arbitrary = BL.pack <$> arbitrary
    shrink xs = BL.pack <$> shrink (BL.unpack xs)

instance Arbitrary ByteArray where
    arbitrary = ByteArray <$> arbitrary
    shrink (ByteArray xs) = ByteArray <$> shrink xs

instance Arbitrary Binary where
    arbitrary = Binary <$> arbitrary
    shrink (Binary xs) = Binary <$> shrink xs

instance Arbitrary DateTime where
    arbitrary = DateTime <$> arbitrary
    shrink (DateTime xs) = DateTime <$> shrink xs

instance Arbitrary Date where
    arbitrary = Date <$> arbitrary
    shrink (Date xs) = Date <$> shrink xs

-- | A naive Arbitrary instance for A.Value:
instance Arbitrary A.Value where
  arbitrary = frequency [(3, simpleTypes), (1, arrayTypes), (1, objectTypes)]
    where
      simpleTypes :: Gen A.Value
      simpleTypes =
        frequency
          [ (1, return A.Null)
          , (2, liftM A.Bool (arbitrary :: Gen Bool))
          , (2, liftM (A.Number . fromIntegral) (arbitrary :: Gen Int))
          , (2, liftM (A.String . T.pack) (arbitrary :: Gen String))
          ]
      mapF (k, v) = (T.pack k, v)
      simpleAndArrays = frequency [(1, sized sizedArray), (4, simpleTypes)]
      arrayTypes = sized sizedArray
      objectTypes = sized sizedObject
      sizedArray n = liftM (A.Array . V.fromList) $ replicateM n simpleTypes
      sizedObject n =
        liftM (A.object . map mapF) $
        replicateM n $ (,) <$> (arbitrary :: Gen String) <*> simpleAndArrays
    
-- | Checks if a given list has no duplicates in _O(n log n)_.
hasNoDups
  :: (Ord a)
  => [a] -> Bool
hasNoDups = go Set.empty
  where
    go _ [] = True
    go s (x:xs)
      | s' <- Set.insert x s
      , Set.size s' > Set.size s = go s' xs
      | otherwise = False

instance ApproxEq TI.Day where
  (=~) = (==)

-- * Models
 
instance Arbitrary ApiResponse where
  arbitrary =
    ApiResponse
      <$> arbitrary -- apiResponseCode :: Maybe Int
      <*> arbitrary -- apiResponseType :: Maybe Text
      <*> arbitrary -- apiResponseMessage :: Maybe Text
    

instance Arbitrary Category where
  arbitrary =
    Category
      <$> arbitrary -- categoryId :: Maybe Integer
      <*> arbitrary -- categoryName :: Maybe Text
    

instance Arbitrary Order where
  arbitrary =
    Order
      <$> arbitrary -- orderId :: Maybe Integer
      <*> arbitrary -- orderPetId :: Maybe Integer
      <*> arbitrary -- orderQuantity :: Maybe Int
      <*> arbitrary -- orderShipDate :: Maybe DateTime
      <*> arbitrary -- orderStatus :: Maybe Text
      <*> arbitrary -- orderComplete :: Maybe Bool
    

instance Arbitrary Pet where
  arbitrary =
    Pet
      <$> arbitrary -- petId :: Maybe Integer
      <*> arbitrary -- petCategory :: Maybe Category
      <*> arbitrary -- petName :: Text
      <*> arbitrary -- petPhotoUrls :: [Text]
      <*> arbitrary -- petTags :: Maybe [Tag]
      <*> arbitrary -- petStatus :: Maybe Text
    

instance Arbitrary Tag where
  arbitrary =
    Tag
      <$> arbitrary -- tagId :: Maybe Integer
      <*> arbitrary -- tagName :: Maybe Text
    

instance Arbitrary User where
  arbitrary =
    User
      <$> arbitrary -- userId :: Maybe Integer
      <*> arbitrary -- userUsername :: Maybe Text
      <*> arbitrary -- userFirstName :: Maybe Text
      <*> arbitrary -- userLastName :: Maybe Text
      <*> arbitrary -- userEmail :: Maybe Text
      <*> arbitrary -- userPassword :: Maybe Text
      <*> arbitrary -- userPhone :: Maybe Text
      <*> arbitrary -- userUserStatus :: Maybe Int
    


