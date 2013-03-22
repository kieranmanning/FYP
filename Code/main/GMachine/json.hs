{-# LANGUAGE OverloadedStrings #-}
module Test where
import Data.Aeson
import Data.Text
import Control.Monad

data Person = Person
    { name  :: Text,
      age   :: Int }
    deriving(Show)

instance FromJSON Coord where
    parseJSON (Object v) = Person <$>
                           v .: "name" <*>
                           v .: "age"
    parseJSON _          = mzero
