module Main where

import Data.Aeson
import Data.Text as T
import Data.ByteString.Lazy as B
import Data.ByteString.Lazy.Char8 as BC
import GHC.Generics

main :: IO ()
main = print "hi"

data Book = Book
            { title :: T.Text
            , author :: T.Text
            , year :: Int
            } deriving (Show,Generic)

instance FromJSON Book
instance ToJSON Book

myBook :: Book
myBook = Book {author="Will Kurt"
                ,title="Learn Haskell"
                ,year=2017}

myBookJSON :: BC.ByteString
myBookJSON = encode myBook

rawJSON :: BC.ByteString
rawJSON = "{\"author\":\"Emil Ciroan\",\"title\":\"A Short History of Decay\",\"year\":1949}"
-- ^ this works, but what is in the book was obviously a copy-paste error of the broken version below

bookFromJSON :: Maybe Book
bookFromJSON = decode rawJSON

wrongJSON :: BC.ByteString
wrongJSON = "{\"writer\":\"Emil Cioran\",\"title\": \"A Short History of Decay\",\"year\"=1949}"

bookFromWrongJSON :: Maybe Book
bookFromWrongJSON = decode wrongJSON

-- Quick check 40.2
-- Use Generic to implement ToJSON and FromJSON for this type:
data Name = Name
    { firstName :: T.Text
    , lastName :: T.Text
    } deriving (Show,Generic)

instance FromJSON Name
instance ToJSON Name

