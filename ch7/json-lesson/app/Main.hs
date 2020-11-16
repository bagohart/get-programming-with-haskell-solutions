module Main where

import Control.Monad
import Data.Aeson
import Data.Text as T
import Data.ByteString.Lazy as B
import Data.ByteString.Lazy.Char8 as BC
import GHC.Generics

-- main :: IO ()
-- main = print "hi"

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

sampleError :: BC.ByteString
sampleError = "{\"message\":\"oops!\",\"error\": 123}"

data ErrorMessage = ErrorMessage
                    { message :: T.Text
                    -- , error :: Int -- reserved keyword in Haskell!
                    , errorCode :: Int
                    } deriving Show

instance FromJSON ErrorMessage where
    parseJSON (Object v) = ErrorMessage <$> v .: "message"
                                        <*> v .: "error"

sampleErrorMessage :: Maybe ErrorMessage
sampleErrorMessage = decode sampleError

instance ToJSON ErrorMessage where 
    toJSON (ErrorMessage message errorCode) =
        object [ "message" .= message
               , "error" .= errorCode
               ]

anErrorMessage :: ErrorMessage
anErrorMessage = ErrorMessage "Everything is Okay" 0

-- commented out to avoid double declaration
-- data Name = Name
--             { firstName :: T.Text
--             , lastName :: T.Text
--             } deriving (Show)
--
-- instance FromJSON Name where 
--     parseJSON (Object v) =
--             pure Name
--         <*> v .: "firstName"
--         <*> v .: "lastName"
--
-- instance ToJSON Name where 
--      toJSON (Name firstName lastName) =
--          object [ "firstName" .= firstName
--                 , "lastName" .= lastName
--                 ]

-- NOAA JSON sample data:
-- {
--     "metadata":{
--         "resultset":{
--             "offset":1,
--             "count":11,
--             "limit":25
--         }
--     },
--     "results":[
--         {
--             "uid":"gov.noaa.ncdc:C00861",
--             "mindate":"1763-01-01",
--             "maxdate":"2017-02-01",
--             "name":"Daily Summaries",
--             "datacoverage":1,
--             "id":"GHCND"
--         },

data NOAAResult = NOAAResult
                    { uid :: T.Text
                    , mindate :: T.Text
                    , maxdate :: T.Text
                    , name :: T.Text
                    , datacoverage :: Float
                    -- the book says it's Int, but that's wrong. parse fails for 0.95 in json ...
                    , resultId :: T.Text
                    } deriving Show

instance FromJSON NOAAResult where
    parseJSON (Object v) =
        NOAAResult  <$> v .: "uid"
                    <*> v .: "mindate"
                    <*> v .: "maxdate"
                    <*> v .: "name"
                    <*> v .: "datacoverage"
                    <*> v .: "id"

instance ToJSON NOAAResult where 
    toJSON (NOAAResult uid mindate maxdate name datacoverage resultId) =
        object [ "uid" .= uid
                , "mindate" .= mindate
                , "maxdate" .= maxdate
                , "name" .= name
                , "datacoverage" .= datacoverage
                , "id" .= resultId
               ]

data Resultset = Resultset
                    { offset :: Int
                    , count :: Int
                    , limit :: Int
                    } deriving (Show,Generic)

instance FromJSON Resultset
instance ToJSON Resultset

data Metadata = Metadata
                {
                resultset :: Resultset
                } deriving (Show,Generic)

instance FromJSON Metadata
instance ToJSON Metadata

data NOAAResponse = NOAAResponse
                    { metadata :: Metadata
                    , results :: [NOAAResult]
                    } deriving (Show,Generic)

instance FromJSON NOAAResponse
instance ToJSON NOAAResponse

printResults :: Maybe [NOAAResult] -> IO ()
printResults Nothing = print "error loading data"
printResults (Just results) = do
    forM_ results (print . name)
    -- print dataName
    -- ^ what is this. lol.

main :: IO ()
main = do
    jsonData <- B.readFile "data.json"
    -- print (eitherDecode jsonData :: Either String NOAAResponse) -- find error because book is broken :)
    let noaaResponse = decode jsonData :: Maybe NOAAResponse
    let noaaResults = results <$> noaaResponse
    printResults noaaResults
    -- ex. q40.1
    print "Now let's json it back"
    print $ toJSON noaaResults
    print $ encode noaaResults
    -- ex. q40.2
    Prelude.putStrLn "intListExample:"
    print $ toJSON intListExample
    print $ encode intListExample

-- q40.2
data IntList = Cons Int IntList | EmptyList deriving (Show,Generic)

instance FromJSON IntList
instance ToJSON IntList

intListExample :: IntList
intListExample = Cons 1 $ Cons 2 EmptyList

-- the end result here looks a bit strange. hm. :
-- Object (fromList [("tag",String "Cons"),("contents",Array [Number 1.0,Object (fromList [("tag",String "Cons"),("contents",Array [Number 2.0,Object (fromList [("tag",String "EmptyList")])])])])])
-- "{\"tag\":\"Cons\",\"contents\":[1,{\"tag\":\"Cons\",\"contents\":[2,{\"tag\":\"EmptyList\"}]}]}"
