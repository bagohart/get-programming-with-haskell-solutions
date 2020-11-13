{-# LANGUAGE OverloadedStrings #-}
-- use
-- :set -XOverloadedStrings
-- to play with in ghci

import Data.Text as T
import Data.Text.IO as TIO
import Data.Semigroup

firstWord :: String
firstWord = "pessimism"

secondWord :: T.Text
secondWord = T.pack firstWord

thirdWord :: String
thirdWord = T.unpack secondWord

fourthWord :: T.Text
fourthWord = T.pack thirdWord

-- needs overloaded strings
myWord :: T.Text
myWord = "dog"

sampleInput :: T.Text
sampleInput = "this\nis\ninput"

-- quick check 23.3
tLines :: Text -> [Text]
tLines = T.splitOn "\n"

tUnlines :: [Text] -> Text
tUnlines = T.intercalate "\n"

-- how does unicode work on my machine what am I missing o_O
dharma :: T.Text
dharma = "dharma"

bgText :: T.Text
bgText = "dharmalolrofldharmawtfdharmauhoh"

highlight :: T.Text -> T.Text -> T.Text
highlight query fullText = T.intercalate highlighted pieces
    where pieces = T.splitOn query fullText
          highlighted = mconcat ["{", query, "}"]

main = do
    TIO.putStrLn (highlight dharma bgText)
