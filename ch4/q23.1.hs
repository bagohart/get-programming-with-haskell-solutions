{-# LANGUAGE OverloadedStrings #-}

import Data.Text as T
import Data.Text.IO as TIO

helloPerson :: T.Text -> T.Text
helloPerson name = "Hello" <> " " <> name <> "!"

main :: IO ()
main = do
    TIO.putStrLn "Hello! What's your name?"
    name <- TIO.getLine
    let statement = helloPerson name
    TIO.putStrLn statement
