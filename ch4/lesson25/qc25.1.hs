{-# LANGUAGE OverloadedStrings #-}

import qualified Data.ByteString as B
import qualified Data.ByteString.Char8 as BC

bcInt :: BC.ByteString
bcInt = "6"

bcInt' :: Int
bcInt' = read (BC.unpack bcInt)
