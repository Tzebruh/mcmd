{-# LANGUAGE OverloadedStrings #-}
module Helpers (addBackslashes) where
import Data.Text (pack, unpack, replace)

addBackslashes :: String -> String
addBackslashes s = let t = pack s in
    unpack $ replace "\"" "\\\"" (replace "\\" "\\\\" t)
    -- replaces backslashes first then backslashes so the backslashing of quotes doesn't cause unnecessary backslashing of their backslashes