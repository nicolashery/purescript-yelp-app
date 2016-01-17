module App.Utils where

import Prelude

import qualified Data.Array as A
import Data.List (List())
import qualified Data.List as L
import Data.Maybe (fromMaybe)
import qualified Data.String as S
import Data.StrMap (StrMap())
import qualified Data.StrMap as M
import Data.Tuple (Tuple(..))

foreign import encodeURIComponent :: String -> String

foreign import decodeURIComponent :: String -> String

foreign import extractQueryString :: String -> String

parseQueryString :: String -> StrMap String
parseQueryString qs =
  M.fromList (map part2tuple parts)
  where

  parts :: List String
  parts = L.toList (S.split "&" (S.drop 1 qs))

  part2tuple :: String -> Tuple String String
  part2tuple part =
    let keyVal = S.split "=" part
        key = decodeURIComponent (fromMaybe "" (A.head keyVal))
        val = decodeURIComponent (fromMaybe "" (keyVal A.!! 1))
    in Tuple key val
