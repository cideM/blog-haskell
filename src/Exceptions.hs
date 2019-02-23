module Exceptions where

import           Control.Exception.Safe
import           Data.Text              (Text)
import           Path                   (File, Path, Rel)

data AppException
  = ExtractSlugE (Path Rel File)
                 Text
  | UnknownException
  deriving (Show)

instance Exception AppException
