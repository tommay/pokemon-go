module Yaml where

import qualified Data.Yaml.Builder as Builder
import qualified Data.Scientific as Scientific

instance Builder.ToYaml Float where
  toYaml = Builder.scientific . Scientific.fromFloatDigits
