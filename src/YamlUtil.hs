module YamlUtil (
  (.=?),
  (.==)
) where

import qualified Data.Yaml.Builder as Builder
import           Data.Yaml.Builder ((.=))

import           Data.Text (Text)

-- Like .= for building yaml, but for Maybe values.  Use
-- Maybe.catMaybe to remove Nothings before passing the list to
-- Builder.mapping:
--   Builder.mapping $ Maybe.catMaybes [
--     "name" .== (Text.pack $ name this),

(.=?) :: (Builder.ToYaml a) => Text -> Maybe a -> Maybe (Text, Builder.YamlBuilder)
label .=? Nothing =
  Nothing
label .=? Just a =
  Just $ label .= a

-- Like ,=? but for required values.

(.==) :: (Builder.ToYaml a) => Text -> a -> Maybe (Text, Builder.YamlBuilder)
label .== a =
  Just $ label .= a

