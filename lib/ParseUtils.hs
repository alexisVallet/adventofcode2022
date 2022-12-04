module ParseUtils (parseOrDie) where

import Data.Text (Text)
import Data.Void
import Text.Megaparsec

parseOrDie :: Parsec Void Text a -> Text -> a
parseOrDie parser contents = case runParser parser "" contents of
  Left err -> error $ errorBundlePretty err
  Right out -> out
