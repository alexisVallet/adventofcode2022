module ParseUtils (parseOrDie, skipLines) where

import Control.Monad
import Data.Text (Text)
import Data.Void
import Text.Megaparsec
import Text.Megaparsec.Char

parseOrDie :: Parsec Void Text a -> Text -> a
parseOrDie parser contents = case runParser parser "" contents of
  Left err -> error $ errorBundlePretty err
  Right out -> out

skipLines :: Int -> Parsec Void Text ()
skipLines i = replicateM_ i $ manyTill anySingle newline 