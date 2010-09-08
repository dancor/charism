import Control.Applicative
import Control.Monad.Random
import Data.Char
import Data.List
import Data.Maybe
import Happstack.Server
import Happstack.Server.FastCGI
import Network.FastCGI
import System.IO
import Text.XHtml
import Text.XHtml.Transitional

import Charism
import WordOrder
import qualified Data.ByteString.Char8 as B
import qualified Data.Trie as T

bothond f (a, b) = (f a, f b)

recIfy :: [String] -> String -> Lex String -> [String]
recIfy seen s lex =
  if null sRecWd || sRecWd `elem` seen then [s]
    else s : recIfy (sRecWd:seen) (fromJust $ T.lookup sRecWd lex) lex
  where
  sRecWd = map toUpper . takeWhile isLetter . drop 1 $
    dropWhile (`notElem` "{<") s

main :: IO ()
main = do
  ls <- liftIO $ map ((\ (a, b) -> (a, a ++ b)) .
    bothond B.unpack . B.break (== ' ')) . B.lines <$>
    B.readFile lexFNWithDefs
  let
    lex = T.fromList ls
    ls' = map (intercalate " -> " . (\ s -> recIfy [] s lex) . snd) ls
  putStr $ unlines ls'

