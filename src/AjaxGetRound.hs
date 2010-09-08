import Control.Applicative
import Control.Monad.Random
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

main :: IO ()
main = do
  ls <- liftIO $
    map (bothond B.unpack . B.break (== ' ')) . B.lines <$>
    B.readFile lexFNWithRecDefs
  let
    wdLen = 7
    lenReq l = l >= 3 && l <= wdLen
    lex = T.fromList $ filter (lenReq . length . fst) ls
  runFastCGI . serverPartToCGI . withRequest $ \ req -> do
    wds <- liftIO . evalRandIO $ genWds lex Rand =<< genRack lex wdLen
    let wdDefs = map (\ x -> x ++ " " ++ fromJust (flip T.lookup lex x)) wds
    ok . toResponse $ show [wds, map (showHtmlFragment . toHtml) wdDefs]

