import Control.Applicative
import Control.Monad.Random
import Happstack.Server
import Happstack.Server.FastCGI
import Network.FastCGI
import System.IO
import Text.XHtml

import Charism
import WordOrder
import qualified Data.ByteString.Char8 as B
import qualified Data.Trie as T

main :: IO ()
main = do
  ls <- liftIO $ B.lines <$> (B.hGetContents =<< openFile lexFN ReadMode)
  let
    wdLen = 7
    lenReq l = l >= 3 && l <= wdLen
    killR = filter (/= '\r')  -- TODO better here (BS func ya?)
    lex = T.fromList . map (flip (,) ()) . filter (lenReq . length) $
      map (killR . B.unpack) ls
  runFastCGI . serverPartToCGI . withRequest $ \ req -> do
    wds <- liftIO . evalRandIO $ genWds lex Rand =<< genRack lex wdLen
    ok . toResponse $ show wds

