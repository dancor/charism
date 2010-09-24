import Control.Applicative
import Control.Arrow
import Control.Monad.Random
import Data.Char
import Data.Maybe
import FUtil
import Happstack.Server
import Happstack.Server.FastCGI
import Network.FastCGI
import System.IO
import Text.XHtml
import Text.XHtml.Transitional
import qualified Text.JSON.AttoJSON as J

import Charism
import WordOrder
--import qualified Data.CompactString.UTF8 as B
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as BSL
import qualified Data.Trie as T

bothond f (a, b) = (f a, f b)

langInfo :: String -> String -> [(Int, Char)] -> (String, (String, String))
langInfo lang lexF ltrs =
  (lang, (lexF, concatMap (\ (n, c) -> replicate n c) ltrs))

tileInfo :: Int -> Int -> Char -> (Int, Char)
tileInfo pts num ltr = (num, ltr)

langToInfo = [
  langInfo "en" "/usr/share/dict/game/en.def" [
    --tileInfo 0 2 '_',
    tileInfo 1 12 'E',
    tileInfo 1 9 'A',
    tileInfo 1 9 'I',
    tileInfo 1 8 'O',
    tileInfo 1 6 'N',
    tileInfo 1 6 'R',
    tileInfo 1 6 'T',
    tileInfo 1 4 'L',
    -- double S since ppl tend to hold them
    --tileInfo 1 4 'S',
    tileInfo 1 8 'S',
    tileInfo 1 4 'U',
    tileInfo 2 4 'D',
    tileInfo 2 3 'G',
    tileInfo 3 2 'B',
    tileInfo 3 2 'C',
    tileInfo 3 2 'M',
    tileInfo 3 2 'P',
    tileInfo 4 2 'F',
    tileInfo 4 2 'H',
    tileInfo 4 2 'V',
    tileInfo 4 2 'W',
    tileInfo 4 2 'Y',
    tileInfo 5 1 'K',
    tileInfo 8 1 'J',
    tileInfo 8 1 'X',
    tileInfo 10 1 'Q',
    tileInfo 10 1 'Z'
    ],
  langInfo "es" "/usr/share/dict/game/es.def" [
    --tileInfo 0 2 '_',
    tileInfo 1 12 'A',
    tileInfo 1 12 'E',
    tileInfo 1 9 'O',
    tileInfo 1 6 'I',
    tileInfo 1 6 'S',
    tileInfo 1 5 'N',
    tileInfo 1 4 'L',
    tileInfo 1 5 'R',
    tileInfo 1 5 'U',
    tileInfo 1 4 'T',
    tileInfo 2 5 'D',
    tileInfo 2 2 'G',
    tileInfo 3 4 'C',
    tileInfo 3 2 'B',
    tileInfo 3 2 'M',
    tileInfo 3 2 'P',
    tileInfo 4 2 'H',
    tileInfo 4 1 'F',
    tileInfo 4 1 'V',
    tileInfo 4 1 'Y',
    tileInfo 5 1 '1',
    tileInfo 5 1 'Q',
    tileInfo 8 1 'J',
    tileInfo 8 1 '2',
    tileInfo 8 1 'Ñ',
    tileInfo 8 1 '3',
    tileInfo 8 1 'X',
    tileInfo 10 6 'Z'
    ],
  langInfo "de" "/usr/share/dict/game/de.def" [
    --tileInfo 0 2 '_',
    tileInfo 1 15 'E',
    tileInfo 1 9 'N',
    tileInfo 1 7 'S',
    tileInfo 1 6 'I',
    tileInfo 1 6 'R',
    tileInfo 1 6 'T',
    tileInfo 1 6 'U',
    tileInfo 1 5 'A',
    tileInfo 1 4 'D',
    tileInfo 2 4 'H',
    tileInfo 2 3 'G',
    tileInfo 2 3 'L',
    tileInfo 2 3 'O',
    tileInfo 3 4 'M',
    tileInfo 3 2 'B',
    tileInfo 3 1 'W',
    tileInfo 3 1 'Z',
    tileInfo 4 2 'C',
    tileInfo 4 2 'F',
    tileInfo 4 2 'K',
    tileInfo 4 1 'P',
    tileInfo 6 1 'Ä',
    tileInfo 6 1 'J',
    tileInfo 6 1 'Ü',
    tileInfo 6 1 'V',
    tileInfo 8 1 'Ö',
    tileInfo 8 1 'X',
    tileInfo 10 1 'Q',
    tileInfo 10 1 'Y'
    ]
  ]

main :: IO ()
main = do
  let
    langsToLexFs = map (second fst) langToInfo
    --wdLen = 7
    wdLen = 5
    lenReq l = l >= 3 && l <= wdLen
  langToLex <- liftIO $ mapM (\ (lang, lexF) ->
    ((,) lang . T.fromList . filter (lenReq . length . fst) .
      --map (bothond B.unpack . B.break (== ' ')) . B.lines) <$> B.readFile lexF
      map (break (== ' ')) . lines) <$> readFile lexF
    ) langsToLexFs
  runFastCGI . serverPartToCGI . withRequest $ \ req -> do
    let
      lang = maybe "en" (map (chr . fromIntegral) .
        BSL.unpack . inputValue) . lookup "lang" $ rqInputs req
      lookupFallback :: (Eq a) => a -> a -> [(a, b)] -> b
      lookupFallback k k' m = fromMaybe (fromJust $ lookup k' m) (lookup k m)
      ltrs = snd $ lookupFallback lang "en" langToInfo
      --lex = lookupFallback lang "en" langToLex
      lex = lookupFallback lang "en"
        (langToLex :: [(String, Lex String)])
    wds <- liftIO . evalRandIO $ genWds lex Rand =<< genRack lex ltrs wdLen
    let
      wdDefs = map (\ x -> x ++ " " ++ fromJust (flip T.lookup lex x)) wds
    ok . toResponse . map (chr . fromIntegral) . B.unpack . J.showJSON $
      J.toJSON [wds, map (showHtmlFragment . toHtml) wdDefs]

