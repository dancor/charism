import Data.Char
import Wumpus.Core

black :: RGBi
black = RGBi 0 0 0

white :: RGBi
white = RGBi 255 255 255

green :: RGBi
green = RGBi 0 127 0

mkFilledSquare :: RGBi -> Double -> DPicture
mkFilledSquare col n = mkFilledRect col n n

mkFilledRect :: RGBi -> Double -> Double -> DPicture
mkFilledRect col w h = frame [
  fill col $ vertexPath [P2 0 0, P2 w 0, P2 w h, P2 0 h]
  ]

main :: IO ()
main = do
  mapM_ makeLtr ['A'..'Z']
  writeSVG_latin1 "@.svg" $ multi [
    translate 10 10 $ mkFilledRect white 300 30,
    mkFilledSquare green 320
    ]
  writeSVG_latin1 "help.svg" $ frame [
    textlabel black (FontAttr 20 $ FontFace "monospace" "monospace" SVG_BOLD)
      "help" (P2 0 (0 :: Double))
    ]

makeLtr :: Char -> IO ()
makeLtr c = writeSVG_latin1 (c:".svg") $ multi [
  translate 60 (if c == 'Q' then 60 else 40) $ frame [
    textlabel black (FontAttr 330 $ FontFace "monospace" "monospace" SVG_BOLD)
      [c] (P2 0 0)
    ],
  translate 10 10 $ mkFilledSquare white 300,
  mkFilledSquare black 320
  ]

