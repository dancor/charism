import Data.Char
import Wumpus.Core

black :: PSRgb
black = RGB3 0 0 0

white :: PSRgb
white = RGB3 1 1 1

mkFilledSquare :: (PSColour c, Fill c) => c -> Double -> DPicture
mkFilledSquare col n = frame $ fill col $ vertexPath
  [P2 0 0, P2 n 0, P2 n n, P2 0 n]

main :: IO ()
main = mapM_ makeLtr ['a'..'z']

makeLtr c = do
  writeSVG_latin1 (c:".svg") pic where
  pic :: Picture Double
  pic = foldr1 picOver $ reverse [
    mkFilledSquare black 320,
    translate 10 10 $ mkFilledSquare white 300,
    translate 60 60 . frame $ 
      textlabel (FontAttr 330 $ FontFace "monospace" "monospace" SVG_BOLD)
        [toUpper c] (P2 0 0)]

