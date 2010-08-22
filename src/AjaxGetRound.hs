import Happstack.Server
import Happstack.Server.FastCGI
import Network.FastCGI
import Text.XHtml

main :: IO ()
main = runFastCGI . serverPartToCGI . withRequest $ \ req ->
  ok (toResponse $ concatHtml [
    toHtml "hello"
    ])

