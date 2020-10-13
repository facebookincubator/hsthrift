module EncodingLib where

import qualified Text.Regex.Base as R
import qualified Text.Regex.PCRE as PCRE
import qualified Data.Text.Encoding as Text
import qualified Data.Text as Text

import Data.Text (Text)
import Data.ByteString (ByteString)
import Data.List (foldl')

import Foreign.C hiding (peekCStringLen)
import Data.Text.Foreign

regexMatchWord :: Text -> Bool
regexMatchWord w = ranges == [(0, Text.length w)]
  where
    regex = R.makeRegexOpts compOpts execOpts r
    compOpts = PCRE.defaultCompOpt + PCRE.compCaseless
    execOpts = PCRE.defaultExecOpt
    r = "luned(\x00ec|i)" :: String
    allMatches = R.match regex (Text.encodeUtf8 w) :: [[ByteString]]
    (ranges, _, _) = foldl' g ([], w, 0) allMatches
    g (ranges', s, offset) matches =
      case map Text.decodeUtf8 matches of
        [] -> (ranges', s, offset)
        ("":_) -> (ranges', s, offset)
        (text:_group) ->
          let (x,xs) = Text.breakOn text s
              m = offset + Text.length x
              n = Text.length text
              s' = Text.drop n xs
              newOffset = m + n
              range = (m, newOffset)
          in (ranges' ++ [range], s', newOffset)

cRegexMatchWord :: CString -> CInt -> IO CInt
cRegexMatchWord cstr clen = do
  w <- peekCStringLen (cstr, fromIntegral clen)
  return $ if regexMatchWord w then 1 else 0

foreign export ccall "hs_regexMatchWord"
  cRegexMatchWord :: CString
                  -> CInt
                  -> IO CInt
