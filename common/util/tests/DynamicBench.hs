-- Copyright (c) Facebook, Inc. and its affiliates.

module DynamicBench (main) where

import Control.Monad (replicateM)
import Control.DeepSeq
import Control.Exception (bracket)
import Data.Aeson hiding (Options, parseJSON)
import Data.ByteString as B
import Data.ByteString.Lazy as LB
import Data.ByteString.Unsafe
import Data.Maybe (fromMaybe, fromJust, isJust)
import Foreign hiding (void)
import Foreign.C
import Util.Timing (timeNF)
import Options.Applicative hiding (str, action)
import Text.Printf (printf)

import Foreign.CPP.Marshallable
import Foreign.CPP.Dynamic as Dynamic
import Foreign.CPP.HsStruct

data Options = Options
  { optFilename :: FilePath
  , optRepeat :: Maybe Int
  }

optionsParser :: Parser Options
optionsParser = Options
  <$> strOption
    (  long "filename"
    <> short 'f'
    <> metavar "FILE"
    <> help "Input json file"
    )
  <*> optional
    ( option auto
      (  long "repeat"
      <> short 'n'
      <> metavar "NUM"
      <> help "Number of times to run"
      )
    )

parseCStringWith :: (B.ByteString -> Maybe Value)
                 -> (CStringLen -> IO B.ByteString)
                 -> IO (Maybe Value)
parseCStringWith decodeJson getJson = do
  json <- alloca $ \plen -> do
    str <- getJsonAsCStringLen plen
    len <- peek plen
    getJson (str, fromIntegral len)
  return $ decodeJson json

parseCString :: IO (Maybe Value)
parseCString = parseCStringWith decodeStrict packCStringLen

parseCString' :: IO (Maybe Value)
parseCString' = parseCStringWith decodeStrict' unsafePackCStringLen

parseDynamic :: IO Value
parseDynamic = bracket getJsonAsPtrDynamic delete readDynamic

parseHsJSON :: IO Value
parseHsJSON = bracket getJsonAsPtrHsJSON delete $ fmap hsJSON . peek

serializeValueToDynamic :: Value -> (Ptr Dynamic -> IO a) -> IO a
serializeValueToDynamic value = bracket buildDynamic delete
  where
  buildDynamic = unsafeUseAsCStringLen (LB.toStrict $ encode value) $
    \(str, len) -> c_parseJSON str $ fromIntegral len

main :: IO ()
main = do
  options <- execParser $ info (optionsParser <**> helper) fullDesc
  let filename = optFilename options
      n = fromMaybe 1000 $ optRepeat options
      time :: NFData a => IO a -> IO Double
      time io = do (t,_,_) <- timeNF (replicateM n io); return t

  withCString filename initializeJson
  j1 <- fromJust <$> parseCString
  j2 <- parseDynamic
  j3 <- parseHsJSON
  let isEqual = j1 == j2 && j2 == j3
  t1 <- time $ isJust <$> parseCString
  t2 <- time parseCString
  t3 <- time parseCString'
  t4 <- time $ (`seq` return ()) =<< parseHsJSON
  t5 <- time parseHsJSON
  t6 <- time $ (`seq` return ()) =<< parseDynamic
  t7 <- time parseDynamic

  b <- B.readFile filename
  t8 <- time $ Dynamic.parseJSON b

  isEqual' <- withDynamic j1 $ \d1 ->
    serializeValueToDynamic j1 $ \d2 ->
      toBool <$> compareDynamic d1 d2
  t1' <- time $ withDynamic j1 (`seq` return ())
  t2' <- time $ serializeValueToDynamic j1 (`seq` return ())

  printf "[%s]\n" filename
  printf "  results %s\n" (if isEqual then "match" else "mismatch" :: String)
  printf "  parseCString(lazy)      %.6fs (x%.2f)\n" t1 (t1 / t6)
  printf "  parseHsJSON(lazy)       %.6fs (x%.2f)\n" t4 (t4 / t6)
  printf "  parseDynamic(lazy)      %.6fs\n" t6
  printf "  parseCString(strict)    %.6fs (x%.2f)\n" t2 (t2 / t7)
  printf "  parseCString(strict')   %.6fs (x%.2f)\n" t3 (t3 / t7)
  printf "  parseHsJSON(strict)     %.6fs (x%.2f)\n" t5 (t5 / t7)
  printf "  parseDynamic(strict)    %.6fs\n" t7
  printf "  Dynamic.parseJSON       %.6fs\n" t8

  printf "  reverse results %s\n"
    (if isEqual' then "match" else "mismatch" :: String)
  printf "  useValueAsDynamic       %.6fs (x%.2f)\n" t1' (t1' / t2')
  printf "  serializeValueToDynamic %.6fs\n" t2'

foreign import ccall unsafe "initializeJson"
  initializeJson :: CString -> IO ()

foreign import ccall unsafe "getJsonAsCStringLen"
  getJsonAsCStringLen :: Ptr CLong -> IO CString

foreign import ccall unsafe "getJsonAsPtrDynamic"
  getJsonAsPtrDynamic :: IO (Ptr Dynamic)

foreign import ccall unsafe "getJsonAsPtrHsJSON"
  getJsonAsPtrHsJSON :: IO (Ptr HsJSON)

foreign import ccall unsafe "compareDynamic"
  compareDynamic :: Ptr Dynamic -> Ptr Dynamic -> IO CLong

foreign import ccall unsafe "parseJSON"
  c_parseJSON :: CString -> CLong -> IO (Ptr Dynamic)
