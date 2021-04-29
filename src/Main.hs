{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}

module Main where

import Network.Wai
import Network.HTTP.Types
import Network.Wai.Handler.Warp ( run )
import CovidRegistration
import Text.Blaze.Html.Renderer.Pretty
import qualified Data.ByteString as BS ( ByteString, null )
import Data.ByteString.Lazy as LazyBS ( null )
import Data.ByteString.Lazy.UTF8 as LazyBS ( ByteString, fromString )
import Data.Maybe ( fromMaybe, isJust )
import Data.Text ( Text )
import GHC.IO
import GHC.Generics
import Data.String ( IsString )
import Data.Char ( toLower, toUpper )
import Data.List ( find )

import Data.Aeson

main :: IO ()
main = do
  let port = 3434
  putStrLn $ "Server is running on http://localhost:" <> show port
  run port covidRegApp

covidRegApp :: Application
covidRegApp request respond =
  case (parseMethod . requestMethod) request of
    Left e -> respond $ response500 textPlainHeader "Internal server error"
    Right method -> case method of

      GET  ->
        if isGetPathCorrect $ pathInfo request
          then respond . processGetQuery $ queryString request
          else invalidPath

      POST ->
        if isPostPathCorrect $ pathInfo request
          then do
            body <- lazyRequestBody request
            respond . processPostBody $ body
          else invalidPath

      _    -> respond $ response400 textPlainHeader "Invalid method in the request"
      where
        invalidPath = respond $ response400 textPlainHeader "Invalid path"

processGetQuery :: Query -> Response
processGetQuery q
  | Prelude.null q    = responseFile status200 textHtmlHeader "static/covid19.html" Nothing
  | otherwise = processGetQuery' q
  where
    isNameAndTelKeysValid :: BS.ByteString -> BS.ByteString -> Bool
    isNameAndTelKeysValid nameKey telKey = nameKey == "name" && telKey == "tel"

    responseTemplate :: BS.ByteString -> BS.ByteString -> BS.ByteString -> Response
    responseTemplate b1 b2 b3 = response200 textHtmlHeader . fromString . renderHtml $
      successfulRegistration b1 b2 b3

    findKey :: Eq a => a -> [(a, b)] -> Maybe (a, b)
    findKey arg list = find (\(k, _) -> k == arg) list

    processGetQuery' :: Query -> Response
    processGetQuery' q =
        let 
          nameKey  = findKey "name" q
          telKey   = findKey "tel" q
          covidKey = findKey "covid19" q

          extractQueryValue (Just (_, Just v)) = v

          extractNameAndTelValues =
            (extractQueryValue nameKey, extractQueryValue telKey)
        in 
          case (isJust nameKey, isJust telKey, isJust covidKey) of
            (True, True, True) -> 
              let
                (n, t) = extractNameAndTelValues
                c = extractQueryValue covidKey 
              in 
                if c == "on"
                  then responseTemplate n t "true"
                  else response400 textHtmlHeader "Invalid value in covid19 parameter"

            (True, True, False) ->
              let 
                (n, t) = extractNameAndTelValues
              in responseTemplate n t "false"

            _                   -> response400 textPlainHeader "Query is ill-formed"

processPostBody :: LazyBS.ByteString -> Response
processPostBody body =
  let json = decode body :: Maybe Covid19
  in case json of
    Just j -> response200 (contentTypeHeader "application/json") (encode . covidConverter $ j)
    Nothing -> response400 textPlainHeader "Invalid request body"

-- Covid19 data types

data Covid19 =
  Covid19 { name    :: Text
          , tel     :: Text
          , covid19 :: Bool
          } deriving (Show, Eq, Generic)

data Covid19Status =
  Covid19Status { name    :: Text
                , tel     :: Text
                , covid19 :: Bool
                , status  :: Text
                } deriving (Show, Eq, Generic)

instance ToJSON Covid19 where
  toEncoding = genericToEncoding defaultOptions
instance ToJSON Covid19Status where
  toEncoding = genericToEncoding defaultOptions

instance FromJSON Covid19
instance FromJSON Covid19Status

covidConverter :: Covid19 -> Covid19Status
covidConverter (Covid19 n t c) = Covid19Status n t c "ok"

-- Utility functions

response200, response400, response500 :: ResponseHeaders -> LazyBS.ByteString -> Response
response200 = responseLBS status200
response400 = responseLBS status400
response500 = responseLBS status500

isGetPathCorrect, isPostPathCorrect :: [Text] -> Bool
isGetPathCorrect =
  isPathCorrect (\path -> length path == 1 && head path == "covid19.html")
isPostPathCorrect =
  isPathCorrect (\path -> length path == 2 && head path == "api" && (head . tail) path == "covid19")

isPathCorrect :: ([a] -> Bool) -> [a] -> Bool
isPathCorrect predicate = predicate

textHtmlHeader, textPlainHeader :: ResponseHeaders
textHtmlHeader = contentTypeHeader "text/html"
textPlainHeader = contentTypeHeader "text/plain"

contentTypeHeader :: IsString a => a -> [(HeaderName, a)]
contentTypeHeader v = [(hContentType, v)]
