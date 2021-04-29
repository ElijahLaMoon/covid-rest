{-# LANGUAGE OverloadedStrings #-}

module CovidRegistration where

import Text.Blaze.Html ( toHtml, Html )
import Text.Blaze.Html5 as H
    ( body, docTypeHtml, h1, h2, head, li, title, ul )
import Data.ByteString ( ByteString )
import Data.ByteString.UTF8 ( toString )

successfulRegistration :: ByteString -> ByteString -> ByteString -> Html
successfulRegistration userName userTel userCovid = docTypeHtml $ do
    H.head $ do
      H.title "Shabak COVID-19 Registration"
    body $ do
        h1 "Shabak COVID-19 Registration"
        h2 "Registration successful:"
        let element = li . toHtml . toString
        ul $
          element ("Name: " <> userName) <>
          element ("Tel: " <> userTel) <>
          element ("Covid-19: " <> userCovid)
