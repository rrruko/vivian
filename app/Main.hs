{-# LANGUAGE OverloadedStrings #-}

module Main where

import Lib

import Web.Scotty
import Data.Monoid (mconcat)
import Control.Monad.IO.Class (liftIO)

import Data.Semigroup
import Data.Text
import Text.Blaze
import Text.Blaze.Html5 hiding (html, main, param)
import Text.Blaze.Html5.Attributes hiding (form)
import Text.Blaze.Html.Renderer.Text


main :: IO ()
main = scotty 3000 $ do
    get  "/:word" doWord
    post "/:word" doPost

doWord :: ActionM ()
doWord = do
    beam <- param "word"
    html . renderHtml $ mkPage beam

mkPage :: Text -> Html
mkPage beam = 
    h1 (toHtml $ "Scotty, " <> beam <> " me up!") <>
    (form ! method "post" $
        input ! type_ "text" ! name "hewwo" <> br <>
	input ! type_ "text" ! name "fucko" <> br <>
	input ! type_ "submit")

doPost :: ActionM ()
doPost = do
    p <- param "hewwo"
    liftIO $ putStrLn p
    doWord
