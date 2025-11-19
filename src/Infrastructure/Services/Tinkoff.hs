{-# LANGUAGE OverloadedStrings #-}

module Infrastructure.Services.Tinkoff (generatePaymentLink, module Types) where

import Data.Text (Text)

import App (AppM)
import Infrastructure.Services.Tinkoff.Types as Types


generatePaymentLink :: AppM (Either TinkoffError Text)
generatePaymentLink = return $ Right "....."