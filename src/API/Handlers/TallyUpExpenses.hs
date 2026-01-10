{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module API.Handlers.TallyUpExpenses (handler) where

import Data.Bifunctor (first)
import Control.Monad.Reader.Class (ask)
import Data.Text (Text)

import App (AppM, _appDBPool)
import API.Types (ApiResponse, mkError, Expenses)
import Infrastructure.Database (tallyUpExpenses)


handler :: Expenses -> AppM (ApiResponse ())
handler expenses = fmap (first mkError . handleResp) $ fmap _appDBPool ask >>= (tallyUpExpenses expenses)
{-# INLINE handler #-}


handleResp :: Either Text Bool -> Either Text ()
handleResp (Left err) = Left err
handleResp (Right rowsAffected) | rowsAffected = Right ()
                                | otherwise = Left "user not found"