{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
module Shipping.Booking where

import Core
import Integration.Http qualified as Http
import Json qualified

data Request = Request { reference :: Text, quantity :: Int }
  deriving (Generic)
instance Json.ToJSON Request

data Booking = Booking { status :: Text }
  deriving (Eq, Show, Generic)
instance Json.FromJSON Booking

request :: Text -> Request -> (Result Text Booking -> command) -> Http.Request command
request endpoint input toCommand = Http.Request {
    method = Http.POST,
    url = endpoint,
    headers = [("Idempotency-Key", input.reference)],
    body = Http.json input,
    onSuccess = \response -> toCommand (decode response),
    onError = Just (\_ -> toCommand (Err "Shipping provider could not be reached")),
    auth = Http.NoAuth,
    retry = Http.noRetry,
    timeoutSeconds = 3
  }

decode :: Http.Response -> Result Text Booking
decode response =
  if response.statusCode < 200 || response.statusCode >= 300
    then Err "Shipping provider rejected the request"
    else case Json.decode @Booking response.body of
      Err _ -> Err "Shipping provider returned an invalid booking"
      Ok booking -> Ok booking
