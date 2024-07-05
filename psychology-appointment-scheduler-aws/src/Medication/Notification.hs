{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}

module Medication.Notification where

import Aws.Lambda
import Network.AWS
import Network.AWS.SNS.Publish
import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Control.Lens
import GHC.Generics
import qualified Data.HashMap.Strict as HM

data Notification = Notification
    { message    :: Text
    , phoneNumber :: Text
    } deriving (Show, Generic)

instance FromJSON Notification
instance ToJSON Notification

main :: IO ()
main = lambdaMain handler

handler :: Notification -> Context -> IO (Either String String)
handler Notification{..} _ = do
    env <- newEnv Discover
    let req = publish message
                & pPhoneNumber ?~ phoneNumber
    runResourceT . runAWS env $ send req
    return $ Right "Notification sent successfully!"
