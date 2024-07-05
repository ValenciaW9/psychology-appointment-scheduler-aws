{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}

module Main where

import Aws.Lambda
import Network.AWS
import Network.AWS.DynamoDB
import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Control.Lens
import GHC.Generics
import qualified Data.HashMap.Strict as HM

data Appointment = Appointment
    { appointmentId :: Text
    , patientId     :: Text
    , dateTime      :: Text
    , psychologist  :: Text
    } deriving (Show, Generic)

instance FromJSON Appointment
instance ToJSON Appointment

main :: IO ()
main = lambdaMain handler

handler :: Appointment -> Context -> IO (Either String String)
handler appointment _ = do
    env <- newEnv Discover
    let req = putItem "Appointments"
                & piItem .~ item
    runResourceT . runAWS env $ send req
    return $ Right "Appointment created successfully!"
  where
    item = toItem appointment

toItem :: Appointment -> HM.HashMap Text AttributeValue
toItem Appointment{..} = 
    [ "AppointmentID" .= appointmentId
    , "PatientID"     .= patientId
    , "DateTime"      .= dateTime
    , "Psychologist"  .= psychologist
    ]
