{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}

module Medication where

import Aws.Lambda
import Network.AWS
import Network.AWS.DynamoDB
import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Control.Lens
import GHC.Generics

data MedicationRequest = MedicationRequest
    { patientId     :: Text
    , dsm5Diagnosis :: Text
    } deriving (Show, Generic)

instance FromJSON MedicationRequest
instance ToJSON MedicationRequest

main :: IO ()
main = lambdaMain handler

handler :: MedicationRequest -> Context -> IO (Either String String)
handler req _ = do
    env <- newEnv Discover
    let medication = assignMedication $ dsm5Diagnosis req
    let update = updateItem "Patients"
                 & uiKey .~ key
                 & uiUpdateExpression ?~ "set Medication = :m"
                 & uiExpressionAttributeValues .~ attrValues medication
    runResourceT . runAWS env $ send update
    return $ Right "Medication assigned successfully!"
  where
    key = [("PatientID", attributeValue & avS ?~ patientId req)]
    attrValues medication = [(":m", attributeValue & avS ?~ medication)]

assignMedication :: Text -> Text
assignMedication "F32.0" = "Fluoxetine"
assignMedication "F41.1" = "Sertraline"
assignMedication _       = "Consultation required"
