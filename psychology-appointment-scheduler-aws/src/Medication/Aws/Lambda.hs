{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
module Medication
  ( lambdaMain
  , Context(..)
  , APIGatewayRequest(..)
  , APIGatewayResponse(..)
  ) where
import Data.Version
import GHC.Generics
import Data.Text (Text)
import qualified Data.Map.Strict as HM

-- Define a data type for the Lambda context
data Context = Context
  { awsRequestId :: Text
  , logGroupName :: Text
  , logStreamName :: Text
  , functionName :: Text
  , memoryLimitInMB :: Int
  , functionVersion :: Text
  , invokedFunctionArn :: Text
  } deriving (Show, Generic)

instance FromJSON Context
instance ToJSON Context

-- Define a data type for API Gateway requests
data APIGatewayRequest = APIGatewayRequest
  { body       :: Maybe Text
  , headers    :: Maybe (HM.HashMap Text Text)
  , httpMethod :: Text
  , path       :: Text
  , queryStringParameters :: Maybe (HM.HashMap Text Text)
  , pathParameters :: Maybe (HM.HashMap Text Text)
  , stageVariables :: Maybe (HM.HashMap Text Text)
  , requestContext :: Maybe Value
  , isBase64Encoded :: Maybe Bool
  } deriving (Show, Generic)

instance FromJSON APIGatewayRequest
instance ToJSON APIGatewayRequest

-- Define a data type for API Gateway responses
data APIGatewayResponse = APIGatewayResponse
  { statusCode :: Int
  , headers    :: Maybe (HM.HashMap Text Text)
  , body       :: Maybe Text
  , isBase64Encoded :: Maybe Bool
  } deriving (Show, Generic)

instance FromJSON APIGatewayResponse
instance ToJSON APIGatewayResponse

-- Define the entry point for the Lambda function
lambdaMain :: (FromJSON event, ToJSON result) => (event -> Context -> IO (Either String result)) -> IO ()
lambdaMain handler = runLambda $ \event ctx -> do
  context <- parseContext ctx
  handler event context

parseContext :: HM.HashMap Text Value -> IO Context
parseContext ctx = do
  let requestId = ctx HM.! "awsRequestId"
      logGroup = ctx HM.! "logGroupName"
      logStream = ctx HM.! "logStreamName"
      functionName = ctx HM.! "functionName"
      memoryLimit = ctx HM.! "memoryLimitInMB"
      functionVersion = ctx HM.! "functionVersion"
      invokedArn = ctx HM.! "invokedFunctionArn"
  return Context
    { awsRequestId = requestId
    , logGroupName = logGroup
    , logStreamName = logStream
    , functionName = functionName
    , memoryLimitInMB = memoryLimit
    , functionVersion = functionVersion
    , invokedFunctionArn = invokedArn
    }
