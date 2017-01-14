{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
import GHC.Generics
import Data.Aeson
import Data.Text.Internal
import Control.Lens hiding ((.=))
import Network.Wreq
import qualified Data.ByteString.Lazy as Lazy


data Agent = Agent {
      hostname          :: String
    , uuid              :: String
    , agentConfigState  :: String
} deriving (Generic, Show)

instance FromJSON Agent where
    parseJSON = withObject "agent" $ \o -> do
        hostname <- o .: "hostname"
        uuid <- o .: "uuid"
        agentConfigState <- o .: "agent_config_state"
        return Agent{..}

instance ToJSON Agent where
    toJSON Agent{..} = object [
        "hostname" .= hostname,
        "uuid" .= uuid,
        "agent_config_state" .= agentConfigState ]



data Agents = Agents {
      agents :: [Agent]
} deriving (Generic, Show)

instance FromJSON Agents where
    parseJSON = withObject "agents" $ \o -> do
        let m = o .: "_embedded"
        k <-  m
        let agents = (k:[])
        return Agents{..}

{-
instance ToJSON Agents where
    toJSON Agents{..} = object [
        "agents" .= toJSON (agent) ]

    -}


getAgentResponse :: IO Lazy.ByteString
getAgentResponse = do
    let opts = defaults & header "Accept" .~ ["application/vnd.go.cd.v4+json"] & header "Content-Type" .~ ["application/json"]
    resp <- getWith opts "http://localhost:8153/go/api/agents/uuid-1"
    let y = resp ^. responseBody
    return y


getAgentsResponse :: IO Lazy.ByteString
getAgentsResponse = do
    let opts = defaults & header "Accept" .~ ["application/vnd.go.cd.v4+json"] & header "Content-Type" .~ ["application/json"]
    resp <- getWith opts "http://localhost:8153/go/api/agents"
    let y = resp ^. responseBody
    return y



getAgentDetails = do
    x <- getAgentResponse
    let z = decode x :: Maybe Agent
    return z


getAgents = do
    x <- getAgentsResponse
    let z = decode x :: Maybe Agents
    return z

