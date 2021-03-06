{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
import GHC.Generics
import Data.Aeson
import Data.Text.Internal
import Data.Maybe
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
        embedded <- o .: "_embedded"
        extractedAgents <-  embedded .: "agents"
        let agents = [agent | agent <- extractedAgents]
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



getAgent = do
    x <- getAgentResponse
    let z = decode x :: Maybe Agent
    return z


getAgents = do
    x <- getAgentsResponse
    let z = decode x :: Maybe Agents
    return z


getHostName = do
    x <- getAgent
    let k = hostName x
    return k


hostName agent = do
    (Agent hostname _ _) <- agent
    Just(hostname)


getConfigState = do
    x <- getAgent
    let k = configState x
    return k


configState agent = do
    (Agent _ _ agentConfigState) <- agent
    Just(agentConfigState)


getEnabledAgents = do
    x <- getAgents
    let k = enabledAgents x
    return k

enabledAgents totalAgents = do
    (Agents agents) <- totalAgents
    let hostName = [hostName | (Agent hostName _ configState) <- agents, configState == "Enabled"]
    Just(hostName)


getDisabledAgents = do
    x <- getAgents
    let k = disabledAgents x
    return k

disabledAgents totalAgents = do
    (Agents agents) <- totalAgents
    let hostName = [hostName | (Agent hostName _ configState) <- agents, configState == "Disabled"]
    Just(hostName)





