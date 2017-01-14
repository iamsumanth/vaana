{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
import GHC.Generics
import Data.Aeson
import Data.Text.Internal
import Control.Lens hiding ((.=))
import Network.Wreq
import qualified Data.ByteString.Lazy as Lazy

data Agents = Agents {
      hostname          :: String
    , uuid              :: String
    , agentConfigState  :: String
} deriving (Generic, Show)


instance FromJSON Agents where
    parseJSON = withObject "agents" $ \o -> do
        hostname <- o .: "hostname"
        uuid <- o .: "uuid"
        agentConfigState <- o .: "agent_config_state"
        return Agents{..}

instance ToJSON Agents where
    toJSON Agents{..} = object [
        "hostname" .= hostname,
        "uuid" .= uuid,
        "agent_config_state" .= agentConfigState ]



getAgentsResponse :: IO Lazy.ByteString
getAgentsResponse = do
    let opts = defaults & header "Accept" .~ ["application/vnd.go.cd.v4+json"] & header "Content-Type" .~ ["application/json"]
    resp <- getWith opts "http://localhost:8153/go/api/agents/uuid-1"
    let y = resp ^. responseBody
    return y


getAgentDetails = do
    x <- getAgentsResponse
    let z = decode x :: Maybe Agents
    return z
