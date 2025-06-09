module ZkFold.Bitcoin.Provider.Node.Exception (
  NodeProviderException (..),
)
where

import Control.Exception (Exception)
import Data.Aeson (Value)
import Data.Text (Text)
import Servant.Client (ClientError)

-- | Exceptions.
data NodeProviderException
  = -- | Error from the Node API.
    NodeApiError !Text !ClientError
  | -- | Received error response.
    NodeErrorResponse !Text !Value
  deriving stock (Eq, Show)
  deriving anyclass (Exception)