module Debug where

import Prelude

-- | A simple passthrough log function
foreign import log :: forall a. String -> a -> a
