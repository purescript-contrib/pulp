
module Pulp.Version ( version ) where

import Prelude
import Data.Version as Version
import Data.List (List(..))

-- | TODO: get this from package.json or git tags or something.
version :: Version.Version
version = Version.version 6 0 0 Nil Nil
