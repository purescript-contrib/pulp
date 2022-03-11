module Pulp.Versions.PureScript where

import Data.List (List(..), (:))
import Data.List.NonEmpty as NEL
import Data.Version.Haskell (Version(..))

type PureScriptVersions =
  { v0_12_0 :: Version
  , v0_12_4 :: Version
  , v0_13_0 :: Version
  , v0_14_0 :: Version
  , v0_15_0 :: Version
  }

psVersions :: PureScriptVersions
psVersions =
  { v0_12_0: Version (NEL.cons' 0 (12 : 0 : Nil)) Nil
  , v0_12_4: Version (NEL.cons' 0 (12 : 4 : Nil)) Nil
  , v0_13_0: Version (NEL.cons' 0 (13 : 0 : Nil)) Nil
  , v0_14_0: Version (NEL.cons' 0 (14 : 0 : Nil)) Nil
  , v0_15_0: Version (NEL.cons' 0 (15 : 0 : Nil)) Nil
  }
