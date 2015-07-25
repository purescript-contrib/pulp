module Pulp.System.Log
       ( out
       , print
       , log
       , err
       ) where

import Prelude

import Pulp.System.Ansi
import Pulp.System.FFI
import Pulp.System.Process (stderr)
import Pulp.System.Stream (write)

out :: Ansi
out = ansi stderr

print :: forall e. String -> AffN e Unit
print text = write out text

bullet :: forall e. String -> String -> AffN e Unit
bullet colour text = do
  col out colour
  bold out
  write out "* "
  reset out
  write out $ text ++ "\n"

log :: forall e. String -> AffN e Unit
log text = bullet "green" text

err :: forall e. String -> AffN e Unit
err text = bullet "red" text
