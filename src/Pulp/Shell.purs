
module Pulp.Shell (shell) where

import Prelude
import Data.Maybe (Maybe(..))
import Control.Monad.Eff.Class (liftEff)
import Node.Encoding (Encoding(UTF8))
import Node.Buffer as Buffer
import Node.FS.Aff as FS

import Pulp.Exec
import Pulp.System.FFI
import Pulp.System.Files (openTemp)
import Pulp.System.Process (getPlatform)
import Pulp.Outputter

shell :: forall e. Outputter e -> String -> AffN e Unit
shell out cmd = do
  platform <- liftEff getPlatform
  if platform == "win32"
    then shell' out cmd { extension: ".cmd", executable: "cmd" }
    else shell' out cmd { extension: ".sh", executable: "sh" }

type ShellOptions = { extension :: String, executable :: String }

shell' :: forall e. Outputter e -> String -> ShellOptions -> AffN e Unit
shell' out cmd opts = do
  out.log $ "Executing " ++ cmd
  cmdBuf <- liftEff $ Buffer.fromString cmd UTF8
  info <- openTemp { prefix: "pulp-cmd-", suffix: opts.extension }
  FS.fdAppend info.fd cmdBuf
  FS.fdClose info.fd
  exec opts.executable [info.path] Nothing
  out.log "Done."
