
module Pulp.Shell (shell) where

import Prelude

import Data.Maybe (Maybe(..))
import Effect.Aff (Aff)
import Effect.Class (liftEffect)
import Node.Buffer as Buffer
import Node.Encoding (Encoding(UTF8))
import Node.FS.Aff as FS
import Node.Platform (Platform(Win32))
import Node.Process as Process
import Pulp.Exec (exec)
import Pulp.Outputter (Outputter)
import Pulp.System.Files (openTemp)

shell :: Outputter -> String -> Aff Unit
shell out cmd = do
  if Process.platform == Just Win32
    then shell' out cmd
            { extension: ".cmd"
            , executable: "cmd"
            , extraArgs: ["/s", "/c"]
            }
    else shell' out cmd
            { extension: ".sh"
            , executable: "sh"
            , extraArgs: []
            }

type ShellOptions =
  { extension :: String
  , executable :: String
  , extraArgs :: Array String
  }

shell' :: Outputter -> String -> ShellOptions -> Aff Unit
shell' out cmd opts = do
  out.log $ "Executing " <> cmd
  cmdBuf <- liftEffect $ Buffer.fromString cmd UTF8
  info <- openTemp { prefix: "pulp-cmd-", suffix: opts.extension }
  _ <- FS.fdAppend info.fd cmdBuf
  _ <- FS.fdClose info.fd
  exec opts.executable (opts.extraArgs <> [info.path]) Nothing
  out.log "Done."
