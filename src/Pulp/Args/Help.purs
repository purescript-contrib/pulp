module Pulp.Args.Help
       ( printHelp
       ) where

import Control.Monad.Eff.Class (liftEff)
import Data.Array (sort)
import Data.Foldable (foldr)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.StrMap (StrMap(..), keys, lookup, insert, empty)
import qualified Data.String as Str
import Data.Traversable (sequence)

import Pulp.Args
import qualified Pulp.Args.Types as Type
import Pulp.Data.Foldable (max)
import Pulp.System.Ansi
import Pulp.System.FFI
import Pulp.System.Process (stderr, commandName)
import Pulp.System.Stream (write)

foreign import pad """
  function pad(n) {
    return new Array(n + 1).join(" ");
  }""" :: Number -> String

foreign import wrap """
  function wrap(s) {
    return function(indent) {
      return function() {
        var cols = process.stdout.columns;
        return cols ? require("wordwrap")(indent, cols)(s).slice(indent) : s;
      };
    };
  }""" :: forall e. String -> Number -> EffN e String

formatTable :: forall e. StrMap String -> EffN e String
formatTable table =
  let headers = sort $ keys table
      longest = fromMaybe 0 $ max $ headers <#> Str.length
      formatEntry key = case lookup key table of
        (Just entry) ->
          let padding = longest - Str.length key
          in do
            formatted <- wrap entry (longest + 4)
            return ("  " ++ key ++ pad (padding + 2) ++ formatted ++ "\n")
  in do
    entries <- sequence $ headers <#> formatEntry
    return $ Str.joinWith "" entries

describeOpt :: Option -> String
describeOpt opt = opt.desc ++ case opt.defaultValue of
  Nothing -> ""
  Just def -> " [Default: \"" ++ def ++ "\"]"

prepareOpts :: [Option] -> StrMap String
prepareOpts = foldr foldOpts empty
  where formatKey n = (Str.joinWith " " n.match) ++ case n.parser.name of
          Nothing -> ""
          Just arg -> " " ++ arg
        foldOpts n = insert (formatKey n) (describeOpt n)

formatOpts :: forall e. [Option] -> AffN e String
formatOpts = liftEff <<< formatTable <<< prepareOpts

prepareCmds :: [Command] -> StrMap String
prepareCmds = foldr foldCmds empty
  where foldCmds n = insert n.name n.desc

formatCmds :: forall e. [Command] -> AffN e String
formatCmds = liftEff <<< formatTable <<< prepareCmds

helpOpt :: Option
helpOpt = option "help" ["--help", "-h"] Type.flag
            "Show this help message."

printHelp :: forall e. Ansi -> [Option] -> [Command] -> AffN e Unit
printHelp stream globals commands = do
  write stream $ "Usage: " ++ commandName ++ " [global-options] <command> [command-options]\n"
  -- if context print command docs
  bold stream
  write stream "\nGlobal options:\n"
  reset stream
  formatOpts (globals ++ [helpOpt]) >>= write stream
  bold stream
  write stream "\nCommands:\n"
  reset stream
  formatCmds commands >>= write stream
  helpText <- liftEff $ wrap ("Use `" ++ commandName ++
                              " <command> --help` to " ++
                              "learn about command specific options.") 2
  write stream $ "\n" ++ helpText ++ "\n\n"
