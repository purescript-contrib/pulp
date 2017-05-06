module Pulp.Args.Help
       ( printHelp
       , printCommandHelp
       ) where

import Prelude

import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Except (runExcept)
import Data.Either (Either(..))
import Data.Array (sort, (!!), null)
import Data.Foldable (foldr, maximum)
import Data.Maybe (Maybe(..), fromMaybe, maybe, fromJust)
import Data.StrMap (StrMap(), keys, lookup, insert, empty)
import Data.String as Str
import Data.Traversable (sequence)
import Data.Foreign (F())
import Data.Foreign.Class (decode)
import Node.Process as Process
import Node.Path as Path

import Pulp.Args
import Pulp.Args.Types as Type
import Pulp.Outputter
import Pulp.System.FFI

import Partial.Unsafe

foreign import pad :: Int -> String

foreign import wrap :: String -> Int -> EffN String

formatTable :: StrMap String -> EffN String
formatTable table =
  let headers = sort $ keys table
      longest = fromMaybe 0 $ maximum $ headers <#> Str.length
      formatEntry key = unsafePartial $ fromJust (lookup key table) # \entry ->
          let padding = longest - Str.length key
          in do
            formatted <- wrap entry (longest + 4)
            pure ("  " <> key <> pad (padding + 2) <> formatted <> "\n")
  in do
    entries <- sequence $ headers <#> formatEntry
    pure $ Str.joinWith "" entries

describeOpt :: Option -> String
describeOpt opt = opt.desc <> case opt.defaultValue of
  Nothing -> ""
  Just def -> maybe "" (\d -> " [Default: " <> d <> "]") (tryDefault def)
  where
  tryDefault def =
    case runExcept (decode def :: F String) of
      Right str ->
        Just (show str)
      Left _ ->
        case runExcept (decode def :: F Int) of
          Right int ->
            Just (show int)
          Left _ ->
            Nothing

prepareOpts :: Array Option -> StrMap String
prepareOpts = foldr foldOpts empty
  where formatKey n = (Str.joinWith " " n.match) <> case n.parser.name of
          Nothing -> ""
          Just arg -> " " <> arg
        foldOpts n = insert (formatKey n) (describeOpt n)

formatOpts :: Array Option -> AffN String
formatOpts = liftEff <<< formatTable <<< prepareOpts

prepareCmds :: Array Command -> StrMap String
prepareCmds = foldr foldCmds empty
  where foldCmds n = insert n.name n.desc

formatCmds :: Array Command -> AffN String
formatCmds = liftEff <<< formatTable <<< prepareCmds

formatPassThrough :: Maybe String -> AffN String
formatPassThrough mdesc =
  let desc = fromMaybe "Passthrough options are ignored." mdesc
  in liftEff (wrap ("  " <> desc) 2)

prepareArguments :: Array Argument -> StrMap String
prepareArguments = foldr foldOpts empty
  where formatKey arg = Str.toUpper arg.name
        foldOpts arg = insert (formatKey arg) arg.desc

formatArguments :: Array Argument -> AffN String
formatArguments = liftEff <<< formatTable <<< prepareArguments

argumentSynopsis :: Array Argument -> String
argumentSynopsis = map format  >>> Str.joinWith " "
  where
  format arg =
    Str.toUpper $
      if arg.required
        then arg.name
        else "[" <> arg.name <> "]"

helpOpt :: Option
helpOpt = option "help" ["--help", "-h"] Type.flag
            "Show this help message."

printHelp :: Outputter -> Array Option -> Array Command -> AffN Unit
printHelp out globals commands = do
  commandName <- liftEff getCommandName
  out.write $ "Usage: " <> commandName <> " [global-options] <command> [command-options]\n"
  out.bolded "\nGlobal options:\n"
  formatOpts (globals <> [helpOpt]) >>= out.write
  out.bolded "\nCommands:\n"
  formatCmds commands >>= out.write
  helpText <- liftEff $ wrap ("Use `" <> commandName <>
                              " <command> --help` to " <>
                              "learn about command specific options.") 2
  out.write $ "\n" <> helpText <> "\n\n"

printCommandHelp :: Outputter -> Array Option -> Command -> AffN Unit
printCommandHelp out globals command = do
  commandName <- liftEff getCommandName
  out.write $ "Usage: " <> commandName <> " [global-options] " <>
                  command.name <> " " <>
                  (if hasArguments then argumentSynopsis command.arguments <> " " else "") <>
                  (if hasCommandOpts then "[command-options]" else "") <> "\n"
  out.bolded $ "\nCommand: " <> command.name <> "\n"
  out.write $ "  " <> command.desc <> "\n"
  when hasArguments do
    out.bolded "\nArguments:\n"
    formatArguments command.arguments >>= out.write
  when hasCommandOpts do
    out.bolded "\nCommand options:\n"
    formatOpts (command.options) >>= out.write
  out.bolded "\nGlobal options:\n"
  formatOpts (globals <> [helpOpt]) >>= out.write
  out.bolded "\nPassthrough options:\n"
  formatPassThrough command.passthroughDesc >>= out.write
  out.write "\n"

  where
  hasCommandOpts = not (null command.options)
  hasArguments = not (null command.arguments)

getCommandName :: EffN String
getCommandName = maybe "pulp" (_.name <<< Path.parse) <<< (_ !! 1) <$> Process.argv
