module Pulp.Args.Help
       ( printHelp
       , printCommandHelp
       ) where

import Prelude

import Control.Monad.Eff.Class (liftEff)
import Data.Either (Either(..))
import Data.Array (sort)
import Data.Foldable (foldr, maximum)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Maybe.Unsafe (fromJust)
import Data.StrMap (StrMap(), keys, lookup, insert, empty)
import Data.String as Str
import Data.Traversable (sequence)
import Data.Foreign (F())
import Data.Foreign.Class (read)

import Pulp.Args
import Pulp.Args.Types as Type
import Pulp.Outputter
import Pulp.System.FFI
import Pulp.System.Process (commandName)

foreign import pad :: Int -> String

foreign import wrap :: forall e. String -> Int -> EffN String

formatTable :: forall e. StrMap String -> EffN String
formatTable table =
  let headers = sort $ keys table
      longest = fromMaybe 0 $ maximum $ headers <#> Str.length
      formatEntry key = fromJust (lookup key table) # \entry ->
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
  Just def -> maybe "" (\d -> " [Default: " ++ d ++ "]") (tryDefault def)
  where
  tryDefault def =
    case read def :: F String of
      Right str ->
        Just (show str)
      Left _ ->
        case read def :: F Int of
          Right int ->
            Just (show int)
          Left _ ->
            Nothing

prepareOpts :: Array Option -> StrMap String
prepareOpts = foldr foldOpts empty
  where formatKey n = (Str.joinWith " " n.match) ++ case n.parser.name of
          Nothing -> ""
          Just arg -> " " ++ arg
        foldOpts n = insert (formatKey n) (describeOpt n)

formatOpts :: forall e. Array Option -> AffN String
formatOpts = liftEff <<< formatTable <<< prepareOpts

prepareCmds :: Array Command -> StrMap String
prepareCmds = foldr foldCmds empty
  where foldCmds n = insert n.name n.desc

formatCmds :: forall e. Array Command -> AffN String
formatCmds = liftEff <<< formatTable <<< prepareCmds

helpOpt :: Option
helpOpt = option "help" ["--help", "-h"] Type.flag
            "Show this help message."

printHelp :: forall e. Outputter e -> Array Option -> Array Command -> AffN Unit
printHelp out globals commands = do
  out.write $ "Usage: " ++ commandName ++ " [global-options] <command> [command-options]\n"
  -- if context print command docs
  out.bolded "\nGlobal options:\n"
  formatOpts (globals ++ [helpOpt]) >>= out.write
  out.bolded "\nCommands:\n"
  formatCmds commands >>= out.write
  helpText <- liftEff $ wrap ("Use `" ++ commandName ++
                              " <command> --help` to " ++
                              "learn about command specific options.") 2
  out.write $ "\n" ++ helpText ++ "\n\n"

printCommandHelp :: forall e. Outputter e -> Array Option -> Command -> AffN Unit
printCommandHelp out globals command = do
  out.write $ "Usage: " ++ commandName ++ " [global-options] " ++
                  command.name ++ " [command-options]\n"
  out.bolded $ "\nCommand: " ++ command.name ++ "\n"
  out.write $ "  " ++ command.desc ++ "\n"
  out.bolded "\nCommand options:\n"
  formatOpts (command.options) >>= out.write
  out.bolded "\nGlobal options:\n"
  formatOpts (globals ++ [helpOpt]) >>= out.write
  out.write "\n"
