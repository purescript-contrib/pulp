module Pulp.Args.Parser where

import Prelude

import Control.Alt
import Control.Monad.Aff
import Control.Monad.Eff.Exception
import Control.Monad.Error.Class
import Control.Monad.Trans
import Data.Array (many)
import Data.Either (Either(..))
import Data.Foldable (find, elem)
import Data.List (List(..))
import Data.Maybe (Maybe(..))
import Data.Tuple (Tuple(..))

import qualified Data.Map as Map

import Text.Parsing.Parser
import Text.Parsing.Parser.Combinators ((<?>), try)
import Text.Parsing.Parser.Token (token)

import Pulp.Args
import Pulp.System.FFI (AffN(..))

halt :: forall a. String -> OptParser a
halt err = lift $ throwError $ error err
-- halt err = ParserT $ \s ->
--   return { consumed: true, input: s, result: Left (strMsg err) }

matchNamed :: forall a r. (Eq a) => { name :: a | r } -> a -> Boolean
matchNamed o key = o.name == key

matchOpt :: Option -> String -> Boolean
matchOpt o key = elem key o.match

lookup :: forall m a b. (Monad m, Eq b, Show b) => (a -> b -> Boolean) -> Array a -> ParserT (List b) m (Tuple b a)
lookup match table = do
  next <- token
  next :: b
  case find (\i -> match i next) table of
    Just entry -> return $ Tuple next entry
    Nothing -> fail ("Unknown command: " ++ show next)

lookupOpt :: Array Option -> OptParser (Tuple String Option)
lookupOpt = lookup matchOpt

lookupCmd :: Array Command -> OptParser (Tuple String Command)
lookupCmd = lookup matchNamed

opt :: Array Option -> OptParser Options
opt opts = do
  o <- lookupOpt opts
  case o of
    (Tuple key option) -> do
      val <- option.parser.parser key
      return $ Map.singleton option.name val

cmd :: Array Command -> OptParser Command
cmd cmds = do
  o <- lookupCmd cmds <?> "command"
  case o of
    (Tuple key option) -> return option

parseArgv :: Array Option -> Array Command -> OptParser Args
parseArgv globals commands = do
  globalOpts <- many $ try $ opt globals
  command <- cmd commands
  commandOpts <- many $ try $ opt command.options
  rest <- many token
  return $ {
    globalOpts: Map.unions globalOpts,
    command: command,
    commandOpts: Map.unions commandOpts,
    remainder: rest
    }

parse :: forall e. Array Option -> Array Command -> Array String -> AffN e (Either ParseError Args)
parse globals commands s =
  runParserT s $ parseArgv globals commands
