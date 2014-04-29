{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import           Control.Applicative        ((<$>), (<*>))
import qualified Control.Exception          as Ex
import qualified Data.Configurator          as C
import           Data.Monoid
import qualified Database.PostgreSQL.Simple as PG
import qualified Options.Applicative        as OA

import qualified Fynder.Command             as FC
import           Fynder.Conf                (fynderConfFromConfigurator)
import           Fynder.Test.Fixture        (loadFixture1)
import           Fynder.Types

--------------------------------------------------------------------------------

main :: IO ()
main = do
    cmdOpts <- OA.execParser $ OA.info (OA.helper <*> pCmdOpts) mempty
    fconf <- C.load [C.Required $ coConfigFile cmdOpts]
                >>= fynderConfFromConfigurator

    Ex.bracket
       (PG.connectPostgreSQL $ _fconfDbConnectString fconf)
       (PG.close)
       (\conn -> do
           putStrLn "If you get a pattern-match exception, make sure your\
                    \ database is empty"
           FC.Outcome{FC._oResult=Right r} <- FC.run conn RoleRoot loadFixture1
           print r
           putStrLn "OK")


-------------------------------------------------------------------------------
-- Commandline configuration options

data CmdOpts = CmdOpts { coConfigFile :: FilePath }
  deriving (Eq, Show, Read)

pCmdOpts :: OA.Parser CmdOpts
pCmdOpts = CmdOpts
   <$> OA.strOption (OA.long "conf"     <>
                     OA.short 'c'       <>
                     OA.metavar "FILE"  <>
                     OA.help "Main configuration file")
