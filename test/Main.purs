module Test.Main where

import Prelude
import IPFS as IPFS
import Control.Monad.Aff (launchAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import IPFS (IPFSEff)

-- main :: forall e. Eff (console :: CONSOLE, ipfs :: IPFSEff | e) Unit
main :: Eff _ Unit
main = do
  ipfs <- IPFS.connect "localhost" 5001
  _ <- launchAff $ do
    ver <- IPFS.version ipfs
    ident <- IPFS.identity ipfs
    liftEff $ do
      log $ "version: " <> ver.version
      log $ "id: " <> ident.id

  -- log ver
  log "You should add some tests."
