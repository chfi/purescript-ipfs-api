module Test.Main where

import Prelude
import IPFS as IPFS
import IPFS.Files as Files
import Node.Stream as Stream
import Control.Monad.Aff (launchAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Maybe (Maybe(..))
import IPFS (IPFSEff)
import IPFS.Types (IPFSPath(..))
import Node.Encoding (Encoding(..))

-- main :: forall e. Eff (console :: CONSOLE, ipfs :: IPFSEff | e) Unit
main :: Eff _ Unit
main = do
  ipfs <- IPFS.connect "localhost" 5001
  _ <- launchAff $ do
    ver <- IPFS.version ipfs
    ident <- IPFS.identity ipfs

    dat <- Files.cat ipfs (IPFSPathString "/ipfs/QmUNLLsPACCz1vLxQVkXqqLX5R1X345qqfHbsf67hvA3Nn")
    -- dat' <- liftEff $ Stream.readString dat Nothing UTF8
    liftEff $ do
      log $ "version: " <> ver.version
      log $ "id: " <> ident.id
      -- case dat' of
      --   Nothing -> log "failed reading file"
      --   Just s  -> log s

  pure unit
