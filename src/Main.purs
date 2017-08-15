module Main where

import Prelude
import Control.Monad.Aff (launchAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Maybe (Maybe(..))
import Global.Unsafe (unsafeStringify)
import IPFS (IPFSEff)
import IPFS as IPFS
import IPFS.Files as Files
import IPFS.Types (IPFSPath(..))
import Node.Encoding (Encoding(..))
import Node.Stream as Stream
import Unsafe.Coerce (unsafeCoerce)

ipfsConnect = IPFS.connect

pathStr = "/ipfs/QmVLDAhCY3X9P2uRudKAryuQFPM5zqA3Yij1dY8FpGbL7T/quick-start"
path = IPFSPathString pathStr

-- main :: forall e. Eff (console :: CONSOLE | e) Unit
main = do
  ipfs <- IPFS.connect "localhost" 5001
  _ <- launchAff $ do
    ver <- IPFS.version ipfs
    ident <- IPFS.identity ipfs

    -- dat <- Files.cat ipfs (IPFSPathString "/ipfs/QmVLDAhCY3X9P2uRudKAryuQFPM5zqA3Yij1dY8FpGbL7T/quick-start")
    dat2 <- Files.cat2 ipfs pathStr

    -- dat' <- liftEff $ Stream.readString dat2 Nothing UTF8
    liftEff $ do
      log $ "version: " <> ver.version
      log $ "id: " <> ident.id
      log $ show $ unsafeStringify dat2

      -- case dat' of
      --   Nothing -> log "failed reading file"
      --   Just s  -> log s

  pure unit
  log "Hello sailor!"
