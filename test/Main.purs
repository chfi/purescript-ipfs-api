module Test.Main where

import Prelude

import Control.Monad.Aff (launchAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Data.Array (init, zipWith)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Traversable (sequence, sequence_, traverse, traverse_)
import Global.Unsafe (unsafeStringify)
import IPFS (IPFSEff)
import IPFS as IPFS
import IPFS.Files as Files
import IPFS.Types (IPFSPath(..))
import Node.Buffer as Buffer
import Node.Encoding (Encoding(..))
import Node.Stream as Stream
import Unsafe.Coerce (unsafeCoerce)


pathStr = "/ipfs/QmVLDAhCY3X9P2uRudKAryuQFPM5zqA3Yij1dY8FpGbL7T/quick-start"
path = IPFSPathString pathStr


main :: Eff _ Unit
main = do
  ipfs <- IPFS.connect "localhost" 5001
  _ <- launchAff $ do
    ver <- IPFS.version ipfs
    ident <- IPFS.identity ipfs

    let strFile f = liftEff $ Stream.onData f \bfr -> do
          log "reading file"
          log =<< Buffer.toString UTF8 bfr

        endFile f = liftEff $ Stream.onEnd f $ log "end file"


    liftEff $ log "----- Read existing file -----"
    stream <- Files.cat ipfs path
    strFile stream
    endFile stream


    liftEff $ log "----- Write file -----"
    buffers <- traverse (\x -> liftEff $ Buffer.fromString x UTF8)
                ["this is a test file", "another test file"]
    let paths = ["/tmp/testfile.txt", "/tmp/testfile2.txt"]
    results <- Files.add ipfs (zipWith (\path content -> {path, content}) paths buffers)

    -- removing the last result path since it's the /tmp/ directory
    let paths = map (IPFSPathString <<< _.hash) $ fromMaybe [] (init results)

    liftEff $ log "----- Read new file ----"
    traverse_ (strFile <=< Files.cat ipfs) paths

    liftEff $ do
      log $ "version: " <> ver.version
      log $ "id: " <> ident.id

  pure unit
