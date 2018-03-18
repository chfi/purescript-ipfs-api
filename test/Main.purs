module Test.Main where

import Prelude

import Control.Coroutine (Consumer, Producer, Transformer, await, consumer, emit, pullFrom, runProcess, transform, ($$), ($~), (~$))
import Control.Coroutine.Aff (produce')
import Control.Monad (when)
import Control.Monad.Aff (Aff, launchAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (CONSOLE, log)
import Control.Monad.Rec.Class (forever)
import Control.Monad.Trans.Class (lift)
import Data.Argonaut (_Number, _Object, _String, jsonParser)
import Data.Array (init, zipWith)
import Data.Either (Either(..), either)
import Data.Int (even)
import Data.Lens ((^?))
import Data.Lens.Index (ix)
import Data.Maybe (Maybe(..), fromMaybe, maybe)
import Data.Profunctor (rmap)
import Data.Traversable (sequence, sequence_, traverse, traverse_)
import Global.Unsafe (unsafeStringify)
import IPFS (IPFSEff)
import IPFS as IPFS
import IPFS.Files (IPFSObject)
import IPFS.Files as Files
import IPFS.Block as Block
import IPFS.Block (defaultPutOpts)
import IPFS.Types (IPFSPath(..))
import Node.Buffer (fromString)
import Node.Buffer as Buffer
import Node.Encoding (Encoding(..))
import Node.Stream as Stream
import Test.Spec.Assertions (shouldEqual)
import Unsafe.Coerce (unsafeCoerce)


pathStr = "/ipfs/QmVLDAhCY3X9P2uRudKAryuQFPM5zqA3Yij1dY8FpGbL7T/quick-start"
path = IPFSPathString pathStr



main :: Eff _ Unit
main = do
  ipfs <- IPFS.connect "localhost" 5001
  _ <- launchAff $ do
    ver <- IPFS.version ipfs
    ident <- IPFS.identity ipfs

    liftEff $ do
      log $ "version: " <> ver.version
      log $ "id: " <> ident.id

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
    let hashes = map (IPFSPathString <<< _.hash) $ fromMaybe [] (init results)

    liftEff $ log "----- Read new file ----"
    traverse_ (strFile <=< Files.cat ipfs) hashes


    liftEff $ log "----- write JSON file -----"
    let jsonObjStr = """
        {"test": "hello!!",
        "other": 1.0}
          """
    jsonObj <- liftEff $ Buffer.fromString jsonObjStr UTF8

    results <- Files.add ipfs [{path: "/tmp/jsontest.txt", content: jsonObj }]
    let hashes = map (IPFSPathString <<< _.hash) $ fromMaybe [] (init results)

    -- Block tests

    liftEff $ log "----- write block -----"
    putRes <- Block.put ipfs jsonObj defaultPutOpts
    liftEff $ log "put block"
    putRes.cid `shouldEqual` "QmVk5ASkHK8LLzcCYNFb1ChWEnTkPQ8fusiwNXGGQKFgJo"
    liftEff $ log "MH matches"

    liftEff $ log "----- get block -----"
    getRes <- Block.get ipfs putRes.cid
    liftEff $ log "got block"

    retData <- liftEff $ Buffer.toArray getRes.data
    inputData <- liftEff $ Buffer.toArray jsonObj
    retData `shouldEqual` inputData
    liftEff $ log "returned data matches"

    retAsStr <- liftEff $ Buffer.toString UTF8 getRes.data
    retAsStr `shouldEqual` jsonObjStr
    liftEff $ log "returned data as string matches"

    liftEff $ log "----- stat block -----"
    statRes <- Block.stat ipfs putRes.cid
    liftEff $ log "stat'd block"

    bufLen <- (liftEff $ Buffer.size jsonObj)
    bufLen `shouldEqual` statRes.size
    liftEff $ log "size matches"

    -- move producers to end because it seems to terminate the test early otherwise

    liftEff $ log "----- Producer with cat -----"
    producers <- traverse (Files.catProducer ipfs) hashes

    let trns :: ∀ m. Monad m
             => Transformer String (Either String {test :: String, other :: Number}) m Unit
        trns = transform \str -> do
                  json <- jsonParser str
                  test <- maybe (Left "No 'test' field") Right $
                            json ^? _Object <<< ix "test" <<< _String
                  other <- maybe (Left "No 'other' field") Right $
                            json ^? _Object <<< ix "other" <<< _Number
                  pure {test, other}


    let cnsm :: Consumer (Either String {test :: String, other :: Number}) (Aff _) Unit
        cnsm = consumer \obj -> do
          liftEff $ case obj of
            Left str -> log str
            Right {test, other} -> do
              log $ "test: " <> test
              log $ "other: " <> show other
          pure Nothing

    traverse_ (\p -> runProcess ((p $~ trns) $$ cnsm)) producers

    liftEff $ log "This message should print but it never does!"

  pure unit
