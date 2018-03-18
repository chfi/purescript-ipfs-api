module IPFS.Block
       ( get
       , put
       , stat
       , defaultPutOpts
       , Cid
       , StatResult
       , BlockObj
       , GetResult
       , PutResult
       , PutOptions
       , PutInput
       ) where


import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Uncurried (EffFn2, EffFn3, runEffFn2, runEffFn3)
import Control.Promise (Promise)
import Control.Promise as Promise
import IPFS (IPFS, IPFSEff)
import Node.Buffer (Buffer)


type Cid = String

type StatResult = { cid :: String, size :: Int }

type BlockObj = { cid :: String, data :: Buffer }

type GetResult = BlockObj

type PutResult = BlockObj

type PutOptions = { format :: String, mhtype :: String, version :: Int }

type PutInput = Buffer

defaultPutOpts :: PutOptions
defaultPutOpts = { format: "dag-pb", mhtype: "sha2-256", version: 0 }


foreign import putImpl :: forall eff.
                          EffFn3 (ipfs :: IPFSEff | eff)
                          IPFS
                          PutInput
                          PutOptions
                          (Promise PutResult)


foreign import getImpl :: forall eff.
                          EffFn2 (ipfs :: IPFSEff | eff)
                          IPFS
                          Cid
                          (Promise GetResult)


foreign import statImpl :: forall eff.
                           EffFn2 (ipfs :: IPFSEff | eff)
                           IPFS
                           Cid
                           (Promise StatResult)


-- | Get a raw block from IPFS
get :: forall eff.
       IPFS
    -> Cid
    -> Aff (ipfs :: IPFSEff | eff) GetResult
get ipfs cid = liftEff (runEffFn2 getImpl ipfs cid) >>= Promise.toAff


-- | Put a raw block in IPFS
put :: forall eff.
       IPFS
    -> PutInput
    -> PutOptions
    -> Aff (ipfs :: IPFSEff | eff) PutResult
put ipfs buf opts = liftEff (runEffFn3 putImpl ipfs buf opts) >>= Promise.toAff


-- | Get stats for raw block in IPFS
stat :: forall eff.
        IPFS
     -> Cid
     -> Aff (ipfs :: IPFSEff | eff) StatResult
stat ipfs cid = liftEff (runEffFn2 statImpl ipfs cid) >>= Promise.toAff
