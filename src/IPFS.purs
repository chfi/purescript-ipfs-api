module IPFS
       ( connect
       , identity
       , IPFSPeer
       , version
       , IPFSVersion
       , module Exports
       ) where


import Prelude
import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Uncurried (EffFn1, EffFn2, runEffFn1, runEffFn2)
import Control.Promise as Promise
import Control.Promise (Promise)
import IPFS.Types (IPFS, IPFSEff)
import IPFS.Types (IPFS, IPFSEff) as Exports

foreign import connectImpl :: ∀ eff. EffFn2 (ipfs :: IPFSEff | eff) String Int IPFS

-- | Connect to an IPFS instance
connect :: forall eff.
           String
        -> Int
        -> Eff ( ipfs :: IPFSEff | eff ) IPFS
connect url port = runEffFn2 connectImpl url port


type IPFSPeer = { id :: String
                , publicKey :: String
                }

foreign import identityImpl :: ∀ eff. EffFn1 (ipfs :: IPFSEff | eff) IPFS (Promise IPFSPeer)

-- | Wrapper over `ipfs.id`
identity :: forall eff.
            IPFS
         -> Aff (ipfs :: IPFSEff | eff) IPFSPeer
identity ipfs = liftEff (runEffFn1 identityImpl ipfs) >>= Promise.toAff


type IPFSVersion = { version :: String
                   , commit :: String
                   , repo :: String
                   }

foreign import versionImpl :: ∀ eff. EffFn1 (ipfs :: IPFSEff | eff) IPFS (Promise IPFSVersion)

-- | Wrapper over `ipfs.version`
version :: forall eff.
           IPFS
        -> Aff (ipfs :: IPFSEff | eff) IPFSVersion
version ipfs = liftEff (runEffFn1 versionImpl ipfs) >>= Promise.toAff
