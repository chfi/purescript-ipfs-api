module IPFS
       ( connect
       , identity
       , IPFSPeer
       , version
       , IPFSVersion
       , module Exports
       ) where


import Prelude
import Control.Promise as Promise
import Control.Monad.Aff (Aff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Uncurried (EffFn1, EffFn2, runEffFn1, runEffFn2)
import Control.Promise (Promise)
import IPFS.Types (IPFS, IPFSEff)
import IPFS.Types (IPFS, IPFSEff) as Exports

foreign import connectImpl :: ∀ eff. EffFn2 (ipfs :: IPFSEff | eff) String Int IPFS

connect :: forall eff. String -> Int -> Eff ( ipfs :: IPFSEff | eff ) IPFS
connect = runEffFn2 connectImpl


type IPFSPeer = { id :: String
                , publicKey :: String
                }

foreign import identityImpl :: ∀ eff. EffFn1 (ipfs :: IPFSEff | eff) IPFS (Promise IPFSPeer)

identity ipfs = liftEff (runEffFn1 identityImpl ipfs) >>= Promise.toAff


type IPFSVersion = { version :: String
                   , commit :: String
                   , repo :: String
                   }

foreign import versionImpl :: ∀ eff. EffFn1 (ipfs :: IPFSEff | eff) IPFS (Promise IPFSVersion)

version :: forall eff.
  IPFS
  -> Aff
       ( ipfs :: IPFSEff
       | eff
       )
       IPFSVersion
version ipfs = liftEff (runEffFn1 versionImpl ipfs) >>= Promise.toAff
