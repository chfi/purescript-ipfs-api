module IPFS.Files where
       -- (
       --   cat
       -- , catImpl2
       -- ) where




import Prelude

import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Uncurried (EffFn2, runEffFn2)
import Control.Promise (Promise)
import Control.Promise as Promise
import IPFS (IPFS, IPFSEff)
import IPFS.Types (IPFSPath(..))
import Node.Buffer (Buffer)
import Node.Stream (Readable)


type IPFSObject = { path :: String
                  , content :: Buffer }

type AddResult = { path :: String
                 , hash :: String
                 , size :: Int
                 }


foreign import addImpl :: ∀ r eff.
                          EffFn2 (ipfs :: IPFSEff | eff)
                          IPFS
                          (Array IPFSObject)
                          (Promise (Array AddResult))

add :: ∀ eff.
  IPFS
  -> Array IPFSObject
  -> Aff ( ipfs :: IPFSEff
         | eff
         ) (Array AddResult)
add ipfs objs = liftEff (runEffFn2 addImpl ipfs objs) >>= Promise.toAff

createAddStream = false


foreign import catImpl :: ∀ r eff.
                          EffFn2 (ipfs :: IPFSEff | eff)
                          IPFS
                          String
                          (Promise (Readable r (ipfs :: IPFSEff | eff)))

cat :: ∀ r eff.
       IPFS
    -> IPFSPath
    -> Aff
          ( ipfs :: IPFSEff
          | eff
          )
          (Readable r
             ( ipfs :: IPFSEff
             | eff
             )
          )
cat ipfs (IPFSPathString path) = liftEff (runEffFn2 catImpl ipfs path) >>= Promise.toAff
