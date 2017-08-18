module IPFS.Files where
       -- (
       --   cat
       -- , catImpl2
       -- ) where




import Prelude

import Control.Coroutine (Producer)
import Control.Coroutine.Aff (produce', produceAff)
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Uncurried (EffFn1, EffFn2, runEffFn1, runEffFn2)
import Control.Promise (Promise)
import Control.Promise as Promise
import Data.Either (Either(..))
import IPFS (IPFS, IPFSEff)
import IPFS.Types (IPFSPath(..))
import Node.Buffer (Buffer)
import Node.Encoding (Encoding(..))
import Node.Stream (Duplex, Readable, onClose, onData, onDataString)
import Unsafe.Coerce (unsafeCoerce)


type IPFSObject = { path :: String
                  , content :: Buffer }

type AddResult = { path :: String
                 , hash :: String
                 , size :: Int
                 }


foreign import addImpl :: ∀ eff.
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


foreign import createAddStreamImpl :: ∀ r eff.
                                      EffFn1 (ipfs :: IPFSEff | eff)
                                      IPFS
                                      (Promise (Duplex (ipfs :: IPFSEff | eff)))

createAddStream ipfs = liftEff (runEffFn1 createAddStreamImpl ipfs)


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


type CatEffs eff = ( ipfs :: IPFSEff
                   , exception :: EXCEPTION
                   , avar :: AVAR | eff)
catProducer :: ∀ eff.
               IPFS
            -> IPFSPath
            -> Aff (CatEffs eff)
                   (Producer String (Aff (CatEffs eff)) Unit)
catProducer ipfs (IPFSPathString path) = do
  str <- liftEff (runEffFn2 catImpl ipfs path) >>= Promise.toAff
  pure $ produce' \emit -> do
    onDataString str UTF8 $ emit <<< Left
    onClose str $ emit (Right unit)


-- TODO: Node.Stream can't deal with Object streams
-- foreign import getImpl :: ∀ r eff.
--                           EffFn2 (ipfs :: IPFSEff | eff)
--                           IPFS
--                           String
--                           (Promise (Readable r (ipfs :: IPFSEff | eff)))
-- get ipfs (IPFSPathString path) = (liftEff (runEffFn2 getImpl ipfs path) >>= Promise.toAff)
