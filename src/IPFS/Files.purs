module IPFS.Files where
       -- (
       --   cat
       -- , catImpl2
       -- ) where




import Prelude

import Control.Coroutine (Consumer, Producer, await, consumer)
import Control.Coroutine.Aff (produce', produceAff)
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Console (log)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Uncurried (EffFn1, EffFn2, runEffFn1, runEffFn2)
import Control.Monad.Rec.Class (Step(..), forever, tailRecM)
import Control.Monad.Trans.Class (lift)
import Control.Promise (Promise)
import Control.Promise as Promise
import Data.Either (Either(..))
import Data.Maybe (Maybe(..))
import Data.Traversable (traverse_)
import IPFS (IPFS, IPFSEff)
import IPFS.Types (IPFSPath(..))
import Node.Buffer (Buffer)
import Node.Encoding (Encoding(..))
import Node.Stream (Duplex, Readable, Writable, onClose, onData, onDataString, write)
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


-- TODO: createAddStream uses object mode streams AFAICT,
-- which are not supported by purescript-node-streams
-- foreign import createAddStreamImpl :: ∀ eff.
--                                       EffFn1 (ipfs :: IPFSEff | eff)
--                                       IPFS
--                                       (Promise (Duplex (ipfs :: IPFSEff | eff)))

createAddStream :: ∀ eff.
                   IPFS
                -> (Consumer (Maybe IPFSObject) (Aff _) (Array AddResult))
createAddStream ipfs = tailRecM go []
  where go r = do
          obj <- await
          case obj of
            Nothing -> pure $ Done r
            Just obj' -> do
              result <- lift $ add ipfs [obj']
              pure $ Loop $ r <> result



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


-- TODO: get also uses object mode streams
-- foreign import getImpl :: ∀ r eff.
--                           EffFn2 (ipfs :: IPFSEff | eff)
--                           IPFS
--                           String
--                           (Promise (Readable r (ipfs :: IPFSEff | eff)))
-- get ipfs (IPFSPathString path) = (liftEff (runEffFn2 getImpl ipfs path) >>= Promise.toAff)
