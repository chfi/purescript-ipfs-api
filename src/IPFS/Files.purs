module IPFS.Files
       ( add
       , cat
       , catProducer
       , IPFSObject
       , AddResult
       ) where


import Prelude

import Control.Coroutine (Producer)
import Control.Coroutine.Aff (produce')
import Control.Monad.Aff (Aff)
import Control.Monad.Aff.AVar (AVAR)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Exception (EXCEPTION)
import Control.Monad.Eff.Uncurried (EffFn2, runEffFn2)
import Control.Promise (Promise)
import Control.Promise as Promise
import Data.Either (Either(..))
import IPFS (IPFS, IPFSEff)
import IPFS.Types (IPFSPath(..))
import Node.Buffer (Buffer)
import Node.Encoding (Encoding(..))
import Node.Stream (Readable, onClose, onDataString)


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


-- | Add an array of objects to an IPFS instance
add :: ∀ eff.
       IPFS
    -> Array IPFSObject
    -> Aff (ipfs :: IPFSEff | eff) (Array AddResult)
add ipfs objs = liftEff (runEffFn2 addImpl ipfs objs) >>= Promise.toAff



foreign import catImpl :: ∀ r eff.
                          EffFn2 (ipfs :: IPFSEff | eff)
                          IPFS
                          String
                          (Promise (Readable r (ipfs :: IPFSEff | eff)))


-- | Read an object from an IPFS instance, returning a Node Stream
cat :: ∀ r eff.
       IPFS
    -> IPFSPath
    -> Aff (ipfs :: IPFSEff | eff) (Readable r (ipfs :: IPFSEff | eff))
cat ipfs (IPFSPathString path) = liftEff (runEffFn2 catImpl ipfs path) >>= Promise.toAff


type CatEffs eff = ( ipfs :: IPFSEff
                   , exception :: EXCEPTION
                   , avar :: AVAR | eff)


-- | Create a coroutine producer that produces the contents of a file as a String
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
