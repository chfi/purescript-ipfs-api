module IPFS.Files where
       -- (
       --   cat
       -- , catImpl2
       -- ) where




import Prelude

import Control.Monad.Aff (Aff, makeAff)
import Control.Monad.Eff (Eff)
import Control.Monad.Eff.Class (class MonadEff, liftEff)
import Control.Monad.Eff.Exception (Error)
import Control.Monad.Eff.Uncurried (EffFn2, runEffFn2)
import Control.Promise (Promise)
import Control.Promise as Promise
import IPFS (IPFS, IPFSEff)
import IPFS.Types (IPFSPath(..))
import Node.Stream (Read, Readable, Stream)




add = false
createAddStream = false

-- makeAff :: forall e a. ((Error -> Eff e Unit) -> (a -> Eff e Unit) -> Eff e Unit) -> Aff e a

foreign import catImpl :: ∀ r eff. EffFn2 (ipfs :: IPFSEff | eff) IPFS String (Promise (Readable r (ipfs :: IPFSEff | eff)))

cat :: forall eff r.
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


cat' :: forall t15 t21 t22.
  MonadEff
    ( ipfs :: IPFSEff
    | t21
    )
    t15
   => IPFS
      -> IPFSPath
         -> t15
              (Promise
                 (Stream
                    ( read :: Read
                    | t22
                    )
                    ( ipfs :: IPFSEff
                    | t21
                    )
                 )
              )
cat' ipfs (IPFSPathString path) = liftEff (runEffFn2 catImpl ipfs path)
