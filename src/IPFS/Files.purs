module IPFS.Files
       (
         cat
       ) where




import Prelude
import Control.Promise as Promise
import Control.Monad.Aff (Aff)
import Control.Monad.Eff.Class (liftEff)
import Control.Monad.Eff.Uncurried (EffFn2, runEffFn2)
import Control.Promise (Promise)
import IPFS (IPFS, IPFSEff)
import IPFS.Types (IPFSPath)
import Node.Stream (Readable)




add = false
createAddStream = false

foreign import catImpl :: ∀ r eff. EffFn2 (ipfs :: IPFSEff | eff) IPFS IPFSPath (Promise (Readable r (ipfs :: IPFSEff | eff)))


-- cat :: ∀ r eff. IPFS -> IPFSPath -> Readable r (ipfs :: IPFSEff | eff)

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
cat ipfs path = liftEff (runEffFn2 catImpl ipfs path) >>= Promise.toAff
