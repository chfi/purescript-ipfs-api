module IPFS.Types where

import Control.Monad.Eff (kind Effect)

foreign import data IPFS :: Type
foreign import data IPFSEff :: Effect


-- TODO multihash support
data IPFSPath = IPFSPathString String
