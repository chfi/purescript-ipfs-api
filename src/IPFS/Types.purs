module IPFS.Types where

import Control.Monad.Eff (kind Effect)
import Prelude (class Show)

foreign import data IPFS :: Type
foreign import data IPFSEff :: Effect


-- TODO multihash support
data IPFSPath = IPFSPathString String

instance showIPFSPath :: Show IPFSPath where
  show (IPFSPathString s) = s
