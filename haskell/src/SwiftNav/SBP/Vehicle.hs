{-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- |
-- Module:      SwiftNav.SBP.Vehicle
-- Copyright:   Copyright (C) 2015 Swift Navigation, Inc.
-- License:     LGPL-3
-- Maintainer:  Mark Fine <dev@swiftnav.com>
-- Stability:   experimental
-- Portability: portable
--
-- Messages from a vehicle.

module SwiftNav.SBP.Vehicle where

import BasicPrelude as P
import Control.Lens
import Control.Monad.Loops
import Data.Aeson.TH           (defaultOptions, deriveJSON, fieldLabelModifier)
import Data.Binary
import Data.Binary.Get
import Data.Binary.IEEE754
import Data.Binary.Put
import Data.ByteString
import Data.ByteString.Lazy    hiding (ByteString)
import Data.Int
import Data.Word
import SwiftNav.SBP.Encoding
import SwiftNav.SBP.TH

msgFwdVel :: Word16
msgFwdVel = 0x0902

-- | SBP class for message MSG_FWD_VEL (0x0902).
--
-- Message representing the forward component of vehicle velocity, which
-- corresponds with the body axis x direction. There are 4 possible user-
-- defined sources of this message  which are labeled arbitrarily source 0
-- through 3.
data MsgFwdVel = MsgFwdVel
  { _msgFwdVel_tow    :: Word32
    -- ^ Time field representing either milliseconds in the GPS Week or local CPU
    -- time from the producing system in milliseconds.  See the tow_source flag
    -- for the exact source of this timestamp.
  , _msgFwdVel_velocity :: Int32
    -- ^ The signed forward component of vehicle velocity.
  , _msgFwdVel_flags  :: Word8
    -- ^ Status flags
  } deriving ( Show, Read, Eq )

instance Binary MsgFwdVel where
  get = do
    _msgFwdVel_tow <- getWord32le
    _msgFwdVel_velocity <- fromIntegral <$> getWord32le
    _msgFwdVel_flags <- getWord8
    return MsgFwdVel {..}

  put MsgFwdVel {..} = do
    putWord32le _msgFwdVel_tow
    putWord32le $ fromIntegral _msgFwdVel_velocity
    putWord8 _msgFwdVel_flags

$(deriveSBP 'msgFwdVel ''MsgFwdVel)

$(deriveJSON defaultOptions {fieldLabelModifier = fromMaybe "_msgFwdVel_" . P.stripPrefix "_msgFwdVel_"}
             ''MsgFwdVel)
$(makeLenses ''MsgFwdVel)
