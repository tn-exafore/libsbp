{-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- |
-- Module:      SwiftNav.SBP.Orientation
-- Copyright:   Copyright (C) 2015 Swift Navigation, Inc.
-- License:     LGPL-3
-- Maintainer:  Mark Fine <dev@swiftnav.com>
-- Stability:   experimental
-- Portability: portable
--
-- Orientation Messages

module SwiftNav.SBP.Orientation where

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

msgBaselineHeading :: Word16
msgBaselineHeading = 0x020F

-- | SBP class for message MSG_BASELINE_HEADING (0x020F).
--
-- This message reports the baseline heading pointing from the base station to
-- the rover relative to True North. The full GPS time is given by the
-- preceding MSG_GPS_TIME with the matching time-of-week (tow). It is intended
-- that time-matched RTK mode is used when the base station is moving.
data MsgBaselineHeading = MsgBaselineHeading
  { _msgBaselineHeading_tow   :: Word32
    -- ^ GPS Time of Week
  , _msgBaselineHeading_heading :: Word32
    -- ^ Heading
  , _msgBaselineHeading_n_sats :: Word8
    -- ^ Number of satellites used in solution
  , _msgBaselineHeading_flags :: Word8
    -- ^ Status flags
  } deriving ( Show, Read, Eq )

instance Binary MsgBaselineHeading where
  get = do
    _msgBaselineHeading_tow <- getWord32le
    _msgBaselineHeading_heading <- getWord32le
    _msgBaselineHeading_n_sats <- getWord8
    _msgBaselineHeading_flags <- getWord8
    return MsgBaselineHeading {..}

  put MsgBaselineHeading {..} = do
    putWord32le _msgBaselineHeading_tow
    putWord32le _msgBaselineHeading_heading
    putWord8 _msgBaselineHeading_n_sats
    putWord8 _msgBaselineHeading_flags

$(deriveSBP 'msgBaselineHeading ''MsgBaselineHeading)

$(deriveJSON defaultOptions {fieldLabelModifier = fromMaybe "_msgBaselineHeading_" . P.stripPrefix "_msgBaselineHeading_"}
             ''MsgBaselineHeading)
$(makeLenses ''MsgBaselineHeading)

msgOrientQuat :: Word16
msgOrientQuat = 0x0220

-- | SBP class for message MSG_ORIENT_QUAT (0x0220).
--
-- This message reports the quaternion vector describing the vehicle
-- orientation with respect to a local-level NED frame.
data MsgOrientQuat = MsgOrientQuat
  { _msgOrientQuat_tow :: Word32
    -- ^ GPS Time of Week
  , _msgOrientQuat_w   :: Int32
    -- ^ Real component
  , _msgOrientQuat_x   :: Int32
    -- ^ 1st imaginary component
  , _msgOrientQuat_y   :: Int32
    -- ^ 2nd imaginary component
  , _msgOrientQuat_z   :: Int32
    -- ^ 3rd imaginary component
  , _msgOrientQuat_var_w :: Float
    -- ^ Variance of w
  , _msgOrientQuat_var_x :: Float
    -- ^ Variance of x
  , _msgOrientQuat_var_y :: Float
    -- ^ Variance of y
  , _msgOrientQuat_var_z :: Float
    -- ^ Variance of z
  , _msgOrientQuat_flags :: Word8
    -- ^ Status flags
  } deriving ( Show, Read, Eq )

instance Binary MsgOrientQuat where
  get = do
    _msgOrientQuat_tow <- getWord32le
    _msgOrientQuat_w <- fromIntegral <$> getWord32le
    _msgOrientQuat_x <- fromIntegral <$> getWord32le
    _msgOrientQuat_y <- fromIntegral <$> getWord32le
    _msgOrientQuat_z <- fromIntegral <$> getWord32le
    _msgOrientQuat_var_w <- getFloat32le
    _msgOrientQuat_var_x <- getFloat32le
    _msgOrientQuat_var_y <- getFloat32le
    _msgOrientQuat_var_z <- getFloat32le
    _msgOrientQuat_flags <- getWord8
    return MsgOrientQuat {..}

  put MsgOrientQuat {..} = do
    putWord32le _msgOrientQuat_tow
    putWord32le $ fromIntegral _msgOrientQuat_w
    putWord32le $ fromIntegral _msgOrientQuat_x
    putWord32le $ fromIntegral _msgOrientQuat_y
    putWord32le $ fromIntegral _msgOrientQuat_z
    putFloat32le _msgOrientQuat_var_w
    putFloat32le _msgOrientQuat_var_x
    putFloat32le _msgOrientQuat_var_y
    putFloat32le _msgOrientQuat_var_z
    putWord8 _msgOrientQuat_flags

$(deriveSBP 'msgOrientQuat ''MsgOrientQuat)

$(deriveJSON defaultOptions {fieldLabelModifier = fromMaybe "_msgOrientQuat_" . P.stripPrefix "_msgOrientQuat_"}
             ''MsgOrientQuat)
$(makeLenses ''MsgOrientQuat)

msgOrientEuler :: Word16
msgOrientEuler = 0x0221

-- | SBP class for message MSG_ORIENT_EULER (0x0221).
--
-- This message reports the yaw, pitch, and roll angles of the vehicle body
-- frame. The rotations should applied intrinsically in the order yaw, pitch,
-- and roll  in order to rotate the from a frame aligned with hte local-level
-- NED frame  to the vehicle body frame.
data MsgOrientEuler = MsgOrientEuler
  { _msgOrientEuler_tow     :: Word32
    -- ^ GPS Time of Week
  , _msgOrientEuler_roll    :: Int16
    -- ^ rotation about the forward axis of the vehicle
  , _msgOrientEuler_pitch   :: Int16
    -- ^ rotation about the rightward axis of the vehicle
  , _msgOrientEuler_yaw     :: Int16
    -- ^ rotation about the downward axis of the vehicle
  , _msgOrientEuler_var_roll :: Float
    -- ^ Variance of roll
  , _msgOrientEuler_var_pitch :: Float
    -- ^ Variance of pitch
  , _msgOrientEuler_var_yaw :: Float
    -- ^ Variance of yaw
  , _msgOrientEuler_flags   :: Word8
    -- ^ Status flags
  } deriving ( Show, Read, Eq )

instance Binary MsgOrientEuler where
  get = do
    _msgOrientEuler_tow <- getWord32le
    _msgOrientEuler_roll <- fromIntegral <$> getWord16le
    _msgOrientEuler_pitch <- fromIntegral <$> getWord16le
    _msgOrientEuler_yaw <- fromIntegral <$> getWord16le
    _msgOrientEuler_var_roll <- getFloat32le
    _msgOrientEuler_var_pitch <- getFloat32le
    _msgOrientEuler_var_yaw <- getFloat32le
    _msgOrientEuler_flags <- getWord8
    return MsgOrientEuler {..}

  put MsgOrientEuler {..} = do
    putWord32le _msgOrientEuler_tow
    putWord16le $ fromIntegral _msgOrientEuler_roll
    putWord16le $ fromIntegral _msgOrientEuler_pitch
    putWord16le $ fromIntegral _msgOrientEuler_yaw
    putFloat32le _msgOrientEuler_var_roll
    putFloat32le _msgOrientEuler_var_pitch
    putFloat32le _msgOrientEuler_var_yaw
    putWord8 _msgOrientEuler_flags

$(deriveSBP 'msgOrientEuler ''MsgOrientEuler)

$(deriveJSON defaultOptions {fieldLabelModifier = fromMaybe "_msgOrientEuler_" . P.stripPrefix "_msgOrientEuler_"}
             ''MsgOrientEuler)
$(makeLenses ''MsgOrientEuler)

msgAngularRate :: Word16
msgAngularRate = 0x0222

-- | SBP class for message MSG_ANGULAR_RATE (0x0222).
--
-- This message reports the orientation rates in the vehicle body frame.  The
-- values represent the measurements a strapped down gyroscope would  make and
-- are not equivalent to the time derivative of the Euler angles. By
-- convention, the vehicle x-axis is expected to be aligned with the forward
-- direction, while the vehicle y-axis is expected to be aligned with the right
-- direction, and the vehicle z-axis should be aligned with the down direction.
data MsgAngularRate = MsgAngularRate
  { _msgAngularRate_tow :: Word32
    -- ^ GPS Time of Week
  , _msgAngularRate_x   :: Int16
    -- ^ angular rate about x axis
  , _msgAngularRate_y   :: Int16
    -- ^ angular rate about y axis
  , _msgAngularRate_z   :: Int16
    -- ^ angular rate about z axis
  , _msgAngularRate_flags :: Word8
    -- ^ Status flags
  } deriving ( Show, Read, Eq )

instance Binary MsgAngularRate where
  get = do
    _msgAngularRate_tow <- getWord32le
    _msgAngularRate_x <- fromIntegral <$> getWord16le
    _msgAngularRate_y <- fromIntegral <$> getWord16le
    _msgAngularRate_z <- fromIntegral <$> getWord16le
    _msgAngularRate_flags <- getWord8
    return MsgAngularRate {..}

  put MsgAngularRate {..} = do
    putWord32le _msgAngularRate_tow
    putWord16le $ fromIntegral _msgAngularRate_x
    putWord16le $ fromIntegral _msgAngularRate_y
    putWord16le $ fromIntegral _msgAngularRate_z
    putWord8 _msgAngularRate_flags

$(deriveSBP 'msgAngularRate ''MsgAngularRate)

$(deriveJSON defaultOptions {fieldLabelModifier = fromMaybe "_msgAngularRate_" . P.stripPrefix "_msgAngularRate_"}
             ''MsgAngularRate)
$(makeLenses ''MsgAngularRate)
