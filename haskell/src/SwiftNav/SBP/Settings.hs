-- |
-- Module:      SwiftNav.SBP.Settings
-- Copyright:   Copyright (C) 2015 Swift Navigation, Inc.
-- License:     LGPL-3
-- Maintainer:  Mark Fine <dev@swiftnav.com>
-- Stability:   experimental
-- Portability: portable
--
-- Messages for reading and writing the device's device settings.  These are in
-- the implementation-defined range (0x0000-0x00FF). Note that some of these
-- messages share the same message type ID for both the host request and the
-- device response. See the accompanying document for descriptions of settings
-- configurations and examples: https://github.com/swift-
-- nav/piksi\_firmware/blob/master/docs/settings.pdf

module SwiftNav.SBP.Settings where

import BasicPrelude
import Control.Monad
import Control.Monad.Loops
import Data.Aeson.TH (deriveJSON, defaultOptions, fieldLabelModifier)
import Data.Binary
import Data.Binary.Get
import Data.Binary.IEEE754
import Data.Binary.Put
import Data.ByteString
import Data.ByteString.Lazy hiding ( ByteString )
import Data.Int
import Data.Word
import SwiftNav.SBP.Encoding

msgSettingsSave :: Word16
msgSettingsSave = 0x00A1

-- | SBP class for message MSG_SETTINGS_SAVE (0x00A1).
--
-- The save settings message persists the device's current settings
-- configuration to its onboard flash memory file system.
data MsgSettingsSave = MsgSettingsSave
  deriving ( Show, Read, Eq )

instance Binary MsgSettingsSave where
  get =
    return MsgSettingsSave

  put MsgSettingsSave =
    return ()

$(deriveJSON defaultOptions {fieldLabelModifier = fromMaybe "msgSettingsSave_" . stripPrefix "msgSettingsSave_"}
             ''MsgSettingsSave)

msgSettingsWrite :: Word16
msgSettingsWrite = 0x00A0

-- | SBP class for message MSG_SETTINGS_WRITE (0x00A0).
--
-- The setting message writes the device's configuration.
data MsgSettingsWrite = MsgSettingsWrite
  { msgSettingsWrite_setting :: ByteString
    -- ^ A NULL-terminated and delimited string with contents [SECTION_SETTING,
    -- SETTING, VALUE].
  } deriving ( Show, Read, Eq )

instance Binary MsgSettingsWrite where
  get = do
    msgSettingsWrite_setting <- liftM toStrict getRemainingLazyByteString
    return MsgSettingsWrite {..}

  put MsgSettingsWrite {..} = do
    putByteString msgSettingsWrite_setting

$(deriveJSON defaultOptions {fieldLabelModifier = fromMaybe "msgSettingsWrite_" . stripPrefix "msgSettingsWrite_"}
             ''MsgSettingsWrite)

msgSettingsReadReq :: Word16
msgSettingsReadReq = 0x00A4

-- | SBP class for message MSG_SETTINGS_READ_REQ (0x00A4).
--
-- The setting message reads the device's configuration.
data MsgSettingsReadReq = MsgSettingsReadReq
  { msgSettingsReadReq_setting :: ByteString
    -- ^ A NULL-terminated and delimited string with contents [SECTION_SETTING,
    -- SETTING].
  } deriving ( Show, Read, Eq )

instance Binary MsgSettingsReadReq where
  get = do
    msgSettingsReadReq_setting <- liftM toStrict getRemainingLazyByteString
    return MsgSettingsReadReq {..}

  put MsgSettingsReadReq {..} = do
    putByteString msgSettingsReadReq_setting

$(deriveJSON defaultOptions {fieldLabelModifier = fromMaybe "msgSettingsReadReq_" . stripPrefix "msgSettingsReadReq_"}
             ''MsgSettingsReadReq)

msgSettingsReadResp :: Word16
msgSettingsReadResp = 0x00A5

-- | SBP class for message MSG_SETTINGS_READ_RESP (0x00A5).
--
-- The setting message reads the device's configuration.
data MsgSettingsReadResp = MsgSettingsReadResp
  { msgSettingsReadResp_setting :: ByteString
    -- ^ A NULL-terminated and delimited string with contents [SECTION_SETTING,
    -- SETTING, VALUE].
  } deriving ( Show, Read, Eq )

instance Binary MsgSettingsReadResp where
  get = do
    msgSettingsReadResp_setting <- liftM toStrict getRemainingLazyByteString
    return MsgSettingsReadResp {..}

  put MsgSettingsReadResp {..} = do
    putByteString msgSettingsReadResp_setting

$(deriveJSON defaultOptions {fieldLabelModifier = fromMaybe "msgSettingsReadResp_" . stripPrefix "msgSettingsReadResp_"}
             ''MsgSettingsReadResp)

msgSettingsReadByIndexReq :: Word16
msgSettingsReadByIndexReq = 0x00A2

-- | SBP class for message MSG_SETTINGS_READ_BY_INDEX_REQ (0x00A2).
--
-- The settings message for iterating through the settings values. It will read
-- the setting at an index, returning a NULL-terminated and delimited string
-- with contents [SECTION_SETTING, SETTING, VALUE].
data MsgSettingsReadByIndexReq = MsgSettingsReadByIndexReq
  { msgSettingsReadByIndexReq_index :: Word16
    -- ^ An index into the device settings, with values ranging from 0 to
    -- length(settings)
  } deriving ( Show, Read, Eq )

instance Binary MsgSettingsReadByIndexReq where
  get = do
    msgSettingsReadByIndexReq_index <- getWord16le
    return MsgSettingsReadByIndexReq {..}

  put MsgSettingsReadByIndexReq {..} = do
    putWord16le msgSettingsReadByIndexReq_index

$(deriveJSON defaultOptions {fieldLabelModifier = fromMaybe "msgSettingsReadByIndexReq_" . stripPrefix "msgSettingsReadByIndexReq_"}
             ''MsgSettingsReadByIndexReq)

msgSettingsReadByIndexResp :: Word16
msgSettingsReadByIndexResp = 0x00A7

-- | SBP class for message MSG_SETTINGS_READ_BY_INDEX_RESP (0x00A7).
--
-- The settings message for iterating through the settings values. It will read
-- the setting at an index, returning a NULL-terminated and delimited string
-- with contents [SECTION_SETTING, SETTING, VALUE].
data MsgSettingsReadByIndexResp = MsgSettingsReadByIndexResp
  { msgSettingsReadByIndexResp_index  :: Word16
    -- ^ An index into the device settings, with values ranging from 0 to
    -- length(settings)
  , msgSettingsReadByIndexResp_setting :: ByteString
    -- ^ A NULL-terminated and delimited string with contents [SECTION_SETTING,
    -- SETTING, VALUE].
  } deriving ( Show, Read, Eq )

instance Binary MsgSettingsReadByIndexResp where
  get = do
    msgSettingsReadByIndexResp_index <- getWord16le
    msgSettingsReadByIndexResp_setting <- liftM toStrict getRemainingLazyByteString
    return MsgSettingsReadByIndexResp {..}

  put MsgSettingsReadByIndexResp {..} = do
    putWord16le msgSettingsReadByIndexResp_index
    putByteString msgSettingsReadByIndexResp_setting

$(deriveJSON defaultOptions {fieldLabelModifier = fromMaybe "msgSettingsReadByIndexResp_" . stripPrefix "msgSettingsReadByIndexResp_"}
             ''MsgSettingsReadByIndexResp)

msgSettingsReadByIndexDone :: Word16
msgSettingsReadByIndexDone = 0x00A6

-- | SBP class for message MSG_SETTINGS_READ_BY_INDEX_DONE (0x00A6).
--
-- The settings message for indicating end of the settings values.
data MsgSettingsReadByIndexDone = MsgSettingsReadByIndexDone
  deriving ( Show, Read, Eq )

instance Binary MsgSettingsReadByIndexDone where
  get =
    return MsgSettingsReadByIndexDone

  put MsgSettingsReadByIndexDone =
    return ()

$(deriveJSON defaultOptions {fieldLabelModifier = fromMaybe "msgSettingsReadByIndexDone_" . stripPrefix "msgSettingsReadByIndexDone_"}
             ''MsgSettingsReadByIndexDone)
