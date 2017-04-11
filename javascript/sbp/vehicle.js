/**
 * Copyright (C) 2015 Swift Navigation Inc.
 * Contact: Joshua Gross <josh@swift-nav.com>
 * This source is subject to the license found in the file 'LICENSE' which must
 * be distributed together with this source. All other rights reserved.
 *
 * THIS CODE AND INFORMATION IS PROVIDED "AS IS" WITHOUT WARRANTY OF ANY KIND,
 * EITHER EXPRESSED OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE IMPLIED
 * WARRANTIES OF MERCHANTABILITY AND/OR FITNESS FOR A PARTICULAR PURPOSE.
 */

/**********************
 * Automatically generated from piksi/yaml/swiftnav/sbp/vehicle.yaml with generate.py.
 * Don't edit this by hand!
 **********************
 * Package description:
 *
 * Messages from a vehicle.
***********************/

var SBP = require('./sbp');
var Parser = require('./parser');
var Int64 = require('node-int64');
var UInt64 = require('cuint').UINT64;

/**
 * SBP class for message MSG_FWD_VEL (0x0902).
 *
 * Message representing the forward component of vehicle velocity, which
 * corresponds with the body axis x direction. There are 4 possible user-defined
 * sources of this message  which are labeled arbitrarily source 0 through 3.
 *
 * Fields in the SBP payload (`sbp.payload`):
 * @field tow number (unsigned 32-bit int, 4 bytes) Time field representing either milliseconds in the GPS Week or local CPU time
 *   from the producing system in milliseconds.  See the tow_source flag for the
 *   exact source of this timestamp.
 * @field velocity number (signed 32-bit int, 4 bytes) The signed forward component of vehicle velocity.
 * @field flags number (unsigned 8-bit int, 1 byte) Status flags
 *
 * @param sbp An SBP object with a payload to be decoded.
 */
var MsgFwdVel = function (sbp, fields) {
  SBP.call(this, sbp);
  this.messageType = "MSG_FWD_VEL";
  this.fields = (fields || this.parser.parse(sbp.payload));

  return this;
};
MsgFwdVel.prototype = Object.create(SBP.prototype);
MsgFwdVel.prototype.messageType = "MSG_FWD_VEL";
MsgFwdVel.prototype.msg_type = 0x0902;
MsgFwdVel.prototype.constructor = MsgFwdVel;
MsgFwdVel.prototype.parser = new Parser()
  .endianess('little')
  .uint32('tow')
  .int32('velocity')
  .uint8('flags');
MsgFwdVel.prototype.fieldSpec = [];
MsgFwdVel.prototype.fieldSpec.push(['tow', 'writeUInt32LE', 4]);
MsgFwdVel.prototype.fieldSpec.push(['velocity', 'writeInt32LE', 4]);
MsgFwdVel.prototype.fieldSpec.push(['flags', 'writeUInt8', 1]);

module.exports = {
  0x0902: MsgFwdVel,
  MsgFwdVel: MsgFwdVel,
}