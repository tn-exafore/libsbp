#!/usr/bin/env python
# Copyright (C) 2015 Swift Navigation Inc.
# Contact: Fergus Noble <fergus@swiftnav.com>
#
# This source is subject to the license found in the file 'LICENSE' which must
# be be distributed together with this source. All other rights reserved.
#
# THIS CODE AND INFORMATION IS PROVIDED "AS IS" WITHOUT WARRANTY OF ANY KIND,
# EITHER EXPRESSED OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE IMPLIED
# WARRANTIES OF MERCHANTABILITY AND/OR FITNESS FOR A PARTICULAR PURPOSE.


"""
Orientation Messages
"""

from construct import *
import json
from sbp.msg import SBP, SENDER_ID
from sbp.utils import fmt_repr, exclude_fields, walk_json_dict, containerize, greedy_string

# Automatically generated from piksi/yaml/swiftnav/sbp/orientation.yaml with generate.py.
# Please do not hand edit!


SBP_MSG_BASELINE_HEADING = 0x020F
class MsgBaselineHeading(SBP):
  """SBP class for message MSG_BASELINE_HEADING (0x020F).

  You can have MSG_BASELINE_HEADING inherit its fields directly
  from an inherited SBP object, or construct it inline using a dict
  of its fields.

  
  This message reports the baseline heading pointing from the base station
to the rover relative to True North. The full GPS time is given by the
preceding MSG_GPS_TIME with the matching time-of-week (tow). It is intended
that time-matched RTK mode is used when the base station is moving.


  Parameters
  ----------
  sbp : SBP
    SBP parent object to inherit from.
  tow : int
    GPS Time of Week
  heading : int
    Heading
  n_sats : int
    Number of satellites used in solution
  flags : int
    Status flags
  sender : int
    Optional sender ID, defaults to SENDER_ID (see sbp/msg.py).

  """
  _parser = Struct("MsgBaselineHeading",
                   ULInt32('tow'),
                   ULInt32('heading'),
                   ULInt8('n_sats'),
                   ULInt8('flags'),)
  __slots__ = [
               'tow',
               'heading',
               'n_sats',
               'flags',
              ]

  def __init__(self, sbp=None, **kwargs):
    if sbp:
      super( MsgBaselineHeading,
             self).__init__(sbp.msg_type, sbp.sender, sbp.length,
                            sbp.payload, sbp.crc)
      self.from_binary(sbp.payload)
    else:
      super( MsgBaselineHeading, self).__init__()
      self.msg_type = SBP_MSG_BASELINE_HEADING
      self.sender = kwargs.pop('sender', SENDER_ID)
      self.tow = kwargs.pop('tow')
      self.heading = kwargs.pop('heading')
      self.n_sats = kwargs.pop('n_sats')
      self.flags = kwargs.pop('flags')

  def __repr__(self):
    return fmt_repr(self)

  @staticmethod
  def from_json(s):
    """Given a JSON-encoded string s, build a message object.

    """
    d = json.loads(s)
    return MsgBaselineHeading.from_json_dict(d)

  @staticmethod
  def from_json_dict(d):
    sbp = SBP.from_json_dict(d)
    return MsgBaselineHeading(sbp, **d)

 
  def from_binary(self, d):
    """Given a binary payload d, update the appropriate payload fields of
    the message.

    """
    p = MsgBaselineHeading._parser.parse(d)
    for n in self.__class__.__slots__:
      setattr(self, n, getattr(p, n))

  def to_binary(self):
    """Produce a framed/packed SBP message.

    """
    c = containerize(exclude_fields(self))
    self.payload = MsgBaselineHeading._parser.build(c)
    return self.pack()

  def to_json_dict(self):
    self.to_binary()
    d = super( MsgBaselineHeading, self).to_json_dict()
    j = walk_json_dict(exclude_fields(self))
    d.update(j)
    return d
    
SBP_MSG_ORIENT_QUAT = 0x0220
class MsgOrientQuat(SBP):
  """SBP class for message MSG_ORIENT_QUAT (0x0220).

  You can have MSG_ORIENT_QUAT inherit its fields directly
  from an inherited SBP object, or construct it inline using a dict
  of its fields.

  
  This message reports the quaternion vector describing the vehicle orientation
with respect to a local-level NED frame.  


  Parameters
  ----------
  sbp : SBP
    SBP parent object to inherit from.
  tow : int
    GPS Time of Week
  w : int
    Real component
  x : int
    1st imaginary component
  y : int
    2nd imaginary component
  z : int
    3rd imaginary component
  var_w : float
    Variance of w
  var_x : float
    Variance of x
  var_y : float
    Variance of y
  var_z : float
    Variance of z
  flags : int
    Status flags
  sender : int
    Optional sender ID, defaults to SENDER_ID (see sbp/msg.py).

  """
  _parser = Struct("MsgOrientQuat",
                   ULInt32('tow'),
                   SLInt32('w'),
                   SLInt32('x'),
                   SLInt32('y'),
                   SLInt32('z'),
                   LFloat32('var_w'),
                   LFloat32('var_x'),
                   LFloat32('var_y'),
                   LFloat32('var_z'),
                   ULInt8('flags'),)
  __slots__ = [
               'tow',
               'w',
               'x',
               'y',
               'z',
               'var_w',
               'var_x',
               'var_y',
               'var_z',
               'flags',
              ]

  def __init__(self, sbp=None, **kwargs):
    if sbp:
      super( MsgOrientQuat,
             self).__init__(sbp.msg_type, sbp.sender, sbp.length,
                            sbp.payload, sbp.crc)
      self.from_binary(sbp.payload)
    else:
      super( MsgOrientQuat, self).__init__()
      self.msg_type = SBP_MSG_ORIENT_QUAT
      self.sender = kwargs.pop('sender', SENDER_ID)
      self.tow = kwargs.pop('tow')
      self.w = kwargs.pop('w')
      self.x = kwargs.pop('x')
      self.y = kwargs.pop('y')
      self.z = kwargs.pop('z')
      self.var_w = kwargs.pop('var_w')
      self.var_x = kwargs.pop('var_x')
      self.var_y = kwargs.pop('var_y')
      self.var_z = kwargs.pop('var_z')
      self.flags = kwargs.pop('flags')

  def __repr__(self):
    return fmt_repr(self)

  @staticmethod
  def from_json(s):
    """Given a JSON-encoded string s, build a message object.

    """
    d = json.loads(s)
    return MsgOrientQuat.from_json_dict(d)

  @staticmethod
  def from_json_dict(d):
    sbp = SBP.from_json_dict(d)
    return MsgOrientQuat(sbp, **d)

 
  def from_binary(self, d):
    """Given a binary payload d, update the appropriate payload fields of
    the message.

    """
    p = MsgOrientQuat._parser.parse(d)
    for n in self.__class__.__slots__:
      setattr(self, n, getattr(p, n))

  def to_binary(self):
    """Produce a framed/packed SBP message.

    """
    c = containerize(exclude_fields(self))
    self.payload = MsgOrientQuat._parser.build(c)
    return self.pack()

  def to_json_dict(self):
    self.to_binary()
    d = super( MsgOrientQuat, self).to_json_dict()
    j = walk_json_dict(exclude_fields(self))
    d.update(j)
    return d
    
SBP_MSG_ORIENT_EULER = 0x0221
class MsgOrientEuler(SBP):
  """SBP class for message MSG_ORIENT_EULER (0x0221).

  You can have MSG_ORIENT_EULER inherit its fields directly
  from an inherited SBP object, or construct it inline using a dict
  of its fields.

  
  This message reports the yaw, pitch, and roll angles of the vehicle body frame.
The rotations should applied intrinsically in the order yaw, pitch, and roll 
in order to rotate the from a frame aligned with hte local-level NED frame 
to the vehicle body frame.


  Parameters
  ----------
  sbp : SBP
    SBP parent object to inherit from.
  tow : int
    GPS Time of Week
  roll : int
    rotation about the forward axis of the vehicle
  pitch : int
    rotation about the rightward axis of the vehicle
  yaw : int
    rotation about the downward axis of the vehicle
  var_roll : float
    Variance of roll
  var_pitch : float
    Variance of pitch
  var_yaw : float
    Variance of yaw
  flags : int
    Status flags
  sender : int
    Optional sender ID, defaults to SENDER_ID (see sbp/msg.py).

  """
  _parser = Struct("MsgOrientEuler",
                   ULInt32('tow'),
                   SLInt16('roll'),
                   SLInt16('pitch'),
                   SLInt16('yaw'),
                   LFloat32('var_roll'),
                   LFloat32('var_pitch'),
                   LFloat32('var_yaw'),
                   ULInt8('flags'),)
  __slots__ = [
               'tow',
               'roll',
               'pitch',
               'yaw',
               'var_roll',
               'var_pitch',
               'var_yaw',
               'flags',
              ]

  def __init__(self, sbp=None, **kwargs):
    if sbp:
      super( MsgOrientEuler,
             self).__init__(sbp.msg_type, sbp.sender, sbp.length,
                            sbp.payload, sbp.crc)
      self.from_binary(sbp.payload)
    else:
      super( MsgOrientEuler, self).__init__()
      self.msg_type = SBP_MSG_ORIENT_EULER
      self.sender = kwargs.pop('sender', SENDER_ID)
      self.tow = kwargs.pop('tow')
      self.roll = kwargs.pop('roll')
      self.pitch = kwargs.pop('pitch')
      self.yaw = kwargs.pop('yaw')
      self.var_roll = kwargs.pop('var_roll')
      self.var_pitch = kwargs.pop('var_pitch')
      self.var_yaw = kwargs.pop('var_yaw')
      self.flags = kwargs.pop('flags')

  def __repr__(self):
    return fmt_repr(self)

  @staticmethod
  def from_json(s):
    """Given a JSON-encoded string s, build a message object.

    """
    d = json.loads(s)
    return MsgOrientEuler.from_json_dict(d)

  @staticmethod
  def from_json_dict(d):
    sbp = SBP.from_json_dict(d)
    return MsgOrientEuler(sbp, **d)

 
  def from_binary(self, d):
    """Given a binary payload d, update the appropriate payload fields of
    the message.

    """
    p = MsgOrientEuler._parser.parse(d)
    for n in self.__class__.__slots__:
      setattr(self, n, getattr(p, n))

  def to_binary(self):
    """Produce a framed/packed SBP message.

    """
    c = containerize(exclude_fields(self))
    self.payload = MsgOrientEuler._parser.build(c)
    return self.pack()

  def to_json_dict(self):
    self.to_binary()
    d = super( MsgOrientEuler, self).to_json_dict()
    j = walk_json_dict(exclude_fields(self))
    d.update(j)
    return d
    
SBP_MSG_ANGULAR_RATE = 0x0222
class MsgAngularRate(SBP):
  """SBP class for message MSG_ANGULAR_RATE (0x0222).

  You can have MSG_ANGULAR_RATE inherit its fields directly
  from an inherited SBP object, or construct it inline using a dict
  of its fields.

  
  This message reports the orientation rates in the vehicle body frame. 
The values represent the measurements a strapped down gyroscope would 
make and are not equivalent to the time derivative of the Euler angles.
By convention, the vehicle x-axis is expected to be aligned with the forward
direction, while the vehicle y-axis is expected to be aligned with the right
direction, and the vehicle z-axis should be aligned with the down direction.


  Parameters
  ----------
  sbp : SBP
    SBP parent object to inherit from.
  tow : int
    GPS Time of Week
  x : int
    angular rate about x axis
  y : int
    angular rate about y axis
  z : int
    angular rate about z axis
  flags : int
    Status flags
  sender : int
    Optional sender ID, defaults to SENDER_ID (see sbp/msg.py).

  """
  _parser = Struct("MsgAngularRate",
                   ULInt32('tow'),
                   SLInt16('x'),
                   SLInt16('y'),
                   SLInt16('z'),
                   ULInt8('flags'),)
  __slots__ = [
               'tow',
               'x',
               'y',
               'z',
               'flags',
              ]

  def __init__(self, sbp=None, **kwargs):
    if sbp:
      super( MsgAngularRate,
             self).__init__(sbp.msg_type, sbp.sender, sbp.length,
                            sbp.payload, sbp.crc)
      self.from_binary(sbp.payload)
    else:
      super( MsgAngularRate, self).__init__()
      self.msg_type = SBP_MSG_ANGULAR_RATE
      self.sender = kwargs.pop('sender', SENDER_ID)
      self.tow = kwargs.pop('tow')
      self.x = kwargs.pop('x')
      self.y = kwargs.pop('y')
      self.z = kwargs.pop('z')
      self.flags = kwargs.pop('flags')

  def __repr__(self):
    return fmt_repr(self)

  @staticmethod
  def from_json(s):
    """Given a JSON-encoded string s, build a message object.

    """
    d = json.loads(s)
    return MsgAngularRate.from_json_dict(d)

  @staticmethod
  def from_json_dict(d):
    sbp = SBP.from_json_dict(d)
    return MsgAngularRate(sbp, **d)

 
  def from_binary(self, d):
    """Given a binary payload d, update the appropriate payload fields of
    the message.

    """
    p = MsgAngularRate._parser.parse(d)
    for n in self.__class__.__slots__:
      setattr(self, n, getattr(p, n))

  def to_binary(self):
    """Produce a framed/packed SBP message.

    """
    c = containerize(exclude_fields(self))
    self.payload = MsgAngularRate._parser.build(c)
    return self.pack()

  def to_json_dict(self):
    self.to_binary()
    d = super( MsgAngularRate, self).to_json_dict()
    j = walk_json_dict(exclude_fields(self))
    d.update(j)
    return d
    

msg_classes = {
  0x020F: MsgBaselineHeading,
  0x0220: MsgOrientQuat,
  0x0221: MsgOrientEuler,
  0x0222: MsgAngularRate,
}