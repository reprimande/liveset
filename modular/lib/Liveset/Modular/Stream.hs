{-# LANGUAGE NoMonomorphismRestriction #-}

module Liveset.Modular.Stream where

import Sound.Tidal.Stream
import Sound.Tidal.Transition (transition)
import Sound.Tidal.OscStream

cvParams = [ S "synth" (Just ""),
             I "ch" (Just 0),
             I "note" (Just 0),
             F "cv" (Just 0),
             I "gate" (Just 0),
             I "length" (Just 0),
             F "slew" (Just 0),
             F "attack" (Just 0),
             F "decay" (Just 0),
             F "sustain" (Just 0),
             F "release" (Just 0),
             F "mul" (Just 0),
             F "gain" (Just 0),
             F "offset" (Just 0),
             I "curve" (Just 0),
             F "attack2" (Just 0),
             F "decay2" (Just 0),
             F "sustain2" (Just 0),
             F "release2" (Just 0),
             F "mul2" (Just 0),
             F "gain2" (Just 0),
             F "offset2" (Just 0),
             I "gate2" (Just 0),
             I "curve2" (Just 0),
             I "acc" (Just 0),
             S "device" (Just "es"),
             I "pgm" (Just 0) ]
cvShape = (zzz cvParams)
synth = makeS cvShape "synth"
-- ch = makeI cvShape "ch"
-- note = makeI cvShape "note"
cv = makeF cvShape "cv"
glen = makeF cvShape "length"
slew = makeF cvShape "slew"
device = makeS cvShape "device"
mul = makeF cvShape "mul"
attack1 = makeF cvShape "attack"
decay1 = makeF cvShape "decay"
gain1 = makeF cvShape "gain"
gate1 = makeI cvShape "gate"
offset1 = makeF cvShape "offset"
curve1 = makeF cvShape "curve"
attack2 = makeF cvShape "attack2"
decay2 = makeF cvShape "decay2"
gain2 = makeF cvShape "gain2"
gate2 = makeI cvShape "gate2"
offset2 = makeF cvShape "offset2"
curve2 = makeF cvShape "curve2"
acc = makeI cvShape "acc"
pgm = makeI cvShape "pgm"

zzz :: [Param] -> Shape
zzz ps = Shape {
  params = ps,
  cpsStamp = True,
  latency = 0
  }

zzzSlang path = OscSlang {
  path = path,
  preamble = [],
  namedParams = True,
  timestamp = NoStamp
  }

zzzBackend path port = do
  s <- makeConnection "127.0.0.1" port (zzzSlang path)
  return $ Backend s (\_ _ _ -> return ())

zzzState port shape = do
  backend <- zzzBackend "/zzz" port
  Sound.Tidal.Stream.state backend shape

zzzSetters getNow = do zs <- zzzState 12345 (zzz cvParams)
                       return (setter zs, transition getNow zs)
