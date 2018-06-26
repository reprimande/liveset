{-# LANGUAGE NoMonomorphismRestriction #-}

module Liveset.Modular.Lib where

import qualified Data.Char as Char
import Sound.Tidal.Context
import Liveset.Modular.Stream as S

a1 = attack1
d1 = decay1
i1 = gain1
o1 = offset1
a2 = attack2
d2 = decay2
i2 = gain2
o2 = offset2
sl = slew
rr xs = foldEvery xs (spreadr (<~) [0.125, 0.25, 0.5, 0.75])
zz xs = foldEvery xs (zoom (0, 0.5))
stutS n d = stut n 1 d
ss x s e = sometimesBy x (within (s, e) (spreadr ($) [(stutS 3 0.25), (stutS 4 0.25), (stutS 5 0.3)]))
ssA x = ss x 0 0.5
ssB x = ss x 0.5 1
br r f = spreadr f [ x * r | x <- [1,2,3,4,5,6,7,8]] . sometimes (zoom (0,0.5))
brF r = br r fast
brS r =  br r slow
db = degradeBy
funcA a d g = a1 a # d1 d # gain1 g # gate1 1
funcB a d g = a2 a # d2 d # gain2 g # gate2 1
funcB' a d g o = funcB a d g # o2 o
sr a b = scale a b rand
ssBy p n d = sometimesBy p $ stut n 1 d
cs xs = (fast 16 $ choose xs)
nc xs = note (cs xs)
oc xs = note $ cs [x * 12 | x <- xs]
no ns os = nc ns |+| oc os
ra1 n x = a1 $ (11 <~) $ sr n x
rd1 n x = d1 $ (22 <~) $ sr n x
ri1 n x = i1 $ (33 <~) $ sr n x
ro1 n x = o1 $ (44 <~) $ sr n x
ra2 n x = a2 $ (55 <~) $ sr n x
rd2 n x = d2 $ (66 <~) $ sr n x
ri2 n x = i2 $ (77 <~) $ sr n x
ro2 n x = o2 $ (88 <~) $ sr n x
rsl n x = slew $ (99 <~)$ sr n x
ra1S = ra1 0.00001 0.00002
ra1M = ra1 0.01 0.1
ra1L = ra1 0.03 0.3
rd1S = rd1 0.1 0.2
rd1M = rd1 0.3 0.6
rd1L = rd1 0.5 3
ra2S = ra2 0.00001 0.00002
ra2M = ra2 0.01 0.1
ra2L = ra2 0.03 0.3
rd2S = rd2 0.1 0.2
rd2M = rd2 0.3 0.6
rd2L = rd2 0.5 3
ba p = (101 <~) $ gate1 p # synth "ba"
bd p = (102 <~) $ gate1 p # synth "bd"
ch p = (103 <~) $ gate1 p # synth "ch"
oh p = (104 <~) $ gate1 p # synth "oh"
cp p = (105 <~) $ gate1 p # synth "cp"
ak p = (106 <~) $ gate1 p # synth "at" # note 36 # d1 0.1
th p = (107 <~) $ gate1 p # synth "th" # note 48 # d1 0.3
bj p = (108 <~) $ gate1 p # synth "bj" # note 12 # d1 0.3
ac p = (109 <~) $ gate1 p # synth "ac" # note 48 # a1 0.001 # d1 0.3 # i1 1 # a2 0.001 # d2 0.2 # i2 0.5 # gate2 p
dp p = (110 <~) $ gate1 p # synth "dp" # note 24 # a1 0.001 # d1 0.3 # i1 1 # a2 0.001 # d2 0.2 # i2 0.5 # gate2 p
pg p xs = synth "ph" # mul (choose xs)

lsysP x n r s = p $ "{" ++ pat ++ "}%" ++ (show x)
  where ds = ["~", "1"] ++ map (\x -> "1/" ++ (show x)) [2..16]
        lsys = take 128 $ lindenmayer n r s
        pat = intercalate " " (map (\x -> ds !! (Char.digitToInt x)) lsys)

lsysN x n r s ns = p $ "{" ++ pat ++ "}%" ++ (show x)
  where lsys = lindenmayer n r s
        l = length ns
        note x
          | Char.isHexDigit x == True = show (ns !! (mod (Char.digitToInt x) l))
          | otherwise = "~"
        pat = intercalate " " (map (\x -> note x) lsys)

lsysO x n r s = p $ "{" ++ pat ++ "}%" ++ (show x)
  where lsys = lindenmayer n r s
        oct x
          | Char.isHexDigit x == True = show $ (Char.digitToInt x) * 12
          | otherwise = "~"
        pat = intercalate " " (map (\x -> oct x) lsys)
