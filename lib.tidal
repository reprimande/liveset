{-- Initial --}

:load /home/reprimande/src/github.com/reprimande/tidal-tools/zzz.hs
import Sound.Tidal.Zzz

let ps = [
      S "synth" (Just ""),
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
      I "pgm" (Just 0)
      ]
    zzzShape = (zzz ps)
    synth = makeS zzzShape "synth"
    ch = makeI zzzShape "ch"
    note = makeI zzzShape "note"
    cv = makeF zzzShape "cv"
    length = makeF zzzShape "length"
    slew = makeF zzzShape "slew"
    device = makeS zzzShape "device"
    mul = makeF zzzShape "mul"
    attack = makeF zzzShape "attack"
    decay = makeF zzzShape "decay"
    atk = makeF zzzShape "attack"
    dcy = makeF zzzShape "decay"
    gain = makeF zzzShape "gain"
    gate = makeI zzzShape "gate"
    offset = makeF zzzShape "offset"
    curve = makeF zzzShape "curve"
    attack2 = makeF zzzShape "attack2"
    decay2 = makeF zzzShape "decay2"
    atk2 = makeF zzzShape "attack2"
    dcy2 = makeF zzzShape "decay2"
    gain2 = makeF zzzShape "gain2"
    gate2 = makeI zzzShape "gate2"
    offset2 = makeF zzzShape "offset2"
    curve2 = makeF zzzShape "curve2"
    acc = makeI zzzShape "acc"
    pgm = makeI zzzShape "pgm"


s0 <- zzzStream "/zzz" 12345 ps
s1 <- zzzStream "/zzz" 12345 ps
s2 <- zzzStream "/zzz" 12345 ps
s3 <- zzzStream "/zzz" 12345 ps
s4 <- zzzStream "/zzz" 12345 ps
s5 <- zzzStream "/zzz" 12345 ps
s6 <- zzzStream "/zzz" 12345 ps
s7 <- zzzStream "/zzz" 12345 ps
s8 <- zzzStream "/zzz" 12345 ps
let hush' = mapM_ ($ silence) [s0,s1,s2,s3,s4,s5,s6,s7,s8]

funcA a d g = attack a # decay d # gain g # gate 1
funcB a d g = attack2 a # decay2 d # gain2 g # gate2 1
funcB' a d g o = funcB a d g # offset2 o
sr a b = scale a b rand

step :: String -> String -> Pattern String
step s steps = fastcat $ map f steps
    where f c | c == 'x' = atom s
              | c >= '0' && c <= '9' = atom $ s
              | otherwise = silence



data ParamSet = ParamSet { p_n  :: Pattern Int,
                     p_a1 :: Pattern Double,
                     p_d1 :: Pattern Double,
                     p_a2 :: Pattern Double,
                     p_d2 :: Pattern Double,
                     p_i2 :: Pattern Double
                   } deriving (Show)

defP = ParamSet { p_n  = 0, p_a1 = 0, p_d1 = 0, p_a2 = 0, p_d2 = 0, p_i2 = 0 }

baP = defP { p_n = 0 }
bdP = defP
chP = defP
ohP = defP
cpP = defP
atP = defP { p_n = 24, p_d1 = 0.3 }
thP = defP { p_n = 36, p_d1 = 0.3 }
bjP = defP { p_n = 24, p_d1 = 0.3 }
acP = defP { p_n = 48, p_a1 = 0.0001, p_d1 = 0.3, p_a2 = 0.0001, p_d2 = 0.3, p_i2 = 0.2 }
dpP = defP { p_n = 60, p_a1 = 0.0001, p_d1 = 0.3, p_a2 = 0.0001, p_d2 = 0.3, p_i2 = 0.2 }

perc s p = gate p # synth s
perc' s p = perc s p # note (p_n a) # decay (p_d1 a)
voice s p = perc s p # note (p_n a) # funcA (p_a1 a) (p_d1 a) 1 # funcB (p_a2 a) (p_d2 a) (p_i2 a)

ba p = perc "ba" p
bd p = perc "bd" p
ch p = perc "ch" p
oh p = perc "oh" p
cp p = perc "cp" p
at p = perc "at" p
th p = perc "th" p
bj p = perc "bj" p
ac p = perc "ac" p # note 48 # a1 0.001 # d1 0.3 # i1 1 # a2 0.001 # d2 0.2 # i2 0.5 # gate2 p
dp p = perc "dp" p # note 24 # a1 0.001 # d1 0.3 # i1 1 # a2 0.001 # d2 0.2 # i2 0.5 # gate2 p

ssBy p n d = sometimesBy p $ stut n 1 d
cs xs = (fast 16 $ choose xs)
nc xs = note $ cs xs
oc xs = note $ cs [x * 12 | x <- xs]



a1 n = attack n
d1 n = decay n
i1 n = gain n
a2 n = attack2 n
d2 n = decay2 n
i2 n = gain2 n
a1r n x = a1 $ (11 <~) $ sr n x
d1r n x = d1 $ (22 <~) $ sr n x
i1r n x = i1 $ (33 <~) $ sr n x
a2r n x = a2 $ (44 <~) $ sr n x
d2r n x = d2 $ (55 <~) $ sr n x
i2r n x = i2 $ (66 <~) $ sr n x


hush'
