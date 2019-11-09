s.options.numBuffers = 2048 * 256;
s.options.maxNodes = 2048 * 512;
s.options.memSize = 8192 * 128;
s.waitForBoot {
  SuperDirt.start
}



(
SynthDef(\cbsp, {|out,sustain=1,freq=440,ffreq=3000,speed=1,begin=0,end=1,pan,accelerate,offset|
  var env, sig;
  env = EnvGen.ar(Env.perc(0.001, 0.999, 1, -3), timeScale:sustain, doneAction:2);
  sig = Mix.ar([
	LFSaw.ar(freq, 0.7),
	LFPulse.ar(freq * 1.3, 0.05, 0.3)
  ]);
  4.do({
	sig = CombN.ar(
	  sig,
	  0.01,
	  SinOsc.kr(
		ExpRand(0.01, 0.99),
		ExpRand(0.01, 0.99),
		ExpRand(0.001, 0.009),
		0.01),
	  1);
  });
  sig = LeakDC.ar(sig);
  OffsetOut.ar(out, DirtPan.ar(sig, ~dirt.numChannels, pan, env));
}).store;

SynthDef(\cbpm, {|out,sustain=1,freq=440,ffreq=3000,speed=1,begin=0,end=1,pan,accelerate,offset|
  var env, sig;
  env = EnvGen.ar(Env.perc(0.001, 0.999, 1, -3), timeScale:sustain, doneAction:2);
  sig = PMOsc.ar(
	freq,
	freq * ExpRand(0.8, 30) * env,
	ExpRand(5, 30) * env,
	ExpRand(1, 10) * env);
  4.do({sig = CombN.ar(
	sig, 0.01, SinOsc.kr(ExpRand(0.05, 0.99), ExpRand(0.01, 0.99), ExpRand(0.001, 0.009), 0.01), 1);
  });
  sig = LeakDC.ar(sig*0.25);
  OffsetOut.ar(out, DirtPan.ar(sig, ~dirt.numChannels, pan, env);
  )
}).store;



SynthDef(\bs, {|out,sustain=1,freq=440,ffreq=1000,rq=0.2,speed=1,begin=0,end=1,pan,accelerate,offset|
  var fenv, env, sig;
  env = EnvGen.ar(Env.perc(0.0001, sustain, 1, -1), doneAction:2);
  fenv = EnvGen.ar(Env.perc(0.0001, sustain*0.8, 1, -1), doneAction:2);
  sig = RLPF.ar(LFSaw.ar(freq * Line.kr(1,1+accelerate, sustain)), ffreq * fenv * 0.8, rq);
  sig = tanh(sig*3);
  OffsetOut.ar(out, DirtPan.ar(sig, ~dirt.numChannels, pan, env));
}).store;


SynthDef(\fb01, {|out,sustain=1,freq=440,pan,accelerate,offset|
  var env, feedback, sig, c1, c2;
  feedback=LocalIn.ar(2);
  sig=SinOscFB.ar([
	LFNoise2.kr(0.01).range(0.00001,0.1)**feedback[1],
	LFNoise2.kr(0.02).range(0.00001,0.1)**feedback[0]
  ], LFNoise0.kr(
	LFNoise2.kr(
	  LFNoise2.kr(0.3).range(0.0001,10)
	).range(0.001,5)**feedback
  ).range(0.0125,4));
  c1 = FFT(LocalBuf(2048), sig[0]);
  c2 = FFT(LocalBuf(2048), sig[1]);
  c1 = PV_BrickWall(c1, LFNoise0.kr(LFNoise2.kr(0.2).range(0.000001,2)**feedback[1]));
  c2 = PV_BrickWall(c2, LFNoise0.kr(LFNoise2.kr(0.21).range(0.000001,2)**feedback[0]));
  sig = [IFFT(c1), IFFT(c2)];
  sig=(sig**feedback).tanh;
  LocalOut.ar(sig);
  env = EnvGen.kr(Env.linen(0.001, 1, 0, -4), timeScale:sustain, doneAction:2);
  OffsetOut.ar(out, sig * env * 0.3);
}).store;



SynthDef(\kik1, {|out=0, freq=100,pan=0.5, sustain=0.5, a=0.00001, d=3|
  var env = EnvGen.ar(Env.perc(a, d, 1, -6), doneAction:2);
  var fenv = EnvGen.ar(Env.perc(a, 0.08, 1, -6), doneAction:0);
  var f = 120;
  var o = PMOsc.ar(fenv * f + 40, fenv * f * 1.8, fenv * 10);
  OffsetOut.ar(out, DirtPan.ar(o * 0.5, 2, pan, env));
}).store;


(
SynthDef(\hh1, {|out=0, freq=20000, pan=0.5, sustain=0.5, a=0.00001, d=0.05|
  var env = EnvGen.ar(Env.perc(a, d, 1, -4), doneAction:2), f = 20000;
  var o = PMOsc.ar(f, f*2.01, 10 * env);
  OffsetOut.ar(out, DirtPan.ar(o * 0.5, 2, pan, env));
}).store;




SynthDef(\p1, {|out=0, freq=1000, pan=0.5, sustain=0.5, a=0.00001, d=0.05|
  var env = EnvGen.ar(Env.perc(a, d * ExpRand(1, 1.5), 1, -4), doneAction:2);
  var o = SinOsc.ar(freq, ExpRand(0, 0.99)) * env;
  o = AllpassN.ar(o, 0.1, ExpRand(0.05, 1), 1.0);
  OffsetOut.ar(out, DirtPan.ar(o, 2, pan));
}).store;
)















