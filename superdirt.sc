include("Vowel")





Platform.userExtensionDir

Platform.systemExtensionDir

Quarks.gui

s.quit

Platform.userExtensionDir

(
s.options.numBuffers = 2048 * 256;
s.options.maxNodes = 2048 * 512;
s.options.memSize = 8192 * 128;
s.latency_(0.1);
s.waitForBoot {
  SuperDirt.start()
}
)



s.quit


~dirt.numChannels

s.dumpOSC

s.dumpOSC(0)

s.record
s.stopRecording

s.record


include("SuperDirt")

include("DirtSample")

include("MembraneHexagon")

include("MCLDUGens")

s.dumpOSC(0)
s.plotTree


Quarks.install("MembraneHexagon")



(
SynthDef(\sawc, {|out,sustain=1,freq=440,ffreq=3000,speed=1,begin=0,end=1,pan,accelerate,offset|
  var env, sig;
  env = EnvGen.ar(Env.perc(0.001, 0.999, 1, -3), timeScale:sustain, doneAction:2);
  sig = LFSaw.ar(freq, 0.8) + LFPulse.ar(freq * 1.18, 0.01, 0.2);
  sig = CombN.ar(sig, 0.01, SinOsc.kr(ExpRand(0.05, 0.99), ExpRand(0.01, 0.99), 0.001, 0.01), 1);
  sig = CombN.ar(sig, 0.05, SinOsc.kr(ExpRand(0.05, 0.99), ExpRand(0.01, 0.99), 0.002, 0.01), 1);
  sig = CombN.ar(sig, 0.05, SinOsc.kr(ExpRand(0.05, 0.99), ExpRand(0.01, 0.99), 0.008, 0.01), 1);
  sig = CombN.ar(sig, 0.05, SinOsc.kr(ExpRand(0.05, 0.99), ExpRand(0.01, 0.99), 0.009, 0.01), 1);
  // sig = CombN.ar(sig, 0.03, SinOsc.kr(ExpRand(0.1, 0.9), 0.8, 0.002, 0.001), 1);
  // sig = CombN.ar(sig, 0.03, SinOsc.kr(0.2, 0.88, 0.009, 0.001), 1);
  // sig = CombN.ar(sig, 0.02, SinOsc.kr(0.1, 0.1, 0.006, 0.005), 1);
  sig = LeakDC.ar(sig);
  OffsetOut.ar(out, DirtPan.ar(sig, ~dirt.numChannels, pan, env);
  )
}).add
)

(
SynthDef(\pmc, {|out,sustain=1,freq=440,ffreq=3000,speed=1,begin=0,end=1,pan,accelerate,offset|
  var env, sig;
  env = EnvGen.ar(Env.perc(0.001, 0.999, 1, -3), timeScale:sustain, doneAction:2);
  sig = PMOsc.ar(
	freq,
	freq * ExpRand(0.8, 30) * env,
	ExpRand(5, 30) * env,
	ExpRand(1, 10) * env);
  4.do({sig = CombN.ar(sig, 0.01, SinOsc.kr(ExpRand(0.05, 0.99), ExpRand(0.01, 0.99), ExpRand(0.001, 0.009), 0.01), 1);  });
  sig = LeakDC.ar(sig);
  OffsetOut.ar(out, DirtPan.ar(sig, ~dirt.numChannels, pan, env);
  )
}).add
)



(
SynthDef(\pmc2, {|out,sustain=1,freq=440,ffreq=3000,speed=1,begin=0,end=1,pan,accelerate,offset|
  var env, sig;
  env = EnvGen.ar(Env.perc(0.001, 0.999, 1, -3), timeScale:sustain, doneAction:2);
  sig = PMOsc.ar(
	freq,
	freq * ExpRand(0.8, 30) * env,
	ExpRand(5, 30) * env,
	ExpRand(1, 10) * env);
  sig = Mix.fill(4, {CombN.ar(sig, 0.01, SinOsc.kr(ExpRand(0.05, 0.99), ExpRand(0.01, 0.99), ExpRand(0.001, 0.009), 0.01), 1);});
  sig = LeakDC.ar(sig);
  OffsetOut.ar(out, DirtPan.ar(sig, ~dirt.numChannels, pan, env);
  )
}).add
)


(
SynthDef("supersaw", {|freq = 440, gain=1|
  var n = 10, rand_freq_range = 3, rand_variable_range = 3, rand_amp_range = 0.1;
  Out.ar(0, DirtPan.ar(
	Mix.fill(n, {
	  Saw.ar(freq + rand_freq_range.rand2 + Rand.new (0.0 - rand_variable_range, rand_variable_range), (1 / n) * gain) }
	) + Rand.new (0.0 - rand_amp_range, rand_amp_range)));
}).store;
)


(
SynthDef("bs", {|out=0 , sustain=0.5, pan, accelerate, freq, ffreq=200, famount=3000, rq=0.1|
  var env = EnvGen.ar(Env.perc(0.001, 0.999, 1, -4), timeScale:sustain, doneAction:2);
  var o = Pulse.ar(freq * Line.kr(1,1+accelerate, sustain), SinOsc.kr(ExpRand(0.01,0.08)));
  o = RLPF.ar(o, freq + (env * famount), rq);
  OffsetOut.ar(out, DirtPan.ar(o, 2, pan, env));
}).store;
)


(
SynthDef("pmbass", {|out=0 , sustain=0.5, pan, accelerate, freq, ffreq=200, famount=3000, rq=0.1|
  var env = EnvGen.ar(Env.perc(0.001, 0.999, 1, -4), timeScale:sustain, doneAction:2);
  var o = PMOsc.ar(freq * Line.kr(1,1+accelerate, sustain), freq + (env * 10) * 1.123, 3);
  OffsetOut.ar(out, DirtPan.ar(o, 2, pan, env));
}).store;
)


SynthDef("chop", {|out=0 , sustain=0.5, pan, accelerate, freq, ffreq=200, famount=1500, rq=0.1|
  var env = EnvGen.ar(Env.perc(0.001, 0.999, 1, -4), timeScale:0.05, doneAction:2);
  var o = SinOsc.ar(freq + (2000 * env));
  OffsetOut.ar(out, DirtPan.ar(o, 2, pan, env));
}).store;

(
SynthDef("pm", {|
  out=0 , sustain=0.5, pan, accelerate, freq,
  ffreq=200, famount=1500, rq=0.1, a=0.001,
  r1=1.0, r2=1.0, i1=5, i2=5|
  var env = EnvGen.ar(Env.perc(a, 0.999, 1, -4), timeScale:sustain, doneAction:2);
  var p = PMOsc.ar(freq*1.3, freq*r2*env, i2*env);
  var o = PMOsc.ar(freq*p, freq*r1*env, i1*env) * 0.5;
  OffsetOut.ar(out, DirtPan.ar(o, 2, pan, env));
}).store;
)


(
SynthDef("pm5", {|
  out=0 , midinote,sustain=0.5, pan, accelerate, freq,
  ffreq=200, famount=1500, rq=0.1, a=0.001,
  r1=1.0, r2=1.0, i1=5, i2=5|
  var env = EnvGen.ar(Env.perc(a, 0.999, 1, -4), timeScale:sustain, doneAction:2);
  var f = ([0,3,6,11,14] + midinote).midicps;
  var o = Mix.ar(PMOsc.ar(
	f* PMOsc.ar(f*1.3, f*r2, i2*env),
	f*r1,
	i1*env
  )) * 0.25;
  OffsetOut.ar(out, DirtPan.ar(o, 2, pan, env));
}).store;
)


(
SynthDef("pmpad", {|
  out=0 , sustain=0.5, pan, accelerate, freq,
  ffreq=200, famount=1500, rq=0.1, a=0.001,
  r1=1.03, r2=1.41, i1=3, i2=5|
  var env = EnvGen.ar(Env.perc(a, 0.999, 1, -4), timeScale:sustain, doneAction:2);
  var o = PMOsc.ar(freq, freq*r1, i1);
  OffsetOut.ar(out, DirtPan.ar(o, 2, pan, env));
}).store;
)


(
SynthDef("pmex", {|
  out=0 , sustain=0.5, pan, accelerate, freq,
  fr1=1, fr2=1, fr3=1, mr1=1, mr2=1, mr3=1, i1=1, i2=1, i3=1, m1=1, m2=1, m3=1
  a=0.001|
  var env = EnvGen.ar(Env.perc(a, 0.999, 1, -4), timeScale:sustain, doneAction:2);
  var p1 = PMOsc.ar(
	freq * fr1,
	freq * mr1,
	i1 * env,
	mul: m1 * env
  );
  var p2 = PMOsc.ar(
	freq * fr2,
	freq * mr2,
	i2 * env,
	mul: m2 * env
  );
  var p3 = PMOsc.ar(
	freq * fr3 * p1,
	freq * mr3 * p2,
	i3 * env,
	mul: m3
  );
  OffsetOut.ar(out, DirtPan.ar(p3, 2, pan, env));
}).store;
)

(
SynthDef(\bass, {|out,sustain=1,freq=440,ffreq=1000,rq=0.5speed=1,begin=0,end=1,pan,accelerate,offset|
  var env, sig;
  env = EnvGen.ar(Env.perc(0.0001, sustain, 1, -1), doneAction:2);
  sig = RLPF.ar(Saw.ar(freq * Line.kr(1,1+accelerate, sustain)), ffreq * env + freq, rq).softclip;
  OffsetOut.ar(out, DirtPan.ar(sig, ~dirt.numChannels, pan, env));
}).add
)


Synth(\bass, [\freq, 50, \sustain, 0.2, \rq, 0.1])

// http://mcld.co.uk/blog/2009/reverse-engineering-the-rave-hoover.html
(
SynthDef(\hv, { |out=0,freq=440, pan=0.5|
    var midfreqs, son, vibamount;

    // Portamento:
    var f = freq.lag(0.2, 0.6);
    // you could alternatively try:
    //  freq = Ramp.kr(freq, 0.2);

    // vibrato doesn't fade in until note is held:
    vibamount = EnvGen.kr(Env([0,0,1],[0.0,0.4], loopNode:1), HPZ1.kr(f).abs);
    // Vibrato (slightly complicated to allow it to fade in):
    f = LinXFade2.kr(f, f * LFPar.kr(3).exprange(0.98, 1.02), vibamount * 2 - 1);

    // We want to chorus the frequencies to have a period of 0.258 seconds
    // ie freq difference is 0.258.reciprocal == 3.87
    midfreqs = f + (3.87 * (-2 .. 2));

    // Add some drift to the frequencies so they don't sound so digitally locked in phase:
    midfreqs = midfreqs.collect{|f| f + (LFNoise1.kr(2) * 3) };

    // Now we generate the main sound via Saw oscs:
    son = LFSaw.ar(midfreqs).sum
        // also add the subharmonic, the pitch-locked bass:
        + SinOsc.ar(f * [0.25, 0.5, 0.75], 0, [1, 0.3, 0.2] * 2).sum;

    // As the pitch scoops away, we low-pass filter it to allow the sound to stop without simply gating it
    son = RLPF.ar(son, f * if(f < 100, 1, 32).lag(0.01));

    // Add a bit more mid-frequency emphasis to the sound
    son = son + BPF.ar(son, 1000, mul: 0.5) + BPF.ar(son, 3000, mul: 0.3);

    // This envelope mainly exists to allow the synth to free when needed:
    son = son * 0.5;

    OffsetOut.ar(out, DirtPan.ar(son, 2, pan));
}).store;
)

(
SynthDef("shoover", {|out=0,freq=1000,pan=0.5,sustain=0.5|
  var f = freq * Line.kr(1.4, 1, sustain);
  var o = Mix.ar([
	SyncSaw.ar(freq, f + SinOsc.ar(0.3)),
	SyncSaw.ar(freq, f + SinOsc.ar(0.4) * 1.02),
	SyncSaw.ar(freq, f + SinOsc.ar(0.5) * 1.06)
  ]) * 0.33;
  OffsetOut.ar(0, DirtPan.ar(o, 2, pan))
}).store;
)


s.dumpOSC(0)

(
SynthDef("vosim", { |out = 0, freq = 1000, pan=0.5, sustain=0.5, a=0.00001|
  var vfreq = freq * 1.3;
  var env = EnvGen.ar(Env.perc(a, 0.5, 1, -4), timeScale:sustain, doneAction:2);
  var vo = VOSIM.ar(Impulse.ar(vfreq), vfreq, 20, 0.9);
  var o = Pulse.ar(freq + (vo * env * 200), SinOsc.kr(0.05, 0.001));
  o = RLPF.ar(o, freq + (freq * env * 60), 0.3) * 0.5;
  OffsetOut.ar(out, DirtPan.ar(o, 2, pan, env));
}).store
)

(
SynthDef("kick", {|out=0, freq=50,pan=0.5, sustain=0.5, a=0.00001, d=3|
  var env = EnvGen.ar(Env.perc(a, d, 1, -6), doneAction:2);
  var fenv = EnvGen.ar(Env.perc(a, 0.08, 1, -6), doneAction:0);
  var f = 120;
  o = PMOsc.ar(fenv * f + 20, fenv * f * 1.2, fenv * 3);
  OffsetOut.ar(out, DirtPan.ar(o * 0.5, 2, pan, env));
}).store
)


(
SynthDef("phh", {|out=0, freq=20000, pan=0.5, sustain=0.5, a=0.00001, d=0.05|
  var env = EnvGen.ar(Env.perc(a, d, 1, -4), doneAction:2), f = 20000;
  o = PMOsc.ar(f, f*2.01, 10 * env);
  OffsetOut.ar(out, DirtPan.ar(o * 0.5, 2, pan, env));
}).store
)






(
SynthDef("lf", {|out=0, freq=100, pan=0, sustain=0.5,v1=1,v2=3,v3=0.5,v4=0.5|
  var o = LatoocarfianL.ar(
    SampleRate.ir*((freq*4) / 44100),
    LFNoise2.kr(v1,1.5,1.5),
    LFNoise2.kr(v2,1.5,1.5),
    LFNoise2.kr(v3,0.5,1.5),
    LFNoise2.kr(v4,0.5,1.5));
  var env = EnvGen.kr(Env.linen(0.001, 1, 0, -4), timeScale:sustain, doneAction:2);
  OffsetOut.ar(out, DirtPan.ar(o, 2, pan, env));
}).store
)

(
SynthDef("lf2", {|out=0, freq=100, pan=0, sustain=0.5,v1=1,v2=3,v3=0.5,v4=0.5|
  var o = LatoocarfianN.ar(
    SampleRate.ir*((freq*4) / 44100),
    v1,
    v2,
    v3,
    v4);
  var env = EnvGen.kr(Env.linen(0.001, 1, 0, -4), timeScale:sustain, doneAction:2);
  OffsetOut.ar(out, DirtPan.ar(o, 2, pan, env));
}).store
)


(
SynthDef("hn", {|out=0, freq=100, pan=0, sustain=0.5,v1=1,v2=3|
  var o = HenonN.ar(
	SampleRate.ir * ((freq*4) / 44100),
	LFNoise2.kr(v1,0.2,1.2),
	LFNoise2.kr(v2, 0.15, 0.15));
  var env = EnvGen.kr(Env.linen(0.001, 1, 0, -4), timeScale:sustain, doneAction:2);
  OffsetOut.ar(out, DirtPan.ar(o, 2, pan, env));
}).store
)

(
SynthDef("st1", {|out=0, freq=1000, pan=0, sustain=0.5,k=1, xi=0.5, yi=0|
  var o = StandardN.ar(freq, k, xi, yi);
  OffsetOut.ar(out, DirtPan.ar(o, 2, pan))
}).store
)



{SinOscFB.ar(MouseY.kr(10,1000,'exponential'),MouseX.kr(0.01,100))*0.1}.play

{SinOscFB.ar(100*SinOscFB.ar(MouseY.kr(1,1000,'exponential'))+200,MouseX.kr(0.5pi,pi))*0.1}.play



// Effects

/*
How to add new effects to Tidal and SuperDirt.
This is a three step process.
1. add the desired parameters to Tidal, so it can be used
2. add a module definition to Superdirt, so it can be found when the parameter is not nil
3. add the synth def to SuperDirt, so it can be played
The following example adds a weird spectral delay
This assumes you have an instance of SuperDirt accessible via ~dirt
*/


// in Haskell

// (1) in  Sound/Tidal/Params.hs
// this adds two new  parameters "tsdelay" (float, delay time) and "xsdelay" (int, delay structure)

/*
tsdelay :: Pattern Double -> ParamPattern
tsdelay = make' VF tsdelay_p
tsdelay_p = F "tsdelay" Nothing
xsdelay :: Pattern Int -> ParamPattern
xsdelay = make' VI xsdelay_p
xsdelay_p = I "xsdelay" Nothing
*/

// ... or you can run this in the interpreter:
/*
let tsdelay = make' VF tsdelay_p
    tsdelay_p = F "tsdelay" Nothing
let xsdelay = make' VI xsdelay_p
    xsdelay_p = I "xsdelay" Nothing
*/



// in SuperCollider

// (2) add a module for superdirt
// this adds a responder for the parameter
// for more examples see synths/core-modules

(
~dirt.addModule('spectral-delay', { |dirtEvent|
	dirtEvent.sendSynth('spectral-delay' ++ ~dirt.numChannels,
		// OPTIONAL
		// passing this array of parameters could be left out,
		// but it makes it clear what happens
		[
			xsdelay: ~xsdelay,
			tsdelay: ~tsdelay,
			sustain: ~sustain,
			out: ~out
		]
	)
}, { ~tsdelay.notNil or: { ~xsdelay.notNil } }); // play synth only if at least one of the two was given
)

// here you can see the effect order:
~dirt.modules;

// OPTIONAL: you can reorder the effects, if you want e.g. the lpf to come after the delay:
~dirt.orderModules(['spectral-delay', 'hpf', 'klm']);


// (3) make a synthdef (for more examples see core-synths)
(

var numChannels =  ~dirt.numChannels;

SynthDef("spectral-delay" ++ numChannels, { |out, tsdelay, xsdelay = 1, sustain|

	var signal, delayTime, delays, freqs, filtered;
	var size = 16;
	var maxDelayTime = 0.2;

	signal = In.ar(out, numChannels);
	delayTime = tsdelay * maxDelayTime;
	filtered = (1..size).sum { |i|
		var filterFreq = i.linexp(1, size, 40, 17000);
		var sig = BPF.ar(signal, filterFreq, 0.005);
		// the delay pattern is determined from xsdelay by bitwise-and:
		DelayN.ar(sig, maxDelayTime, i & xsdelay * (1/size) * delayTime )
	};
	signal = signal * 0.2 + (filtered * 4); // this controls wet/dry
	ReplaceOut.ar(out, signal)

}).add;
)

/*
now you should be able to write in tidal:
d1 $ sound "can*4" # tsdelay "0 0.25 0.5 0.75 1" # xsdelay "3 124 3 12 62 2"
*/








{HenonL.ar(LFNoise2.kr(0.3, 10, SampleRate.ir), LFNoise2.kr(2,0.2,1.2), LFNoise2.kr(1, 0.15, 0.15)) }.play

(
{LatoocarfianN.ar(
    SampleRate.ir/4,
    LFNoise2.kr(2,1.5,1.5),
    LFNoise2.kr(2,1.5,1.5),
    LFNoise2.kr(2,0.5,1.5),
    LFNoise2.kr(2,0.5,1.5)
)}.play
)

(
{LatoocarfianN.ar(
    SampleRate.ir/4,
    LFNoise2.kr(ExpRand(0.8,4),1.5,1.5),
    LFNoise2.kr(ExpRand(0.8,4),1.5,1.5),
    LFNoise2.kr(ExpRand(0.8,4),0.5,1.5),
    LFNoise2.kr(ExpRand(0.8,4),0.5,1.5)
)}.play
)



(
{ FBSineN.ar(
    LFNoise2.kr(1, 1e4, 1e4),
    LFNoise2.kr(1, 32, 33),
    LFNoise2.kr(1, 0.5),
    LFNoise2.kr(1, 0.05, 1.05),
    LFNoise2.kr(1, 0.3, 0.3)
) }.play
)

{ CuspN.ar(MouseX.kr(20, SampleRate.ir), 1.0, 1.99)}.play

{ CuspN.ar(SampleRate.ir/4, MouseX.kr(0.9,1.1,1), MouseY.kr(1.8,2,1)) * 0.3 }.play




(
{ CuspN.ar(
    SampleRate.ir/4,
    LFNoise2.kr(2,0.9,1.1),
    LFNoise2.kr(2,1.8,2)
) }.play
)

(
var a = 1, b = -1, c = -0.75, xi = 0, size = 64;
plot(size.collect { xi = (a * (xi ** 2)) + (b * xi) + c; xi });
)

(
{
  var r;
  r = LFNoise2.kr(10,3.5441, 3.8);    // stable range
  QuadN.ar(SampleRate.ir/4, r.neg, r, 0, 0.1)
}.play
)

(
(
{ LinCongN.ar(
    LFNoise2.kr(1, 1e4, 1e4),
    LFNoise2.kr(0.1, 0.5, 1.4),
    LFNoise2.kr(0.1, 0.1, 0.1),
    LFNoise2.kr(0.1)
)}.play
)

(
{
LorenzL.ar(
    SampleRate.ir,
    LFNoise0.kr(1, 2, 10),
    LFNoise0.kr(1, 20, 38),
    LFNoise0.kr(1, 1.5, 2)
)}.play
)



{StandardN.ar(MouseX.kr(20, SampleRate.ir), MouseY.kr(0.9,4))}.play

(
{StandardN.ar(
  LFNoise0.kr(3, SampleRate.ir/8, SampleRate.ir/4),
  LFNoise0.kr(4, 0.9, 4))
}.play
)

{GbmanN.ar(SampleRate.ir/4, MouseX.kr(10, -10), MouseY.kr(10, -10))}.play

s.dumpOSC













/*
Schemawound track for Waxen Wing's "Give Me A Sine" compilation.

Compilation Description: All songs written using ONLY sine waves in their creation. All oscillations, modulations, lfo's, envelopes, etc, use only sine waves. No samples or outside source audio were permitted on this releases, unless of course the samples were of pure sine waves. Download includes full 8 panel artwork and extensive liner notes on each piece written by each artist. 

Download the free compilation here: http://waxenwings.bandcamp.com/album/give-me-a-sine

Blog post about the creation of this track: http://schemawound.tumblr.com/post/24520532915/sinusoid
*/

(
//-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- Sinusoid -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
	SynthDef(\Sinusoid, {
		|
			out = 0,					gate = 1,					amp = 1,					freqArrayMult = 1,		
			mod1FreqRLfoFreq = 0.1,		mod1FreqRLfoDepth = 100,	mod1FreqRLfoOffset = 100,	
			mod2Freq = 50,				combDelay = 0.7, 			combDecay = 9,				
			attack = 0.001 				release = 0.5
		|
		//
		var combMaxDelay = 10;
		//Many Sines
		var freqArray = (1..50) * freqArrayMult; 
		var manySines = Mix(SinOsc.ar(freqArray));
		//Mod1
		var mod1FreqL = SinOsc.kr(150, 0, 20);
		var mod1FreqRLfo = SinOsc.kr(mod1FreqRLfoFreq, 0, mod1FreqRLfoDepth, mod1FreqRLfoOffset);
		var mod1FreqR = SinOsc.kr(mod1FreqRLfo, 0, 37);
		var mod1 = SinOsc.ar([mod1FreqL, mod1FreqR]);
		//Mod2
		var mod2 = SinOsc.ar(mod2Freq);
		//Sum and FX
		var sinSum = manySines * mod1 * mod2;
		var comb = sinSum; //+ CombC.ar(sinSum, combMaxDelay, combDelay, combDecay);
		var dist = comb.tanh;
		var env = dist * Linen.kr(gate, attack, amp, release, doneAction: 2);
		//Output
		Out.ar(out, env);
	}).add;

//-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- ELEKTRO KICK -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
	SynthDef(\ElektroKick, { 
		|
			out = 0,		gate = 1,		amp = 1,			freqArrayMult = 1,		
			basefreq = 50,	envratio = 3, 	freqdecay = 0.02, 	ampdecay = 0.5
		|
		var fenv = EnvGen.ar(Env([envratio, 1], [freqdecay], \exp), 1) * basefreq;
		var aenv = EnvGen.ar(Env.perc(0.005, ampdecay), 1, doneAction:2);
		var output = SinOsc.ar(fenv, 0.5pi, aenv) * amp;
		Out.ar(out, output!2);
	}).add;

//-=-=-=-=-=-=-=-=-=-=-=-=-=-=-=- VERB -=-=-=-=-=-=-=-=-=-=-=-=-=-=-=-
	SynthDef(\Verb, {
		arg 	out = 0,	in,
			mix = 0.25,	room = 0.15,	damp = 0.5;
	
		var input, verb;
		
		input = In.ar(in);
		verb = FreeVerb.ar(input, mix, room, damp);
		Out.ar(out, verb!2);
	}).add;
)

(
	//Groups and Busses
	var sourceGroup = Group.new;
	var fxGroup = Group.after(~sourceGroup);
	var verbBus = Bus.audio(s, 2);
	var mainOut = 0;
	var verb = Synth.tail(~fxGroup, \Verb, [\in, verbBus, \out, mainOut, \mix, 1, \room, 1]);

	//Song Variables
	var bar = 0.94;
	var qNote = bar/4;
	var eNote = bar/8;

	//Mix
	var finalAmp 	= 0.1;
	var hatAmp 		= 0.6 * finalAmp;
	var bassAmp 	= 0.8 * finalAmp;
	var loToneAmp 	= 0.8 * finalAmp;
	var hiWhineAmp 	= 0.7 * finalAmp;
	var eKickAmp 	= 2.1 * finalAmp;

	//Basic Patterns
	var hat = {|beatsPerMeasure = 9, freqArrayMult = 1|
		Pbind(*[instrument: \Sinusoid, amp: hatAmp, group: sourceGroup,
			dur:				Pseq([bar / beatsPerMeasure], beatsPerMeasure),
			freqArrayMult:		Pxrand((1..12), inf),
			mod2Freq:			Pwhite(60, 6000, inf),
			mod1FreqRLfoFreq:	0.01,
			mod1FreqRLfoDepth:	Pwhite(1000, 3000, inf),
			mod1FreqRLfoOffset:	Pwhite(10, 300, inf),
			release:			Pkey(\dur)
		])
	};
	var bass = {|beatsPerMeasure|
		Pbind(*[instrument: \Sinusoid, amp: bassAmp, group: sourceGroup,
			dur:				Pseq([bar / beatsPerMeasure], beatsPerMeasure),
			mod2Freq:			Pwhite(30, 300, inf),
			mod1FreqRLfoFreq:	Pwhite(0.1, 0.3, inf),
			mod1FreqRLfoDepth:	Pwhite(10, 300, inf),
			mod1FreqRLfoOffset:	Pwhite(10, 300, inf),
			release:			0.001
		])
	};
	var lowtone = {|beatsPerMeasure = 1, attack = 0.001, out = 0|
		Pbind(*[instrument: \Sinusoid, amp: loToneAmp, group: sourceGroup,
			dur:				Pseq([bar / beatsPerMeasure], beatsPerMeasure),
			mod2Freq:			Pwhite(30, 400, inf),
			mod1FreqRLfoFreq:	Pwhite(0.1, 0.3, inf),
			mod1FreqRLfoDepth:	Pwhite(10, 300, inf),
			mod1FreqRLfoOffset:	Pwhite(10, 300, inf),
			attack:				attack,
			release:			Pkey(\dur),
			out:				out
		])
	};
	var lowtoneLong = Pbind(*[instrument: \Sinusoid, amp: loToneAmp, group: sourceGroup,
		dur:				Pseq([bar*8], 1),
		freqArrayMult:		3,
		mod2Freq:			50,
		mod1FreqRLfoFreq:	0.1,
		mod1FreqRLfoDepth:	100,
		mod1FreqRLfoOffset:	100,
		release:			3
	]);
	var hiWhine = {|out = 0|
		Pbind(*[instrument: \Sinusoid, amp: hiWhineAmp, group: sourceGroup,
			dur:				Pseq([bar], 1),
			mod2Freq:			2000,
			mod1FreqRLfoFreq:	Pwhite(0.1, 0.3, inf),
			mod1FreqRLfoDepth:	100,
			mod1FreqRLfoOffset:	100,
			release:			Pkey(\dur),
			out:				out
		])
	};
	var elektroKick = {|beatsPerMeasure = 1|
		Pbind(*[instrument: \ElektroKick, amp: eKickAmp, group: sourceGroup,
			dur:				Pseq([bar / beatsPerMeasure], beatsPerMeasure),
			basefreq:			Pwhite(70, 75),
			ampdecay:			2,
			envratio:			1,
			freqdecay:			1
		])
	};

	//8 Bar Patterns
	var loTonePat = [
		Pn(lowtone.(1), 8),					//loTone pattern 0 - 8 bars
		Pn(lowtone.(1, bar), 8),				//loTone pattern 1 - 8 bars
		Pn(lowtone.(1, out:verbBus), 8)	//loTone pattern 2 - 8 bars
	];

	var hiWhinePat = [
		Pn(hiWhine.(verbBus), 8),	//hiWhine pattern 0 - 8 bars
		Pn(hiWhine.(mainOut), 8)	//hiWhine pattern 0 - 8 bars
	];

	var hatPat = [
		Pseq([//hat pattern 0 - 8 bars
			Pn(hat.(9), 7),		hat.(11)
		]),	
		Pseq([//hat pattern 1 - 8 bars
			hat.(8), 			hat.(9,(1..12)),	hat.(9), 			hat.(7,(1..12)),
			hat.(6,(1..12)),	hat.(12,(1..12)),	hat.(6,(1..12)),	hat.(24,(1..12))
		]),
		Pseq([//hat pattern 2 - 8 bars
			hat.(8), 			hat.(3),			hat.(6),			hat.(9),
			hat.(8,(1..12)),	hat.(3,(1..12)),	hat.(6,(1..12)),	hat.(12,(1..12))
		]),
		Pseq([//hat pattern 3 - 8 bars
			hat.(9), 			hat.(8),			hat.(7),			hat.(8),
			hat.(9,(1..12)),	hat.(8,(1..12)),	hat.(16,(1..12)),	hat.(32,(1..12))
		])
	];

	var bassPat = [
		Pn(bass.(3), 8),	//bass pattern 0 - 8 bars
		Pseq([				//bass pattern 1 - 8 bars
			bass.(4),	Pn(bass.(3),3)
		], 2),
		Pseq([				//bass pattern 2 - 8 bars
			bass.(4),	Pn(bass.(3),3),		
			bass.(4),	Pn(bass.(3),2), 	bass.(5)
		]),
		Pseq([				//bass pattern 3 - 8 bars
			bass.(4), 	bass.(3.5), 		bass.(3),		bass.(3.5),
			bass.(4), 	bass.(3), 			bass.(6),		bass.(7)
		]),
		Pseq([				//bass pattern 4 - 8 bars
			bass.(4),	Pn(bass.(3), 7)
		]),
	];

	var kickPat = [
		Pn(elektroKick.(1), 8),		//kick pattern 0 - 8 bars
		Pn(elektroKick.(2), 8)	//kick pattern 1 - 8 bars
	];

	var drop = [
		Pn(Ppar([lowtone.(1), hiWhine.(verbBus)]), 2),										//Drop Pattern 0 - 2 bars
		Pn(Ppar([lowtone.(1), hiWhine.(verbBus), elektroKick.(1)]), 2),						//Drop Pattern 1 - 2 bars
		Pn(Ppar([lowtone.(1), hiWhine.(verbBus), elektroKick.(1), hiWhine.(mainOut)]), 2),	//Drop Pattern 2 - 2 bars
	];

	//Song
	var song = Pseq([
								loTonePat[0], 				
		Ppar([	bassPat[0], 	loTonePat[0]																				]), 
		Ppar([	bassPat[0], 	loTonePat[0],					hatPat[0]													]), 
		drop[0], 
		Ppar([	bassPat[0], 	loTonePat[0], 								kickPat[0]										]), 
		Ppar([	bassPat[0], 	loTonePat[0],					hatPat[0], 	kickPat[0]										]),
		drop[1],
		Ppar([	bassPat[1], 									hatPat[2], 	kickPat[0]										]), 
		Ppar([	bassPat[2], 	loTonePat[0], 					hatPat[2], 	kickPat[0]										]),
		drop[2],
		Ppar([	bassPat[1], 									hatPat[2], 	kickPat[0],		hiWhinePat[0]					]), 
		Ppar([	bassPat[2], 	loTonePat[0], 					hatPat[2], 	kickPat[0]										]),
		Ppar([	bassPat[1], 					loTonePat[2],	hatPat[2], 	kickPat[0],		hiWhinePat[0],	hiWhinePat[1]	]), 
		Ppar([	bassPat[2], 	loTonePat[0],	loTonePat[2],	hatPat[2], 	kickPat[0],						hiWhinePat[1]	]),
		drop[0],
								loTonePat[2]
	]);

	song.play;
)






(
var numChannels =  ~dirt.numChannels;

 ~dirt.addModule('binscramble', { |dirtEvent|
    dirtEvent.sendSynth('binscramble' ++ numChannels,
      [
        out: ~out,
        bscr: ~binscr
      ]
    )
  }, { ~binscr.notNil });


  SynthDef("binscramble" ++ numChannels, { |out, pan = 0, binscr = 0|
    var signal, chain, chain_r, lfo, lfo2, trig;
    signal = In.ar(out, numChannels) * 1.9;
    chain = FFT(LocalBuf(2048), signal[0]);
    chain_r = FFT(LocalBuf(2048), signal[1]);
    lfo = LFNoise2.kr(4).abs;
    lfo2 = LFNoise2.kr(2).abs;
    trig = Impulse.kr(32);
    chain = PV_BinScramble(chain, lfo, lfo2, trig);
    chain_r = PV_BinScramble(chain_r, lfo, lfo2, trig);
    ReplaceOut.ar(out, Pan2.ar((signal * (1.0 - binscr)) + (binscr * [IFFT(chain), IFFT(chain_r)]), pan));
  }).store;

  ~dirt.addModule('binfreeze', { |dirtEvent|
    dirtEvent.sendSynth('binfreeze' ++ numChannels,
      [
        out: ~out,
        binfrz: ~binfrz
      ]
    )
  }, { ~binfrz.notNil });

  SynthDef("binfreeze" ++ numChannels, { |out, pan = 0, binfrz = 0|
    var signal, chain, chain_r, thr;
    signal = In.ar(out, numChannels) * 1.9;
    chain = FFT(LocalBuf(2048), signal[0]);
    chain_r = FFT(LocalBuf(2048), signal[1]);
    thr = MouseY.kr;
    chain = PV_MagFreeze(chain, thr > 0.5);
    chain_r = PV_MagFreeze(chain_r, thr > 0.5);
    ReplaceOut.ar(out, Pan2.ar((signal * (1.0 - binfrz)) + (binfrz * [IFFT(chain), IFFT(chain_r)]), pan));
  }).store;

  ~dirt.addModule('binshift', { |dirtEvent|
    dirtEvent.sendSynth('binshift' ++ numChannels,
      [
        out: ~out,
        binshf: ~binshf
      ]
    )
  }, { ~binshf.notNil });


  SynthDef("binshift" ++ numChannels, { |out, pan = 0, binshf = 0|
    var signal, chain, chain_r, thr;
    signal = In.ar(out, numChannels) * 1.4;
    chain = FFT(LocalBuf(2048), signal[1]);
    chain_r = FFT(LocalBuf(2048), signal[0]);
    thr = MouseX.kr(0.25, 4, \exponential);
    chain =  PV_BinShift(chain, thr);
    chain_r =  PV_BinShift(chain_r, thr);
    ReplaceOut.ar(out, Pan2.ar((signal * (1.0 - binshf)) + (binshf * [IFFT(chain), IFFT(chain_r)]), pan));
  }).store;


  ~dirt.addModule('binsmear', { |dirtEvent|
    dirtEvent.sendSynth('binsmear' ++ numChannels,
      [
        out: ~out,
        binsmr: ~binsmr
      ]
    )
  }, { ~binsmr.notNil });

  SynthDef("binsmear" ++ numChannels, { |out, pan = 0, binsmr = 0|
    var in, chain, chain_r, bins;
    in = In.ar(out, numChannels) * 1.75;
    chain = FFT(LocalBuf(2048), in[1]);
    chain_r = FFT(LocalBuf(2048), in[0]);
    bins = MouseY.kr(0, 100);
    chain = PV_MagSmear(chain, bins);
    chain_r = PV_MagSmear(chain_r, bins);

    ReplaceOut.ar(out, Pan2.ar((in * (1.0 - binsmr)) + (binsmr * [IFFT(chain), IFFT(chain_r)]), pan));
  }).store;


  ~dirt.addModule('brick', { |dirtEvent|
    dirtEvent.sendSynth('brick' ++ numChannels,
      [
        out: ~out,
        brick: ~brick
      ]
    )
  }, { ~brick.notNil });

  SynthDef("brick" ++ numChannels, { |out, pan = 0, brick = 0|
    var in, chain, chain_r, wall;
    in = In.ar(out, numChannels) * 1.35;
    chain = FFT(LocalBuf(2048), in[1]);
    chain_r = FFT(LocalBuf(2048), in[0]);
    wall = LFNoise2.kr(0.0 + brick);
    chain = PV_BrickWall(chain, wall);
    chain_r = PV_BrickWall(chain_r, wall);

    ReplaceOut.ar(out, Pan2.ar([IFFT(chain), IFFT(chain_r)], pan));
  }).store;


  ~dirt.addModule('distortion', { |dirtEvent|
    dirtEvent.sendSynth('distortion' ++ numChannels,
      [
        out: ~out,
        dist: ~dist
      ]
    )
  }, { ~dist.notNil });

  SynthDef("distortion" ++ numChannels, { arg out, pan = 0, dist = 0;
    var in, mix;
    in = Pan2.ar(In.ar(out, numChannels), LFNoise2.kr(0.3,0.4));
    mix = ((in[0]*120).softclip)* 0.75;
    ReplaceOut.ar(out, (in * (1.0 - dist)) + (dist * mix));
  }).store;

  ~dirt.addModule('convolution', { |dirtEvent|
    dirtEvent.sendSynth('convolution' ++ numChannels,
      [
        out: ~out,
        conv: ~conv
      ]
    )
  }, { ~conv.notNil });

  SynthDef("convolution" ++ numChannels, { arg out, note, pan = 0, conv = 0;
    var in, karnel, mix;
    karnel = LFPulse.ar((note + 60).midicps, 0.0, 0.15, 0.2) + Impulse.ar((note + 60).midicps, 0.0, 0.4);
    in = In.ar(out, numChannels);
    mix = [Convolution.ar(in[0], karnel, 1024, 0.5), Convolution.ar(in[1], karnel, 1024, 0.5)];
    ReplaceOut.ar(out, Pan2.ar((in * (1.0 - conv)) + (conv * mix), pan));
  }).store;

  ~dirt.addModule('mod', { |dirtEvent|
    dirtEvent.sendSynth('mod' ++ ~dirt.numChannels,  [
      out: ~out,
      mod: ~mod
    ])
  }, { ~mod.notNil });


  SynthDef('mod'++ numChannels, { |out, pan = 0, mod|

    var trate, index, in, mix, signal;

    index = Amplitude.kr((mod.ceil).clip(0,1));

    in = In.ar(out, numChannels);

    trate = MouseY.kr(12,9320,1);

    mix = MonoGrain.ar(in, 0.75 / trate, trate);

    signal = Select.ar(index, [in, mix]);

    ReplaceOut.ar(out, Pan2.ar(signal, pan));

  }).store();


  ~dirt.addModule('convolution_n', { |dirtEvent|
    dirtEvent.sendSynth('convolution_n' ++ numChannels,
      [
        out: ~out,
        convn: ~convn
      ]
    )
  }, { ~convn.notNil });

  SynthDef("convolution_n" ++ numChannels, { arg out, note, pan = 0, convn = 0;
    var in, karnel, mix;
    karnel = ClipNoise.ar(0.2);
    in = In.ar(out, numChannels);
    mix = [Convolution.ar(in[0], karnel, 1024, 0.5), Convolution.ar(in[1], karnel, 1024, 0.5)];
    ReplaceOut.ar(out, Pan2.ar((in * (1.0 - convn)) + (convn * mix),pan));
  }).store;

  ~dirt.addModule('convolution_p', { |dirtEvent|
    dirtEvent.sendSynth('convolution_p' ++ numChannels,
      [
        out: ~out,
        convp: ~convp
      ]
    )
  }, { ~convp.notNil });

  SynthDef("convolution_p" ++ numChannels, { arg out, note = 0, pan = 0, convp = 0;
    var src, kernel, mix, env;
    kernel = Mix.new(LFPulse.ar(([0,2,5,10,24] + (note + 60)).midicps, 0, 0.1, 0.04));
    src = In.ar(out, numChannels);
    mix = RLPF.ar(Convolution.ar(src[0], kernel, 2048), 8500);
    ReplaceOut.ar(out, Pan2.ar((src * (1.0 - convp)) + (convp * mix),pan));
  }).store();

  ~dirt.addModule('decay-filter', { |dirtEvent|
    dirtEvent.sendSynth('decay-filter' ++ numChannels,
      [
        out: ~out,
        decay: ~decay
      ]
    )
  }, { ~decay.notNil });

  SynthDef("decay-filter" ++ numChannels, { |out = 0, decay = 0|
    var signal, mix, mix_2, trig;
    signal = In.ar(out, numChannels);
    trig = Decay.ar(Impulse.ar(decay, 2 / decay ));
    mix = signal[0] * trig;
    mix_2 = signal[1] * trig;
    ReplaceOut.ar(out, [mix, mix_2]);
  }).store;

  ~dirt.addModule('wah', { |dirtEvent|
    dirtEvent.sendSynth('wah' ++ numChannels,
      [
        out: ~out,
        wah: ~wah
      ]
    )
  }, { ~wah.notNil });

  SynthDef("wah" ++ numChannels, { arg out, pan = 0, wah = 0;
    var index, in, mix, signal;

    index = Amplitude.kr((wah.ceil).clip(0,1));

    in = In.ar(out, numChannels);

    mix = RLPF.ar(in, LFNoise2.kr(wah.linexp(0, 1.0, 0.8, 40), 40, 84).midicps, 0.2);

    signal = Select.ar(index, [in, mix]);

    ReplaceOut.ar(out, Pan2.ar(signal, pan));
  }).store;

  ~dirt.addModule('henon', { |dirtEvent|
    dirtEvent.sendSynth('henon' ++ numChannels,
      [
        out: ~out,
        henon: ~henon
      ]
    )
  }, { ~henon.notNil });

  SynthDef("henon" ++ numChannels, { arg out, pan = 0, henon = 0;
    var index, in, mix, rate;

    rate = henon;

    in = In.ar(out, numChannels);

    mix = in * HenonC.ar(SampleRate.ir/2, LFNoise2.ar(0.6,1.2),  LFNoise1.ar(0.4,0.9));

    ReplaceOut.ar(out, Pan2.ar((in * (1.0 - rate)) + (mix * rate), pan));
  }).store;

  ~dirt.addModule('flange', { |dirtEvent|
    dirtEvent.sendSynth('flange' ++ numChannels,
      [
        out: ~out,
        flangefq: ~flangefq,
        flangefb: ~flangefb,
      ]
    )
  }, { ~flangefq.notNil or: { ~flangefb.notNil } });

  SynthDef("flange" ++ numChannels, { | out, pan = 0, flangefq = 0.1, flangefb = 0.1 |
    var input, effect;
    input = In.ar(out, numChannels);
    input = input + LocalIn.ar(numChannels); //add some feedback
    effect = DelayN.ar(input, 0.02, SinOsc.kr(flangefq, 0, 0.005, 0.005));
    LocalOut.ar(flangefb * effect);
    ReplaceOut.ar(out, Pan2.ar(effect, pan));
  }).store;


  ~dirt.addModule('pipe', { |dirtEvent|
     dirtEvent.sendSynth('pipe' ++ numChannels,
       [
         out: ~out,
         pipe: ~pipe
       ]
     )
   }, { ~pipe.notNil && ~pipe != 0 });

   SynthDef("pipe" ++ numChannels, { | out, pipe, pan = 0 |
     var input;
     input = In.ar(out, numChannels) ;
     OffsetOut.ar(pipe, input);
     ReplaceOut.ar(out, Silent.ar(numChannels));
   },[\ir, \ir, \ir]).store;


  // ~dirt.orderModules(['sound', 'vowel', 'flange', 'shape',  'crush', 'coarse', 'mod', 'wah', 'brick', 'spectral-delay','decay-filter','binfreeze', 'binscramble', 'binshift', 'binsmear', 'conv', 'convn', 'convp', 'henon', 'distortion', 'hpf', 'lpf', 'bpf', 'envelope', 'tremolo', 'phaser', 'convolution', 'convolution_n', 'convolution_p']);

  ~dirt.orderModules(['sound', 'vowel', 'flange', 'shape',  'crush', 'coarse', 'mod', 'wah', 'brick', 'spectral-delay','decay-filter','binfreeze', 'binscramble', 'binshift', 'binsmear', 'conv', 'convn', 'convp', 'henon', 'distortion', 'hpf', 'lpf', 'bpf', 'envelope', 'tremolo', 'phaser', 'convolution', 'convolution_n', 'convolution_p', 'pipe']);


)