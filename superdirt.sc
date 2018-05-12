include("Vowel")



Platform.userExtensionDir

Platform.systemExtensionDir

Quarks.gui

s.quit

Platform.userExtensionDir

(
// configure the sound server: here you could add hardware specific options
// see http://doc.sccode.org/Classes/ServerOptions.html
s.options.numBuffers = 1024 * 256; // increase this if you need to load more samples
s.options.memSize = 8192 * 128; // increase this if you get "alloc failed" messages
s.options.maxNodes = 1024 * 512; // increase this if you are getting drop outs and the message "too many nodes"
s.options.numOutputBusChannels = 2; // set this to your hardware output channel size, if necessary
s.options.numInputBusChannels = 2; // set this to your hardware input channel size, if necessary
// boot the server and start SuperDirt
s.latency_(0.5);
s.waitForBoot {
    ~dirt = SuperDirt(2, s); // two output channels, increase if you want to pan across more channels
    ~dirt.loadSoundFiles;   // load samples (path containing a wildcard can be passed in)
    s.sync; // wait for samples to be read
    ~dirt.start(57120, [0, 0]);   // start listening on port 57120, create two orbits, each sending audio to channel 0. You can direct sounds to the orbits from tidal e.g. by: `# orbit "0 1 1"
}
)




s.options.numBuffers = 2048 * 256;
s.options.maxNodes = 2048 * 512;
s.options.memSize = 8192 * 128;

s.boot


s.quit



SuperDirt.start

SuperDirt.stop

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
    son = son * 0.1;

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
SynthDef("kick", {|out=0, freq=100,pan=0.5, sustain=0.5, a=0.00001, d=0.2|
  var env = EnvGen.ar(Env.perc(a, d, 1, -4), doneAction:2);
  o = PMOsc.ar(env * freq * 0.8, env * (freq * 0.9), env * 1.8) + SinOsc.ar((freq * 1.2) * env + 20, 0.5);
  OffsetOut.ar(out, DirtPan.ar(o * 0.5, 2, pan, env));
}).store
)


(
SynthDef("pmhh", {|out=0, freq=20000, pan=0.5, sustain=0.5, a=0.00001, d=0.05|
  var env = EnvGen.ar(Env.perc(a, d, 1, -4), doneAction:2), f = 20000;
  o = PMOsc.ar(f, f * 2.355555, 60 * env);
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







(4000.123 / 44100)

(SampleRate.ir * )


{HenonL.ar(MouseX.kr(10, SampleRate.ir), LFNoise2.kr(2,0.2,1.2), LFNoise2.kr(1, 0.15, 0.15)) }.play

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