(
MIDIClient.init;
MIDIClient.list;
~midiOut = MIDIOut.newByName("UX16", "UX16 MIDI 1");
~midiOut.latency = 0.0;

~digitoneOut = MIDIOut.newByName("Elektron Digitone", "Elektron Digitone MIDI 1");
~digitoneOut.latency = 0.0;

//Server.supernova;

s.options.device = "SuperCollider";
s.options.sampleRate = 44100;
s.options.numBuffers = 32;
s.options.hardwareBufferSize = 16;
s.options.numOutputBusChannels = 16;
s.options.numInputBusChannels = 0;
s.options.numBuffers = 1024 * 256; // increase this if you need to load more samples
s.options.memSize = 8192 * 32; // increase this if you get "alloc failed" messages
s.options.maxNodes = 1024 * 32; // increase this if you are getting drop outs and the message "too many nodes"
s.waitForBoot{
  var ch2val = { |ch| if (ch == 7) { -1 } {(1 << ch) / 127} };
  var n2cv = { |n| (0.089 / 11) * n };
  var dictAt = { |dict, key, default| if (dict.at(key) == nil) { default } { dict.at(key) }; };
  var es8;
  var es3;
  var es5;
  var playDPO;
  var playSynthA;
  var playSynthB;
  var playGate;
  var playMG;
  var playES8;
  var playES3;
  var synths;
  var zzzOut;

  SynthDef(\function, { | ch=14, attack=0.001, decay=0.2, curve=(-6), mul=1, t_trig=1, offset=0 |
	var e, dc;
	e = EnvGen.ar(Env.perc(attack, decay, mul, curve), gate: t_trig);
	dc = DC.ar(1) * offset;
	OffsetOut.ar(ch, e + dc);
  }).store;

  SynthDef(\cv, {|ch=14, val=0, time=0|
	OffsetOut.ar(ch, Lag.ar(DC.ar(1) * val, time));
  }).store;

  SynthDef(\es5, {|ch=20, val=1, time=0.001, t_trig=0|
	var e;
	e = EnvGen.ar(Env([0, 1, 1, 0], [0, time, 0], curve: \exp), gate: t_trig);
	OffsetOut.ar(ch, e * val);
  }).store;

  es8 = Array.fill(8, { |i|
	Synth(\cv, [\ch, i]);
  });

  es3 = Array.fill(8, { |i|
	Synth(\cv, [\ch, 8 + i])
  });

  es5 = Array.fill(8, { |i|
	Synth(\es5, [\ch, 8 + 6, \val, ch2val.(i)])
  });

  playDPO = {|params, cvOffset=1|
	var note, note2, idx, shp, agl, decay, slew, slew2, slewi, slews, slewa, trig, acc, pan;
	note  = dictAt.value(params, \note,  0);
	slew  = dictAt.value(params, \slew,  0);
	trig  = dictAt.value(params, \gate,  1);

	es8[0].set(\val, n2cv.(note), \time, slew);
	es5[0].set(\t_trig, trig, \time, 0.003);
  };

  playSynthA = {|params, noteCh, decayCh, gateCh, panCh, cvOffset=1|
	var note, note2, decay, slew, slew2,trig, acc, pan;
	note  = dictAt.value(params, \note,  0);
	slew  = dictAt.value(params, \slew,  0);
	slew2  = dictAt.value(params, \slew2,  0);
	trig  = dictAt.value(params, \gate,  1);
	decay = dictAt.value(params, \sustain, 0.2) * cvOffset;
	pan = dictAt.value(params, \pan, 0.5) * cvOffset;

	es8[noteCh].set(\val, n2cv.(note), \time, slew);
	es8[decayCh].set(\val, decay);
	es5[gateCh].set(\t_trig, trig, \time, 0.003);
	// es3[panCh].set(\val, pan);
  };

  playSynthB = {|params, ch, panCh|
	var gate, acc, pan;
	gate = dictAt.value(params, \gate, 1);
	pan = dictAt.value(params, \pan, 0.5);
	es5[ch].set(\t_trig, gate);
	es3[panCh].set(\val, pan);
  };

  playGate = {|params, ch|
	var gate, acc, pan;
	gate = dictAt.value(params, \gate, 1);
	es5[ch].set(\t_trig, gate);
  };

  playMG = {|params, mixCh, morphCh|
	es3[mixCh].set(\val, dictAt.value(params, \mix,  0));
	es3[morphCh].set(\val, dictAt.value(params, \morph,  0));
  };

  playES8 = {|params, ch|
	var val, slew;
	val = dictAt.value(params, \val,  0);
	slew = dictAt.value(params, \slew,  0);
	es8[ch].set(\val, val, \time, slew);
  };

  playES3 = {|params, ch|
	var val, slew;
	val = dictAt.value(params, \val,  0);
	slew = dictAt.value(params, \slew,  0);
	es3[ch].set(\val, val, \time, slew);
  };

  synths = (
	dp: { |p| playDPO.(p); },
	// bj: { |p| playSynthA.(p, 3, 4, 5, 1, 1) },
	bd: { |p| playSynthA.(p, 6, 7, 2, 2) },

	pitch2: { |p| playES8.(p, 1); },
	decay1: { |p| playES8.(p, 2); },
	decay2: { |p| playES8.(p, 3); },
	idx:    { |p| playES8.(p, 4); },
	pitch3: { |p| playES8.(p, 5); },
	pan:    { |p| playES3.(p, 0); },
	speed1: { |p| playES3.(p, 2); },
	speed2: { |p| playES3.(p, 3); },

	// params, gateCh, panCh
	b8: { |p| playGate.(p, 3) },
	sn: { |p| playGate.(p, 4) },
	cp: { |p| playGate.(p, 5) },
	oh: { |p| playGate.(p, 6) },
	ch: { |p| playGate.(p, 7) },

	mg: { |p| playMG.(p, 4, 5)},
  );

  zzzOut = (play: { |dirtEvent|
	var e = dirtEvent.event;
	var synth = synths.at(e.at(\synth));
	if (synth != nil, {
	  thisThread.clock.sched(~latency, {
		synth.(e);
	  })
	});
	true
  });

  ~dirt = SuperDirt(2, s);
  ~dirt.start();
  ~dirt.soundLibrary.addSynth(\modular, zzzOut);
  ~dirt.soundLibrary.addMIDI(\nord, ~midiOut);
  ~dirt.soundLibrary.addMIDI(\dgtn, ~digitoneOut)
};
)