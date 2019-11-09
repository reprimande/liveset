MIDIClient.init;
MIDIClient.list;
~midiOut = MIDIOut.newByName("UX16", "UX16 MIDI 1");
~midiOut.latency = 0.0;

~midiOut2 = MIDIOut.newByName("Elektron Analog Rytm", "Elektron Analog Rytm MIDI 1");
~midiOut2.latency = 0.0;

s.options.device = "SuperCollider";
s.options.sampleRate = 44100;
s.options.numBuffers = 32;
s.options.hardwareBufferSize = 16;
s.options.numOutputBusChannels = 16;
s.options.numInputBusChannels = 0;
s.waitForBoot{
  var ch2val = { |ch| if (ch == 7) { -1 } {(1 << ch) / 127} };
  var n2cv = { |n| (0.089 / 11) * n };
  var dictAt = { |dict, key, default|
	if (dict.at(key) == nil) { default } { dict.at(key) };
  };

  var es8 = Array.fill(8, { |i|
	Synth(\cv, [\ch, i]);
  });

  var es3 = Array.fill(8, { |i|
	Synth(\cv, [\ch, 8 + i])
  });

  var es5 = Array.fill(8, { |i|
	Synth(\es5, [\ch, 8 + 6, \val, ch2val.(i)])
  });

  var playSynthA = {|params, noteCh, decayCh, gateCh, panCh, cvOffset=1|
	var note, decay, slew, trig, acc, pan;
	note  = dictAt.value(params, \note,  0);
	slew  = dictAt.value(params, \slew,  0);
	trig  = dictAt.value(params, \gate,  1);
	decay = dictAt.value(params, \sustain, 0.2) * cvOffset;
	pan = dictAt.value(params, \pan, 0.5) * cvOffset;

	es8[noteCh].set(\val, n2cv.(note), \time, slew);
	es8[decayCh].set(\val, decay);
	es5[gateCh].set(\t_trig, trig, \time, 0.003);
	es3[panCh].set(\val, pan);
  };

  var playSynthB = {|params, ch, panCh|
	var gate, acc, pan;
	gate = dictAt.value(params, \gate, 1);
	pan = dictAt.value(params, \pan, 0.5);
	es5[ch].set(\t_trig, gate);
	es3[panCh].set(\val, pan);
  };

  var playGate = {|params, ch|
	var gate, acc, pan;
	gate = dictAt.value(params, \gate, 1);
	es5[ch].set(\t_trig, gate);
  };

  var playMG = {|params, mixCh, morphCh, pitchCh, speedCh, inflCh|
	es8[mixCh].set(\val, dictAt.value(params, \mix,  0));
	es8[morphCh].set(\val, dictAt.value(params, \morph,  0));
	es8[pitchCh].set(\val, dictAt.value(params, \pitch,  0));
	es8[speedCh].set(\val, dictAt.value(params, \speed,  0));
	es8[inflCh].set(\val, dictAt.value(params, \infl,  0));
  };

  var synths = (
	mg: { |p| playMG.(p,0,1,2,3,4)},
  );

  var zzzOut = (play: { |dirtEvent|
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
  ~dirt.soundLibrary.addMIDI(\rytm, ~midiOut2);

  SynthDef(\function, { | ch=14, attack=0.001, decay=0.2, curve=(-6), mul=1, t_trig=1, offset=0 |
	var e, dc;
	e = EnvGen.ar(Env.perc(attack, decay, mul, curve), gate: t_trig);
	dc = DC.ar(1) * offset;
	OffsetOut.ar(ch, e + dc);
  }).add;

  SynthDef(\cv, {|ch=14, val=0, time=0|
	OffsetOut.ar(ch, Lag.ar(DC.ar(1) * val, time));
  }).add;

  SynthDef(\es5, {|ch=20, val=1, time=0.001, t_trig=0|
	var e;
	e = EnvGen.ar(Env([0, 1, 1, 0], [0, time, 0], curve: \exp), gate: t_trig);
	OffsetOut.ar(ch, e * val);
  }).add;
};
