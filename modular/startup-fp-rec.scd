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

  var playMG = {|params, mixCh, morphCh|
	es3[mixCh].set(\val, dictAt.value(params, \mix,  0));
	es3[morphCh].set(\val, dictAt.value(params, \morph,  0));
  };

  var synths = (
	// params, noteCh, decayCh, gateCh, panCh
	dp: { |p| playSynthA.(p, 0, 1, 7, 0); },
	at: { |p| playSynthA.(p, 2, 3, 5, 2); },
	st: { |p| playSynthA.(p, 4, 5, 0, 1) },
	bi: { |p| playSynthA.(p, 6, 7, 6, -1) },

	// params, gateCh, panCh
	bd: { |p| playSynthB.(p, 1, 1) },
	cp: { |p| playSynthB.(p, 2, 2) },
	oh: { |p| playSynthB.(p, 3, 3) },
	ch: { |p| playSynthB.(p, 4, 3) },
	mg: { |p| playMG.(p, 4, 5)}
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
