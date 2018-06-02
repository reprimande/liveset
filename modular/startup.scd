(
s.options.device = "SuperCollider";
s.options.sampleRate = 44100;
s.options.numBuffers = 32;
s.options.hardwareBufferSize = 16;
s.options.numOutputBusChannels = 16;
s.options.numInputBusChannels = 0;
s.waitForBoot{
  SynthDef(\cv, {|ch=14, val=0, time=0|
	OffsetOut.ar(ch, Lag.ar(DC.ar(1) * val, time));
  }).store;

  SynthDef(\function, { | ch=14, attack=0.001, decay=0.2, curve=(-6), mul=1, t_trig=1, offset=0 |
	var e, dc;
	e = EnvGen.ar(Env.perc(attack, decay, mul, curve), gate: t_trig);
	dc = DC.ar(1) * offset;
	OffsetOut.ar(ch, e + dc);
  }).store;

  SynthDef(\es5, {|ch=20, val=1, time=0.003, t_trig=0|
	var e;
	e = EnvGen.ar(Env([0, 1, 1, 0], [0, time, 0], curve: \exp), gate: t_trig);
	OffsetOut.ar(ch, e * val);
  }).store;

  ~ch2val = { |ch| if (ch == 7) { -1 } {(1 << ch) / 127} };
  ~n2cv = { |n| (0.089 / 11) * n };
  ~dictAt = { |dict, key, default|
	if (dict.at(key) == nil) { default } { dict.at(key) };
  };

  ~es8 = Array.fill(8, { |i|
	if (i < 4) { Synth(\function, [\ch, i]); } { Synth(\cv, [\ch, i]); };
  });


  ~es3 = Array.fill(8, { |i|
	Synth(\cv, [\ch, 8 + i])
  });


  ~es5 = Array.fill(8, { |i|
	Synth(\es5, [\ch, 8 + 6, \val, ~ch2val.(i)])
  });
  // ~esx8gt = Array.fill(8, { |i|
  //   var v = [0.0000002, 0.0000003, 0.0000005, 0.000001, 0.000002, 0.0000039, 0.0000077, 0.0000153].at(i);
  //   Synth(\es5, [\ch, 8 + 7, \val, v]);
  // });

  ~playSynthA = {|params, noteCh, env1Ch, env2Ch|
	var note, slew,
	trig,  attack,  decay,  release,  offset,  gain, curve,
	trig2, attack2, decay2, release2, offset2, gain2, curve2;

	note    = ~dictAt.value(params, \note,    0);
	slew    = ~dictAt.value(params, \slew,    0);

	trig    = ~dictAt.value(params, \gate,    0);
	attack  = ~dictAt.value(params, \attack,  0);
	decay   = ~dictAt.value(params, \decay,   0);
	gain    = ~dictAt.value(params, \gain,    0);
	offset  = ~dictAt.value(params, \offset,  0);
	curve   = ~dictAt.value(params, \curve,  -6);

	trig2   = ~dictAt.value(params, \gate2,   0);
	attack2 = ~dictAt.value(params, \attack2, 0);
	decay2  = ~dictAt.value(params, \decay2,  0);
	gain2   = ~dictAt.value(params, \gain2,   0);
	offset2 = ~dictAt.value(params, \offset2, 0);
	curve2  = ~dictAt.value(params, \curve2, -6);

	~es3[noteCh].set(\val, ~n2cv.(note), \time, slew);
	~es8[env1Ch].set(
	  \t_trig, trig,
	  \attack, attack,
	  \decay, decay,
	  \mul, gain,
	  \offset, offset,
	  \curve, curve
	);
	~es8[env2Ch].set(
	  \t_trig, trig2,
	  \attack, attack2,
	  \decay, decay2,
	  \mul, gain2,
	  \offset, offset2,
	  \curve, curve2
	);
  };

  ~playSynthB = {|params, noteCh, decayCh, gateCh, cvOffset=1|
	var note, decay, slew, trig, acc;
	note  = ~dictAt.value(params, \note,  0);
	slew  = ~dictAt.value(params, \slew,  0);
	trig  = ~dictAt.value(params, \gate,  0);
	decay = ~dictAt.value(params, \decay, 0.2) * cvOffset;
	// acc = ~dictAt.value(params, \acc, 0);

	~es3[noteCh].set(\val, ~n2cv.(note), \time, slew);

	if (decay >= 0, {
	  ~es8[decayCh].set(\val, decay);
	});
	~es5[gateCh].set(\t_trig, trig, \time, 0.003);
  };

  ~playSynthC = {|params, ch|
	var gate, acc;
	gate = ~dictAt.value(params, \gate, 0);
	//acc = ~dictAt.value(params, \acc, 0);

	~es5[ch].set(\t_trig, gate);
  };

  ~sendCv = {|params, ch|
	~es8[ch].set(\val, ~dictAt.value(params, \mul,  0));
  };

  ~synths = (
	// params, noteCh, env1Ch, env2Ch
	dp: { |p| ~playSynthA.(p, 0, 0, 1); },
	ac: { |p| ~playSynthA.(p, 1, 2, 3); },

	// params, noteCh, decayCh, gateCh, cvOffset
	bj: { |p| ~playSynthB.(p, 2, 4, 2, 0.5) },
	at: { |p| ~playSynthB.(p, 4, 6, 4) },
	th: { |p| ~playSynthB.(p, 5, 5, 5) },
	ba: { |p| ~playSynthB.(p, 3, -1, 3) },

	// params, gatech

	oh: { |p| ~playSynthC.(p, 6) },
	ch: { |p| ~playSynthC.(p, 7) },
	bd: { |p| ~playSynthC.(p, 0) },
	cp: { |p| ~playSynthC.(p, 1) },

	ph: { |p| ~sendCv.(p, 7)}
  );

  OSCdef(\zzz, {|msg, time, addr, recvPort|
	var d = (\ch: -1, \gate: -1, \note: -1, \device: 'es', \cv: nil, \length: 0.001, \slew: 0.0, \synth: 'foo'),
	ch = -1, gate = -1, note = -1, device, cv, length, slew, synth;
	msg.postln;
	msg[3..].pairsDo({ |k, v| d.put(k, v); });

	device = d.at(\device);

	synth = ~synths.at(d.at(\synth));
	if (synth == nil, {
	  ch = d.at(\ch);
	  gate = d.at(\gate);
	  length = d.at(\length);
	  note = d.at(\note);
	  cv = d.at(\cv);
	  slew = d.at(\slew);

	  if (note >= 0,
		{ ~es3[ch - 1].set(\val, ~n2cv.(note), \time, slew); },
		{ ~es3[ch - 1].set(\val, 0, \time, slew); }
	  );
	  if (cv != nil, { ~es8[ch - 1].set(\val, cv, \time, slew);});
	  ~es5[ch - 1].set(\t_trig, 1);
	}, {
	  synth.(d);
	});
  }, "/zzz", nil, 12345);
};
)