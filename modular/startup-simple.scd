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
	Synth(\cv, [\ch, i])
  });


  ~es3 = Array.fill(8, { |i|
	Synth(\cv, [\ch, 8 + i])
  });

  ~cvout = ~es8 ++ ~es3;

  ~es5 = Array.fill(8, { |i|
	Synth(\es5, [\ch, 8 + 6, \val, ~ch2val.(i)])
  });


  OSCdef(\zzz, {|msg, time, addr, recvPort|
	var d = (\ch: -1, \cv: nil,\slew: 0.0),
	ch = -1, cv, slew;
	msg.postln;
	msg[1..].pairsDo({ |k, v| d.put(k, v); });
	d.postln;

	ch = d.at(\ch);
	cv = d.at(\cv);
	slew = d.at(\slew);
	if (cv != nil, { ~cvout[ch - 1].set(\val, cv, \time, slew);});
  }, "/zzz", nil, 12345);
};
)