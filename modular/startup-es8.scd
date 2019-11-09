(
s.options.device = "SuperCollider";
s.options.sampleRate = 44100;
s.options.numBuffers = 32;
s.options.hardwareBufferSize = 16;
s.options.numOutputBusChannels = 16;
s.options.numInputBusChannels = 0;
s.waitForBoot{
  var dictAt = { |dict, key, default|
	if (dict.at(key) == nil) { default } { dict.at(key) };
  };
  var es8 = Array.fill(8, { |i|
	Synth(\cv, [\ch, i])
  });
  var zzzOut = (play: { |dirtEvent|
	var e, ch, out;
	e = dirtEvent.event;
	ch = dictAt.value(e, \ch, -1);
	out = es8.at(ch);
	if (out != nil, {
	  var val, time;
	  val= dictAt.value(e, \cv, 0);
	  time = dictAt.value(e, \slew, 0);
	  thisThread.clock.sched(~latency, {
		out.set(\val, val, \time, time);
	  })
	});
	true
  });

  SynthDef(\cv, {|ch=14, val=0, time=0|
	OffsetOut.ar(ch, Lag.ar(DC.ar(1) * val, time));
  }).store;

  ~dirt = SuperDirt(2, s);
  ~dirt.start();
  ~dirt.soundLibrary.addSynth(\z, zzzOut);
}
)