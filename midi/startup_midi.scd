MIDIClient.init;
MIDIClient.list;
~rytmOut = MIDIOut.newByName("Elektron Analog Rytm", "Elektron Analog Rytm MIDI 1");
~fs1rOut = MIDIOut.newByName("MIDISPORT 2x2 Anniv", "MIDISPORT 2x2 Anniv MIDI 1");
~nordOut = MIDIOut.newByName("MIDISPORT 2x2 Anniv", "MIDISPORT 2x2 Anniv MIDI 2");
~rytmOut.latency = 0.0;
~fs1rOut.latency = 0.0;
~nordOut.latency = 0.0;
s.options.numBuffers = 2048 * 256;
s.options.maxNodes = 2048 * 512;
s.options.memSize = 8192 * 128;
s.waitForBoot {
  ~dirt = SuperDirt(2, s);
  ~dirt.start();
  ~dirt.soundLibrary.addMIDI(\rytm, ~rytmOut);
  ~dirt.soundLibrary.addMIDI(\fs1r, ~fs1rOut);
  ~dirt.soundLibrary.addMIDI(\nord, ~nordOut);
}

