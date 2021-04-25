(
MIDIClient.init;
MIDIClient.list;
~midiOut = MIDIOut.newByName("UX16", "UX16 MIDI 1");
~midiOut.latency = 0.0;

s.options.device = "SuperCollider";
s.options.sampleRate = 44100;
s.waitForBoot{
  ~dirt = SuperDirt(2, s);
  ~dirt.start();
  ~dirt.soundLibrary.addMIDI(\nord, ~midiOut);
};
)