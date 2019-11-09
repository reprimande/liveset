(
MIDIClient.init;
MIDIClient.list; // to check, which are there, you can query them
)


// create a midi out connection
~rytmOut = MIDIOut.newByName("Elektron Analog Rytm", "Elektron Analog Rytm MIDI 1"); // substitute your own device here

 // you may want to adjust the latency here
~rytmOut.latency = 0.0;



// define one instrument called "midi" (you may call it what you like)
// it uses the built in event type "midi" (this is predefined).


s.stop

(
s.options.numBuffers = 1024 * 256; // increase this if you need to load more samples
s.options.memSize = 8192 * 16; // increase this if you get "alloc failed" messages
s.options.maxNodes = 1024 * 32; // increase this if you are getting drop outs and the message "too many nodes"
s.options.numOutputBusChannels = 2; // set this to your hardware output channel size, if necessary
s.options.numInputBusChannels = 2; // set this to your hardware output channel size, if necessary
s.waitForBoot {
  ~dirt = SuperDirt(2, s);
  ~dirt.start()
}
)

~dirt.loadSoundFiles;






(
// configure the sound server: here you could add hardware specific options
// see http://doc.sccode.org/Classes/ServerOptions.html
s.options.numBuffers = 1024 * 256; // increase this if you need to load more samples
s.options.memSize = 8192 * 16; // increase this if you get "alloc failed" messages
s.options.maxNodes = 1024 * 32; // increase this if you are getting drop outs and the message "too many nodes"
s.options.numOutputBusChannels = 2; // set this to your hardware output channel size, if necessary
s.options.numInputBusChannels = 2; // set this to your hardware output channel size, if necessary
// boot the server and start SuperDirt
s.waitForBoot {
	~dirt = SuperDirt(2, s); // two output channels, increase if you want to pan across more channels
	~dirt.loadSoundFiles;   // load samples (path containing a wildcard can be passed in)
	// for example: ~dirt.loadSoundFiles("/Users/myUserName/Dirt/samples/*");
	// s.sync; // optionally: wait for samples to be read
	~dirt.start(57120, [0, 0]);   // start listening on port 57120, create two busses each sending audio to channel 0

	// optional, needed for the sclang tests only:
	(

		~d1 = ~dirt.orbits[0]; // one orbit
		~d2 = ~dirt.orbits[1];

	);
};

s.latency = 0.3; // increase this if you get "late" messages
);


~dirt.soundLibrary.addMIDI(\rytm, ~rytmOut);

~dirt.soundLibrary.addMIDI(\midi, ~midiOut);


s.dumpOSC(1)

s.stop

