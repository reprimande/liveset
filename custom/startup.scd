s.options.numBuffers = 1024 * 512;
s.options.maxNodes = 1024 * 1024;
s.options.memSize = 1024 * 1024;
(
s.waitForBoot {
  ~dirt = SuperDirt(2, s);
  ~dirt.loadSoundFiles("/home/reprimande/samples/*");
  ~dirt.start()
}
)
