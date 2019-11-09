s.options.numBuffers = 2048 * 256;
s.options.maxNodes = 2048 * 512;
s.options.memSize = 8192 * 128;
s.waitForBoot {
  SuperDirt.start
}
