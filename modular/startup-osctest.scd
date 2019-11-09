(
  OSCdef(\zzz, {|msg, time, addr, recvPort|
	msg.postln;
  }, "/zzz", nil, 12345);
)