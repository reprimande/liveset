// =====================================================================
// SuperCollider Workspace
// =====================================================================

s.boot


(
SynthDef(\main,{arg out=0, f=50,t_trig=0,p=0,i=0,d=10,l=0.1,a=0.5,n=1;
  var fmod=SinOsc.ar(f/60,0,f/60);
  var sig=[SinOsc.ar([f,f+600]+fmod,0,[d,0.005]).mean.tanh,HPF.ar(WhiteNoise.ar(1),8000),SinOsc.ar(30,0,1),VarSaw.ar(f/40,mul:d*10000)];
  var env=EnvGen.ar(Env([0.0,1.0,0.0],[0.0,l]),gate:t_trig);
  Out.ar(out,Pan2.ar(((Select.ar(i,sig)*env).fold(-1,1)*0.7+SinOsc.ar(40,0,0.3))*(1+HPF.ar(WhiteNoise.ar(0.02*n),8000)),p)*a);
}).add;
SynthDef(\hat,{arg out=0, f=50,t_trig=0,p=0,a=1;
  var sig=HPF.ar(WhiteNoise.ar(1),6000);
  var env=EnvGen.ar(Env([0.0,1.0,0.0],[0.0,0.01]),gate:t_trig);
  Out.ar(out,Pan2.ar((sig*env*a),p));
}).add;

y=Pmono(
  \main,
  \dur,Pxrand((1/8!8)++(1/4!8)++Pseq([1/16,1/16],Prand([1,2],1)),inf),
  \trig,1,
  \p,{rrand(-1,1)},
  \i,Pwrand([0,1,3,4],[0.946,0.03,0.02,0.004],inf),
  \d,Pwrand([1,30],[0.98,0.02],inf),
  \f,Pwrand([35,40,6000,20000],[0.30,0.65,0.03,0.02],inf),
  \l,{rrand(0.1,0.5)},
  \n,{[1,2,3,30].wchoose([0.8,0.1,0.05,0.05])});
z=Pmono(
  \hat,
  \dur,Pxrand((1/8!8)++(1/4!8)++Pseq([1/32,1/32,1/32,1/32],Prand([1,2],1)),inf),
  \trig,1,
  \p,{rrand(-1,1)},
  \a,0.1);

w=Ppar([y,z],inf).play;

)


(
SynthDef(\fx, {arg out = 0, in, trig = 0, lag = 0.1, grid = 1/2, bit = 7, sample = 5000;
    var sig, del;
    sig = In.ar(in,2)*0.5 + Select.ar(trig, [In.ar(in, 2)*0.5, LocalIn.ar(2), LPF.ar(Decimator.ar(In.ar(in,2)*0.5, sample, bit), 6000)]);
    del = DelayL.ar(sig, 1.0, Lag.kr(grid, lag));
 12   LocalOut.ar(HPF.ar(del*(trig.clip(0, 1)), 2000));
    Out.ar(out, Pan2.ar(sig[0], TRand.kr(-0.5, 0.5, trig)*trig));
}).add;
)

(
~rep = Bus.audio(s, 2);

Pmono(\fx, *[\trig : Pwrand([0, 1, 2], [0.4, 0.3, 0.3], inf),
    \grid: Prand([1/2, 1/4, 1/8, 1/16], inf),
    \bit: Prand([7, 10, 8, 24], inf),
    \sample: Pwhite(1000, 10100, inf),
    \lag: Pwhite(0, 0.1, inf),
    \in: ~rep,
    \dur: 1/8]).play(quant: 4);
)

s.quit;



// probably horrible for speakers... 

// glitchift
{l=LocalIn.ar(2);k=LFSaw.ar(l,0,l,1);j=k.range(0.25,4.0);s=PitchShift.ar(SinOscFB.ar(j**[l,k],k),[0.05,0.03],j);LocalOut.ar(s);s}.play;
// drone
{l=LocalIn.ar(2);k=LFSaw.ar(l,0,l,1);j=k.range(0.25,4.0);s=PitchShift.ar(SinOscFB.ar(j**[2,2.1],k),[0.05,0.03],j);LocalOut.ar(s);s}.play;


// few more I was mucking around with (remove tanh loop for < 140c)
{l = LocalIn.ar(2);k=LFSaw.ar(l,0,l,1);j=k.range(0.25,4.0);s=PitchShift.ar(SinOscFB.ar(j**l,k),[0.05,0.03],j);5.do{s=s.tanh+(s*0.1)};LocalOut.ar(s);s}.play;

{l = LocalIn.ar(2);k=LFSaw.ar(l,0,l,1);j=k.range(0.25,4.0);s=PitchShift.ar(SinOscFB.ar(j**2,k),[0.05,0.03],j);5.do{s=s.tanh+(s*0.1)};LocalOut.ar(s);s}.play;

{l = LocalIn.ar(2);k=LFSaw.ar(l,0,l,1);j=k.range(0.25,4.0);s=PitchShift.ar(SinOscFB.ar(j*20,k),[0.05,0.03],j);5.do{s=s.tanh+(s*0.1)};LocalOut.ar(s);s}.play;

{l = LocalIn.ar(2);k=LFSaw.ar(l,0,l,1);j=k.range(0.25,4.0);s=PitchShift.ar(SinOscFB.ar(j**j,k),[0.05,0.03],j);5.do{s=s.tanh+(s*0.1)};LocalOut.ar(s);s}.play;

{l = LocalIn.ar(2);k=LFSaw.ar(l,0,l,1);j=k.range(0.25,4.0);s=PitchShift.ar(SinOscFB.ar(j**l.abs,k),[0.05,0.03],j);5.do{s=s.tanh+(s*0.1)};LocalOut.ar(s);s}.play;



//mouse controls lfo volume in the x axis and band pass filter cut off in y, bit of randomness on the cutoffs and mucho glitchiness results
(
{BPF.ar(WhiteNoise.ar(mul: SinOsc.ar(MouseX.kr(0,8),phase:Rand (0,pi), mul:Rand( 0.1,1) )),MouseY.kr(0,20000) * Rand(0,400)  ) + BPF.ar(WhiteNoise.ar(mul: SinOsc.ar(MouseX.kr(0,8),phase:Rand (0,pi), mul:Rand( 0.1,1) )),MouseY.kr(0,20000) * Rand(0,400) )  + BPF.ar(WhiteNoise.ar(mul: SinOsc.ar(MouseX.kr(0,8),phase:Rand (0,pi), mul:Rand( 0.1,1) )),MouseY.kr(0,20000) * Rand(0,400)  )     }.scope;

)

{ SinOscFB.ar([400,301], MouseX.kr(0,4),0.3); }.scope;

s.boot

(
// s.record;
a = {
  l=LocalIn.ar(2);
  o=SinOscFB.ar([
	LFNoise2.kr(0.00001).range(0.00001,0.1)**l[1],
	LFNoise2.kr(0.00002).range(0.00001,0.1)**l[0]
  ], LFNoise0.kr(
	LFNoise2.kr(
	  LFNoise2.kr(0.00003).range(0.00001,10)
	).range(0.001,5)**l
  ).range(0.0125,4));
  c = FFT(LocalBuf(2048), o[0]);
  d = FFT(LocalBuf(2048), o[1]);
  c = PV_BrickWall(c, LFNoise0.kr(LFNoise2.kr(0.00002).range(0.000001,2)**l[1]));
  d = PV_BrickWall(d, LFNoise0.kr(LFNoise2.kr(0.000021).range(0.000001,2)**l[0]));
  o = [IFFT(c), IFFT(d)];
  o=(o**l).tanh;
  LocalOut.ar(o);
  o
}
)

z = a.play

z.free


s.boot

(
{
  l=LocalIn.ar(2);
  a=SinOscFB.ar(
	[LFNoise2.kr(0.00001).range(0.00001,0.1)**l[1],LFNoise2.kr(0.00002).range(0.00001,0.1)**l[0]],
	LFNoise0.kr(LFNoise2.kr(LFNoise2.kr(0.00003).range(0.00001,10)).range(0.001,5)**l).range(0.0125,4));
  b=[IFFT(PV_BrickWall(FFT(LocalBuf(2048),a[0]),LFNoise0.kr(LFNoise2.kr(0.00002).range(0.000001,2)**l[1]))),
	IFFT(PV_BrickWall(FFT(LocalBuf(2048),a[1]),LFNoise0.kr(LFNoise2.kr(0.000021).range(0.000001,2)**l[0])))];
  c=(b**l).tanh;
  LocalOut.ar(c);
  c
}.play
)


(
{
  l=LocalIn.ar(2);
  a=SinOscFB.ar(
    [LFNoise2.kr(0.00001).range(0.00001,0.1)**l[1],LFNoise2.kr(0.00002).range(0.00001,0.1)**l[0]],
    LFNoise0.kr(LFNoise2.kr(LFNoise2.kr(0.00003).range(0.00001,10)).range(0.001,5)**l).range(0.0125,4));
  b=([
    IFFT(PV_BrickWall(FFT(LocalBuf(2048),a[0]),LFNoise0.kr(LFNoise2.kr(0.00002).range(0.000001,2)**l[1]))),
    IFFT(PV_BrickWall(FFT(LocalBuf(2048),a[1]),LFNoise0.kr(LFNoise2.kr(0.000021).range(0.000001,2)**l[0])))
  ]**l).tanh;
  LocalOut.ar(b);
  b
}.play
)



(
// s.record;
{
  l=LocalIn.ar(2);
  o=PMOsc.ar([
	LFNoise2.kr(0.01).range(0.00001,0.1)**l[1],
	LFNoise2.kr(0.02).range(0.00001,0.1)**l[0]
  ], LFNoise0.kr(
	LFNoise2.kr(
	  LFNoise2.kr(0.3).range(0.0001,10)
	).range(0.001,5)**l
  ).range(0.0125,30),
  LFNoise0.kr(
	LFNoise2.kr(
	  LFNoise2.kr(0.3).range(0.0001,10)
	).range(0.001,5)**l
  ).range(0.0125,4));
  c = FFT(LocalBuf(2048), o[0]);
  d = FFT(LocalBuf(2048), o[1]);
  c = PV_BrickWall(c, LFNoise0.kr(LFNoise2.kr(0.2).range(0.000001,2)**l[1]));
  d = PV_BrickWall(c, LFNoise0.kr(LFNoise2.kr(0.21).range(0.000001,2)**l[0]));
  o = [IFFT(c), IFFT(d)];
  o=(o**l).tanh;
  LocalOut.ar(o);
  o
}.play
)




(
// s.record;
{
  l=LocalIn.ar(2);
  o=SinOsc.ar([0.00000001**l[1], 0.0000002**l[0]]) ** SinOsc.ar([0.000000001**l[0], 0.00000002**l[1]]);
  c = FFT(LocalBuf(1024), o[0]);
  d = FFT(LocalBuf(1024), o[1]);
  // c = PV_BrickWall(c, LFNoise0.kr(LFNoise2.kr(0.2).range(0.000001,2)**l[1]));
  // d = PV_BrickWall(c, LFNoise0.kr(LFNoise2.kr(0.21).range(0.000001,2)**l[0]));
  c = PV_RectComb(c, MouseX.kr(0, 32), MouseY.kr, 0.2);
  d = PV_RectComb(d, MouseX.kr(0, 32), MouseY.kr, 0.2);
  o = [IFFT(c), IFFT(d)];
  o=LeakDC.ar(o, 0.2);
  o=RLPF.ar(o, 80, 20)*1000;
  o=atan(o);
  LocalOut.ar(o);
  o
}.play
)



(
{
    var a;
    a = LFPulse.ar(800, 0.5, 0.5, 0.5);
    [a, LeakDC.ar(a, 0.1)]
}.scope(bufsize: 22050)
)


s.boot








(
  SynthDef(\ld0, { |out, sustain = 1, note = 0, speed = 1, begin=0, end=1, pan, accelerate, amp = 1, offset|
    var env, sound, dur;
    dur = 0.5 * (end - begin)/speed;
    sound = RLPF.ar( Mix.fill(7, {Pulse.ar((note + 60).midicps * (1.0 + Sweep.kr(1, accelerate)), 0.1, 0.04) + SinOsc.ar((note+[60, 72].choose).midicps * 2, LFSaw.ar((note+48).midicps,0,0.3,2), 0.03)}), LFNoise2.kr(0.4, 20 ,72).midicps, 0.2);
    env = EnvGen.ar(Env.perc(0.02, dur, amp * 0.5, -8), timeScale:sustain,
      doneAction:2);
    OffsetOut.ar(out,
      DirtPan.ar(sound, 2, pan, env)
    )
  }).store();

  SynthDef(\ld1, {|out, sustain = 1, note = 0, speed = 1, begin=0, end=1, pan, accelerate, amp = 1, offset|
    var sound, d_freq, dur, env;
    dur = 1.5 * (end - begin);
    d_freq = (note + 60).midicps * (1.0 + Sweep.kr(1, accelerate)) * speed;

    env = EnvGen.ar(Env.adsr(0.1, dur, 0.8, 2.3, 1, 2), timeScale:sustain, doneAction:2);

    sound=MoogFF.ar(
      SinOsc.ar(d_freq + LFNoise2.ar(0.2, 10),
        SinOsc.ar(d_freq * 3, SinOsc.ar(d_freq, 0, 6), 3) * EnvGen.kr(Env.perc(0.1, 0.35, amp , -4),  timeScale:sustain),
        SinOsc.ar(d_freq, pi / 2, amp)
      ),
      TExpRand.kr(1800.0, 8300.0, Impulse.kr(1.5))
    );

    OffsetOut.ar(out,
      DirtPan.ar(sound, ~dirt.numChannels, pan, env)
    )
  }).store;


  SynthDef(\ld2, {|out, sustain = 1, note = 0, speed = 1, begin=0, end=1, pan, accelerate, amp = 1, offset|
    var sound, d_freq, dur, env;
    dur = 2.0 * (end - begin);
    d_freq = (note + 48).midicps * (1.0 + Sweep.kr(1, accelerate));
    env = EnvGen.ar(Env.adsr(0.0, dur * sustain, 0.2, 1.5 * sustain,  1.2 * amp, -4), timeScale:sustain, doneAction:2);
    sound = Resonz.ar(Dust.ar(12000), d_freq, 0.0025) * 50 * amp;
    OffsetOut.ar(out,
      DirtPan.ar(sound, ~dirt.numChannels, pan, env)
    )
  }).store;


  SynthDef(\ldb, {|out, sustain = 1, gate = 1, note = 0, speed = 1, begin=0, end=1, pan, accelerate, amp = 1, offset|
    var sound, d_freq, dur, env;
    dur = 1.0 * (end - begin)/speed;
    d_freq = (note + 60).midicps * (1.0 + Sweep.kr(1, accelerate));
    env = EnvGen.ar(Env.perc(0.025 * sustain, 2*dur,  1, -4), gate, timeScale:sustain*8, doneAction:2);
    sound = RLPF.ar(SinOsc.ar(SinOsc.kr(d_freq/pi, 0, d_freq*2, 0.5), 0, 0.3),LFNoise2.kr(5,23,60).midicps,0.3);
    OffsetOut.ar(out,
      DirtPan.ar(sound, ~dirt.numChannels, pan, env)
    )
  }).store;


  SynthDef(\moe1, {|out, sustain = 1, gate = 1, note = 0, speed = 1, begin=0, end=1, pan, accelerate, amp = 1, offset|
    var sound, d_freq, dur, env;
    dur = 1.0 * (end - begin)/speed;
    d_freq = ((note + 60).midicps * (1.0 + Sweep.kr(1, accelerate)) * 0.003 )+ 1.0;
    env = EnvGen.ar(Env.perc(0.0, 0.05*dur,  1, -6), gate, timeScale:sustain*8, doneAction:2);
    sound = Klank.ar(`[[60,18492,8354] * d_freq, nil, [0.6, 0.6,1]], ClipNoise.ar(0.04)) * amp;
    OffsetOut.ar(out,
      DirtPan.ar((sound*32).softclip*0.3, ~dirt.numChannels, pan, env)
    )
  }).store;


  s.sendMsg(\b_allocRead, 10, Platform.resourceDir ++ "/sounds/sui1.aif");

  SynthDef(\moe2, {|out, sustain = 1, gate = 1, note = 0, speed = 1, begin=0, trate = 5,end=1, pan, accelerate, amp = 1, offset|
    var sound, d_freq, dur,r_dur, env;
    dur = 1.0 * (end - begin) / speed;
    r_dur = SinOsc.ar(2,0,18,20).abs / trate;
    d_freq = ((note + 60).midicps * (1.0 + Sweep.kr(1, accelerate)) * 0.3 )+ 1.0;
    env = EnvGen.ar(Env.perc(0.01, 0.2 * dur, 1, -2), gate, timeScale:sustain*8, doneAction:2);
    sound = RLPF.ar((TGrains.ar(2, Impulse.ar(XLine.ar(222,trate,0.2)), 10, speed , MouseX.kr(0,BufDur.kr(10)), 0.02 * dur, LFNoise2.kr(4), 1)*20).softclip * 0.2 * amp, LFNoise2.ar(2.8,20,98).midicps,0.3);
    OffsetOut.ar(out,
      DirtPan.ar((sound*32).softclip*0.2, ~dirt.numChannels, pan, env)
    )
  }).store;


  SynthDef(\moe3, {|out, sustain = 1, gate = 1, note = 0, speed = 1, begin=0, end=1, pan, accelerate, amp = 1, offset|
    var sound, d_freq, dur, env, dom;
    dom=12800;
    dur = 1.0 * (end - begin)/speed;
    d_freq = ((note + 60).midicps * (1.0 + Sweep.kr(1, accelerate)) * 0.003 )+ 1.0;
    env = EnvGen.ar(Env.perc(0.02, 0.2 * dur,  1, -3), gate, timeScale:sustain, doneAction:2);
    sound = Pulse.ar(XLine.kr(dom,60,0.05),0.5,0.4) * Decay2.ar(Impulse.ar(16), 0.01, 0.14, 5) * amp;
    OffsetOut.ar(out,
      DirtPan.ar(sound.softclip, ~dirt.numChannels, pan, env)
    )
  }).store;


  SynthDef(\moe4, {|out, sustain = 1, gate = 1, note = 0, speed = 1, begin=0, end=1, pan, accelerate, amp = 1, offset|
    var sound, d_freq, dur, env, yy, ww;

    dur = 1.0 * (end - begin)/speed;
    d_freq = ((note + 60).midicps * (1.0 + Sweep.kr(1, accelerate)) * 0.003 ) + 1.0;
    env = EnvGen.ar(Env.perc(0.02 , 0.3*dur,  1, 1), gate, timeScale:sustain, doneAction:2);
    sound = ClipNoise.ar(0.5)*Decay2.ar(Impulse.ar(9), 0.01, 0.02, 5) * amp;

    sound = (sound*800)*gate/8;
    ww = (sound).softclip*0.2;
    ww = AllpassN.ar(ww, 0.1,{[ Rand(0.03, 0.1), Rand(0.03, 0.1)]}.dup(2), 1.0);
    //yy = (yy * -0.5) + (ww * 0.5);

    OffsetOut.ar(out,
      DirtPan.ar(ww, ~dirt.numChannels, pan, env);
    )
  }).store;


  SynthDef('mgut',{|out, sustain = 1, gate = 1, note = 0, speed = 1, begin=0, end=1, pan, accelerate, amp = 1, offset|

   var sound, d_freq, dur, env;
    dur = 1.5 * (end - begin);
    d_freq = (note + 60).midicps * (1.0 + Sweep.kr(1, accelerate)) * speed;

    env = EnvGen.ar(Env.adsr(0.1, dur, 0.2, 1, amp, -6), timeScale:sustain, doneAction:2);

    sound = Pluck.ar(WhiteNoise.ar(0.1),1, 68.midicps.reciprocal, d_freq.reciprocal, 12,
          coef: 0);
    OffsetOut.ar(out,
      DirtPan.ar(sound, ~dirt.numChannels, pan, env);
    )
  }).store();

  /*
  SynthDef(\mstring, {|out, sustain = 1, gate = 1, note = 0, speed = 1, begin=0, end=1, pan, accelerate, amp = 1, offset|
  var sound, d_freq, dur, env, sig;
  dur = 1.0 * (end - begin)/speed;
  env = EnvGen.ar(Env.perc(0.1 , 0.3 * dur,  1, 1), gate, timeScale:sustain, doneAction:2);
  sound = Mix.fill(32 ,{|i|
  Pluck.ar(WhiteNoise.ar(0.05),
  Impulse.kr(200),
  ((60 + note).midicps).reciprocal,
  {((([0, 2, 7, 10, 24].choose + 60 + note).midicps) * rrand(0.99, 1.01)).reciprocal},
  30,
  coef: {rrand(-0.02, 0.02)})
  * LFNoise2.kr(0.03, 1, 0.3).abs
  * 0.3;
  }) * amp;
  sig = AllpassN.ar(sound, 0.1,{[ Rand(0.03, 0.1), Rand(0.03, 0.1)]}.dup(2), 1.0);
  //yy = (yy * -0.5) + (ww * 0.5);
  OffsetOut.ar(out,
  DirtPan.ar(sig, ~dirt.numChannels, pan, env);
  )
  }).store;
  */

  // viznut grain synth

  /*
  viznut_sig = Signal.fill(512 * 1200, {|i|
  var t= i;
  var calc;
  calc= t*(((t>>12)|(t>>8))&(56&(t>>3)));
  (calc%256)/128.0-1.0;
  });
  viznut_buf = Buffer.alloc(s, 512 * 1200, bufnum: 99999);
  viznut_buf.loadCollection(viznut_sig);
  SynthDef(\mviznut, {|out, sustain = 1, gate = 1, note = 0, speed = 1, begin=0, end=1, pan, accelerate, amp = 1, offset|
  var sound, d_freq, dur, env, sig, tdur, trate, rate;
  trate = MouseY.kr(0.7,200,1);
  tdur = 4 / trate;
  rate = Dseq([1, 0.5], inf);
  dur = 1.0 * (end - begin)/speed;
  env = EnvGen.ar(Env.perc(0.0 , dur,  1, 4), gate, timeScale:sustain, doneAction:2);
  sound = TGrains.ar(1, Impulse.ar(trate), 99999, rate * speed, MouseX.kr(0,BufDur.kr(99999)), tdur, 0, 0.1, 1) * 0.5 * amp;
  OffsetOut.ar(out,
  DirtPan.ar(sound, ~dirt.numChannels, pan, env);
  )
  }).store;
  */

  SynthDef(\pmosc, {|out, sustain = 1, gate = 1, note = 0, speed = 1, begin=0, end=1, pan, accelerate, amp = 1, offset|
    var sound, d_freq, dur, env;
    dur = 1.0 * (end - begin)/speed;

    sound = Mix.new(
      PMOsc.ar(
        ((([0, 5, 12, 24, 36] + (note + 60)).midicps)) * (1.0 + Sweep.kr(1, accelerate)) * speed,
        ExpRand(3, 50),
        EnvGen.ar(Env.adsr(Rand(0.1, 0.25) * dur, ExpRand(0.01, 0.5), 0.03, 1.0, ExpRand(3, 10), 1)),
        1,
        0.1));
    env = EnvGen.ar(Env.perc(0.5, 2.2 * dur,  1 * amp, -9), gate, timeScale:sustain, doneAction:2);
    OffsetOut.ar(out,
      DirtPan.ar(sound, ~dirt.numChannels, pan, env)
    )
  }).store;
)