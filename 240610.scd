


(
~reverbBus = Bus.audio(s, 2);

{
  GVerb.ar(In.ar(~reverbBus,2), 100, 6);
}.play(s, target: 1, addAction: \addAfter);

Ndef(\fm).quant_(1).play;
)




(
Ndef(\fm)[0] = {
  var trig = \trig.tr(0);
  var env = Env([0.0, 0.25, 0.0],[0.0, 1.0]).kr(0, trig);
  var freq = Env([0.1, 200, 20], [0.0, 0.1], \exp).kr(0, trig);
  var mod = SinOsc.ar(\modFreq.kr(100));
  var sig = Pan2.ar(PMOsc.ar(freq, freq*\ratio.kr(1), \indx.kr(1), mod, env));
  Out.ar(~reverbBus, sig*\rev.kr(0));
  sig
}
)

(
Ndef(\fm)[1] = \set->Pbind(
  \ratio, 2**Pwhite(0,4),
  \indx, Pwhite(1,10),
  \modFreq, 2**Pwhite(1, 20),
  \rev, Pif(0.7.loop.coin, 0.0, Pwhite(0.1, 0.3).trunc(0.1)),
  \dur, Pif(0.7.loop.coin, Pwhite(1,2)*0.125, Rest(0.125))
)
)


(
SynthDef(\pwm_pad, {|freq = 100, dur=4.0, cutoff=1000|
  var env = Env([0.0, 0.1, 0.1, 0.0], dur*[0.3, 0.4, 0.3]).kr(2);
  var pw = SinOsc.ar(55.midicps, 0, 0.4, 0.5);
  var sig = RLPF.ar(LFPulse.ar(freq +[0,2], 0, pw, 2, -1) * env, cutoff);
  Out.ar(0, sig);
  Out.ar(~reverbBus, sig*0.1);
}).add;
)

(
Pbindef(\pad, 
  \instrument, \pwm_pad,
  \midinote, Ptuple([48,55,Pxrand([65,67,71,74],inf)],inf),
  \dur, 6.0,
  \delta, 4.0
).play;
)


