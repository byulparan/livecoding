// 
// 2024.08.06 byulparan@gmail.com
// 
// 
(
~extBus = Bus.audio(s, 2);
~buffer1 = Buffer.alloc(s, s.sampleRate);
~buffer2 = Buffer.alloc(s, s.sampleRate);
)


(
SynthDef(\vst_inst, {
  var sig = VSTPlugin.ar(nil, 2);
  Out.ar(0, sig);
  Out.ar(~extBus, sig);
}).add;

SynthDef(\record, {|dur=1,buffer=0|
  RecordBuf.ar(Mix(In.ar(~extBus, 2)), buffer);
  Line.kr(0,0,dur,doneAction: Done.freeSelf);
}).add;

SynthDef(\player, {|dur=1, rate=1, pan=0, buffer=0, gain=1, cutoff=10000|
  var sig = RLPF.ar(PlayBuf.ar(1, buffer, rate), cutoff);
  Line.kr(0,0,dur,doneAction: Done.freeSelf);
  Out.ar(0, Pan2.ar(sig*gain, pan));
}).add;
)


~piano = VSTPluginController(Synth(\vst_inst)).open("Pianoteq 8.vst");





Pdef.defaultQuant_(4);

(
p = Pbind(
  \type, \vst_midi,
  \vst, ~piano,
  \scale, Scale.phrygian,
)
)

(
Pbindf(p,
  \midicmd, \control, 
  \ctlNum, 64,
  \control, Pseq([0,127],inf),
  \dur, Pseq([1/16, 63/16],inf),
  \timingOffset, 0.1
).play(quant:4);
)


(
Pdef(\glit, 
  Pbind(
	\instrument, Pstep(Pseq([\record, \player],inf), Pseq([1/4, 6/8],inf)),
	\buffer, ~buffer1,
	\addAction, \addToTail,
	\pan, Pstep(Pwhite(-1.0, 1.0), 1/2),
	\dur, 1/2
  )
).play;


Pdef(\glit2, 
  Pbind(
	\instrument, Pstep(Pseq([\record, \player],inf), Pseq([1/2, 28/8],inf)),
	\buffer, ~buffer2,
	\addAction, \addToTail,
	\cutoff, 100 * pow(2, Pwhite(0, 5)),
	\rate, Pif(0.8.loop.coin, 1, 2),
	\gain, 0.7,
	\dur, 1/2
  )
).play;
)




(
Pdef(\lh,
  Pbindf(p,
	\octave, 4,
	\degree, Ptuple([-7, Pwhite(2,4), Pwhite(5, 9), Pwhite(10, 14)]),
	\amp, Pwhite(0.2,0.3),
	\dur, 1,
	\delta, 4,
  )
).play
)


(
Pdef(\rh,
  Pbindf(p,
	\octave, Pwhite(4,5),
	\degree, Pwhite(5,15),
	\amp, Pwhite(0.25,0.35),
	\dur, Pstep(Prand([1,Rest(1)],inf),1),
  )
).play
)






