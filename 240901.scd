// 
// 2024.09.01 byulparan@gmail.com
// 
// 

// Need Quarks Library
Quarks.install("wslib");



(
// ================================================================================
// 
// Setup 
// 
// ================================================================================

TempoClock.tempo_(110/60);


// ================================================================================
// Synth Definition
// ================================================================================

SynthDef(\vst_synth, {
  Out.ar(0, VSTPlugin.ar(nil, 2)*0.8);
}).add;


// ================================================================================
// Aalto Setup
// ================================================================================

~releaseControl = Bus.control;   // evn2_release
~reverbControl = Bus.control;    // reverb

fork {
  ~aalto = VSTPluginController(Synth(\vst_synth)).open("Aalto.vst");
  4.0.wait;
  ~aalto.editor;
  ~aalto.map(123, ~reverbControl);
  ~aalto.map(74, ~releaseControl);
  ~aalto.readProgram(Document.current.dir ++ "/samples/240901-aalto.fxp");
};


Ndef(\aaltoControl, {
  Out.kr(~reverbControl, \reverb.kr(0.0));
  Out.kr(~releaseControl, \release.kr(0.0));
});


// ================================================================================
// FM Noise
// ================================================================================

Ndef(\fmOsc, {
  var trig = \trig.tr(0);
  var dur = \dur.kr(0.2);
  var fr = \freq.kr(110);
  var freq = Env([fr, fr, 40], [0.0, \freqDec.kr(0.08)], \exp).kr(0, trig);
  var env = Env([0.2, 0.2, 0.0], dur*[0.0, 1.0]).kr(0, trig);
  var modOsc = SinOsc.ar(\modFreq.kr(500), 0, \modIndx.kr(0.2));
  var sig = Pan2.ar(Mix(PMOsc.ar(freq, freq*\ratio.kr(1), 4.0, modOsc, env)), \pan.kr(0.0));
  Fold.ar(sig * LFNoise1.kr(1.3).range(1, 10), -0.3, 0.3) * 0.2;
})


)





(
// ================================================================================
// 
// Performances
// 
// ================================================================================


// ================================================================================
// Sequencer Data 
// ================================================================================

var scStep = Array.fill(64, {|i| if(i % 4 == 0, {1}, {0}) });
var aaltoStep = Array.fill(64, {|i| if(i % 4 == 0, {1}, {0}) });
var curSelect = 0;
var aaltoSeq, aaltoControlSeq, fmOscSeq, allStreamPlayer;


// ================================================================================
// GUI
// ================================================================================

var win = Window("Aalto in SuperCollider", Rect(0, 90, 900, 250)).front;

// Play/Stop Button ================================================================================
var transport = Button(win, Rect(10, 15, 100, 20))
.states_([["PLAY"], ["STOP"]])
.action_({
  if(transport.value == 1, {
	Ndef(\aaltoControl).play;
	Ndef(\fmOsc).play;
	allStreamPlayer = [aaltoSeq, aaltoControlSeq, fmOscSeq].collect({|seq| seq.play})
  }, {
	allStreamPlayer.do{|player|
	  player.stop;
	}
  })
});

// Led ================================================================================
var leds = {|i|
  Button(win, Rect(i * 40 + 135, 15, 20, 20))
  .states_([["", Color.black, if(i % 4 == 0, {Color.black} ,{ Color.gray}) ], ["", Color.red, Color.red]]);
} ! 16;

// StepNote ================================================================================
var scStepValue = {|i| 
  RoundButton(win, Rect(i * 40 + 135, 50, 20, 20))
  .states_([["", Color.black], ["", Color.new255(147, 112, 219), Color.new255(147, 112, 219)]])
  .action_({ scStep[16*curSelect + i] = scStepValue[i].value })
  .value_(scStep[i])
} ! 16;

var aaltoStepValue = {|i| 
  RoundButton(win, Rect(i * 40 + 135, 80, 20, 20))
  .states_([["", Color.black], ["",  Color.new255(130, 130, 255),  Color.new255(130, 130, 255)]])
  .action_({ aaltoStep[16*curSelect + i] = aaltoStepValue[i].value })
  .value_(aaltoStep[i])
} ! 16;


// Label  ================================================================================
var scLabel = StaticText(win, Rect(765, 45, 80, 30)).align_(\center).background_(Color.new255(147, 112, 219)).string_("SC_FM_Pat");
var aaltoLabel = StaticText(win, Rect(765, 75, 80, 30)).align_(\center).background_(Color.new255(130, 130, 255)).string_("Aalto_Pat");


// Sequencer Selector ================================================================================
var patternSelector = ListView(win, Rect(10, 40, 100, 80))
.items_(["1", "2", "3", "4"])
.action_({
  curSelect = patternSelector.selection[0];
  scStepValue.do{|but, idx|
	{ but.value = scStep[16 * curSelect + idx] }.defer;
  };
  aaltoStepValue.do{|but, idx|
	{ but.value = aaltoStep[16 * curSelect + idx] }.defer;
  };
});


// ================================================================================
// for Monitor(scope, meter etc)
// ================================================================================

var meterL = LevelIndicator(win, Rect(60,140,20,100)).drawsPeak_(true).warning_(0.9).critical_(1.0);
var meterR = LevelIndicator(win, Rect(90,140,20,100)).drawsPeak_(true).warning_(0.9).critical_(1.0);

var oscHandle = OSCFunc({|level|
  {
	var dbLow = -80;
	meterL.value = level[4].ampdb.linlin(dbLow, 0, 0, 1);
	meterL.peakLevel = level[3].ampdb.linlin(dbLow, 0, 0, 1);
	meterR.value = level[6].ampdb.linlin(dbLow, 0, 0, 1);
	meterR.peakLevel = level[5].ampdb.linlin(dbLow, 0, 0, 1);
  }.defer;
}, "/level");

var freqscope = FreqScopeView(win, Rect(135, 140, 200, 100)).freqMode_(0).active_(true);
var buffer = Buffer.alloc(s, 256, 2, bufnum: 10);
var scope = ScopeView(win, Rect(350, 140, 200, 100));

var monitorSynth = {
  ScopeOut2.ar(In.ar(0,2)*2, buffer.bufnum, 4096, buffer.numFrames);
  SendPeakRMS.kr(In.ar(0, 2), cmdName: "/level");
}.play(target: 1, addAction: \addAfter);


freqscope.waveColors_([Color.green]);
scope.bufnum = buffer.bufnum;
scope.server = s;
scope.waveColors_([Color.yellow, Color.yellow]);
scope.start;


// ================================================================================
// Sequence
// ================================================================================

aaltoSeq = Pbind(
  \type, \vst_midi,
  \vst, ~aalto,
  \amp, 0.5,
  \dur, p{
	inf.do{|i|
	  16.do{|j|
		{leds[j].value = if(i % 16 == j, {1}, {0})}.defer(s.latency);
	  };
	  if(aaltoStep[16 * curSelect + (i % 16)] == 1, { 0.25.yield  }, { Rest(0.25).yield });
	};
  }
);

aaltoControlSeq = Pbind(
  \type, \set,
  \id, Ndef(\aaltoControl).nodeID,
  \args, #[release, reverb],
  \release, Pif(0.5.loop.coin, 0.04, Pwhite(0.6, 0.8)),
  \reverb, Pif(0.8.loop.coin, 0.0, Pwhite(0.4, 0.6)),
  \dur, 0.25
);


fmOscSeq = Pbind(
  \type, \set,
  \id, Ndef(\fmOsc).nodeID,
  \args, #[trig, freq, freqDec, ratio, modFreq, modIndx, pan],
  \trig, 1,
  \freq, 110 * Pif(0.7.loop.coin, 1, Pwhite(2,4)),
  \freqDec, 0.08 * Pif(0.7.loop.coin, 1, Pwhite(2, 4)),
  \ratio, Pif(0.5.loop.coin, 1, 4),
  \modFreq, 500 * Pwhite(1, 10),
  \modIndx, 0.2 * Pwhite(0,2),
  \pan, Pif(0.7.loop.coin, 0.0, Pwhite(-1.0, 1.0)),
  \dur, 0.125 * pow(2, Pwhite(0, 4)),
  \delta, p{
	inf.do{|i|
	  if(scStep[16 * curSelect + (i % 16)] == 1, { 0.25.yield  }, { Rest(0.25).yield });
	};
  }
);


// ================================================================================
// handle for close the GUI
// ================================================================================

CmdPeriod.doOnce({win.close});

win.onClose_({ 
  freqscope.kill;
  buffer.free;
  monitorSynth.free;
  allStreamPlayer.do{|player|
	player.stop;
  }
});


)


