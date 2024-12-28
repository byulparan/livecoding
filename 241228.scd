
// ================================================================================
// Ensure Server Boot
s.boot;




(
// ================================================================================
// Everything In Its Right Place - Radiohead 
// Cover in SuperCollider
// 

// ================================================================================
// Setup 
// ================================================================================


TempoClock.tempo_(123/60);

SynthDef(\kick, {
  var freq = XLine.kr(220, 20, 0.1);
  var sig = SinOsc.ar(freq, 0, Line.kr(0.3, 0, 0.2, doneAction: Done.freeSelf));
  Out.ar(0, Pan2.ar(sig));
}).add;

SynthDef(\keyboard, {|freq = 440, dur = 1.0, amp= 0.1|
  var env = EnvGen.kr(Env([0,1,1,0], dur*[0, 0.8, 0.2]), doneAction: Done.freeSelf);
  var mod = SinOsc.ar(freq, 0, 1.6);
  var sig = SinOsc.ar(freq + [0, 1], mod, env*amp);
  Out.ar(0, sig);
}).add;



b = Buffer.read(s, Platform.resourceDir +/+ "sounds/a11wlk01.wav");
~bus = Bus.audio(s, 2);

{
  CombN.ar(In.ar(~bus, 2), 2.0, TempoClock.tempo.reciprocal * [0.25, 1/3], 3.0);
}.play(target: 0, addAction: \addToTail);


Ndef(\voice)[0] = {
  var local = LocalBuf(256);
  var trig = \trig.tr(0);
  var startPos = (BufFrames.kr(b) / 16) * \pos.kr(0);
  var src = PlayBuf.ar(1, b, \rate.kr(1), startPos: startPos, trigger: trig);
  var env = Env([0,1,1,0], \dur.kr(1) * (TempoClock.tempo.reciprocal) * [0.0,0.8,0.2]).kr(0, trig);
  var chain = FFT(local, src * env);
  var sig;
  chain = PV_Freeze(chain, \freeze.kr(0));
  sig = Pan2.ar(IFFT(chain) * Env.asr(4,0.8,4).kr(0, \fade.kr(0)), \pan.kr(0.0));
  Out.ar(~bus, sig*\del.kr(0));
  sig;
};

Ndef(\voice).set(\fade, 0);

Ndef(\voice)[1] = \set->Pbind(
  \freeze, Pif(0.75.loop.coin, 0, 1),
  \pos, Pstep(Pif(0.85.loop.coin, Ptime() * 1.3 % 12, Pwhite(3, 8)), 0.5),
  \rate, Pstep(Prand([0.75, 1.0],inf), 0.5),
  \pan, Pif(0.7.loop.coin, 0.0, Pwhite(-1.0, 1.0)),
  \del, Pif(0.9.loop.coin, 0, 0.3),
  \dur, Pstep(Prand([0.5, 0.5, 0.25, 0.125], inf), 0.5)
);


// ================================================================================
// Performance
// ================================================================================


Routine{

  // ================================================================================
  // define function for play score
  var playScore = {|notes, beatsArray|
	beatsArray.do{|beats, indx|
	  beats.do{|beat|
		notes[indx].do{|note|
		  s.bind({ Synth(\keyboard, [\freq, note.midicps, \dur, beat*0.6]) });
		};
		beat.wait;
	  }
	};
  };
  
  var playArp = {|notes|
	notes.do{|note|
	  if(note.notNil, {
		s.bind({ Synth(\keyboard, [\freq, note.midicps, \dur, 0.5]) });
	  });
	  0.5.wait;
	}
  };

  
  // ================================================================================
  // play Music
  playArp.([60,56,55,48]);

  
  // ================================================================================
  // play kick and voice
  fork{
	Ndef(\voice).set(\fade, 0);
	20.wait;
	Ndef(\voice).play;
	Ndef(\voice).set(\fade, 1);

	inf.do{
	  s.bind({ Synth(\kick) });
	  1.0.wait;
	}
  };


  // ================================================================================
  // Part.1
  7.do{|i|
	var notes = [[36, 48, 52, 55, 60],[37, 49, 53, 55, 60], [39, 51, 55, 60]];
	var beats = [[1.5, 1, 1], [1, 1], [1, 1.5]];

	playScore.(notes, beats);
	playArp.(if(i % 2 == 0, { [60,58,60,48] }, { [60,56,55,48] }));
  };
  
  inf.do{
	// ================================================================================
	// Part.2
	4.do{|i|
	  var notes = [[41, 53, 57, 60], [36, 48, 52, 55, 60], [37, 49, 53, 55, 60],  [39, 51, 55, 60]];
	  var beats = [[1.5], [1, 1], [1, 1], [1, 1.5]];

	  playScore.(notes, beats);
	  playArp.(if(i != 3, { [60,58,60,48] }, { [60,56,55,48] }));
	}; 

	// ================================================================================
	// Part.1
	4.do{|i|
	  var notes = [[36, 48, 52, 55, 60],[37, 49, 53, 55, 60], [39, 51, 55, 60]];
	  var beats = [[1.5, 1, 1], [1, 1], [1, 1.5]];

	  playScore.(notes, beats);
	  playArp.(if(i != 3, { [60,56,55,48] }, { [60,58,60,nil]  } ));
	};

	// ================================================================================
	// Part.3
	1.do{
	  var notes = [[37, 49, 53, 56, 60], [36, 48, 52, 55, 60], [39, 51, 55, 58, 60], [39, 51, 55, 58, 61]];
	  
	  playScore.(notes, [[2, 1.5, 1],  [1, 1], [1], [1]]);

	  2.do{
		playScore.(notes, [[1.5, 2, 1.5, 1], [1, 1], [1], [1]]);	
	  };
	  
	  playScore.(notes, [[1.5, 2, 1.5, 1], [1, 1], [1, 1.5]]);	
	};
  }
  
}.play;
)







