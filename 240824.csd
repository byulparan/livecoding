<CsoundSynthesizer>
<CsOptions>
-odac -m4 --port=10000
</CsOptions>
<CsInstruments>

sr = 44100
ksmps = 32
nchnls = 2
0dbfs = 1.0
seed 0


gkBPM init 116

gaRevL init 0
gaRevR init 0
gaDelL init 0
gaDelR init 0


giKick ftgen 1, 0, 0, -1, "samples/kick1.aif", 0, 4, 1
giHihat ftgen 2, 0, 0, -1, "samples/hihat1.aif", 0, 4, 1
giWave1 ftgen 3, 0, 8192, 10, 1, 0.5, 0.3, 0.2, 0.3, 0.2, 0.4


schedule "reverb", 0, -1
schedule "delay", 0, -1
schedule "schedule", 1, 0.1, 0


instr schedule
  ibeat = p4
  ibeatDur = 60/i(gkBPM)

  ;; ================================================================================
  ;; Kick Drum
  ;; ================================================================================
  
  if ibeat % 1 == 0 then
    schedule "sample_player", .0, .2, giKick, .3
  endif

  ;; ================================================================================
  ;; Hihat Drum
  ;; ================================================================================
  
  if rnd(1) < .7 then
    schedule "sample_player", .0, .02 * pow(2, int(random(0, 4))), giHihat, .13
  endif

  ;; ================================================================================
  ;; Bass
  ;; ================================================================================
  
  schedule "bass", .0, .1, 70 * (rnd(1) < .8 ? 1 : int(random(2,5)))

  ;; ================================================================================
  ;; Strings
  ;; ================================================================================
  
  if ibeat % 4 == 0 then
    schedule "strings", .0, 4.0, cpsmidinn(61)
    schedule "strings", .0, 4.0, cpsmidinn(68)
    iNotes[] fillarray 80, 81, 83
    schedule "strings", .0, 4.0, cpsmidinn(iNotes[int(ibeat / 16) % lenarray(iNotes)])
  endif


  ;; ================================================================================
  ;; Synth
  ;; ================================================================================
  
  if ibeat % .5 == 0 then
    schedule "synth", .0, .5, 70
  endif
    
  if ibeat % 8 == 0 then
    schedule "synth", .0, .5, 190
    schedule "synth", .0, .5, 250
    schedule "synth", .0, .5, 670

    schedule "synth", ibeatDur * 1.75, .5, 190
    schedule "synth", ibeatDur * 1.75, .5, 250
    schedule "synth", ibeatDur * 1.75, .5, 400
  endif


  ;; ================================================================================
  ;; Recursive call to schedule
  ;; ================================================================================
  
  schedule "schedule", ibeatDur*.25, 0.1, ibeat+.25
  
endin


 
instr synth
  kenv linseg .13, p3* .9, .13, p3*.1, .0
  a1 vco2 kenv, p4
  a2 vco2 kenv, p4 * pow(2, 7/12)
  asig = (a1 + a2) * .6
  icutoff = 200
  asig moogladder2 asig, linseg:k(3400, p3*.2,icutoff, p3*.1, icutoff), 0.1
  outs asig, asig
  
  gaRevL += asig * .5
  gaRevR += asig * .5
endin



instr strings
  aenv linseg .0, p3*.2, .1, p3*.6, .1, p3*.2, .0
  aenv *= .2
  al oscil aenv, p4, giWave1
  ar oscil aenv, p4+.1, giWave1
  icutoff = 1600
  outs moogladder(al, icutoff, 0.2), moogladder(ar, icutoff, 0.2)

  gaRevL += al*.4
  gaRevR += ar*.4
  gaDelL += al*.3
  gaDelR += ar*.3
endin



instr bass
  kenv linseg .1, p3*.9, .1, p3*.1, .0
  asig vco2 kenv*.4, p4, 2, 0.5
  outs asig, asig
  gaDelL += asig*.2
  gaDelR += asig*.2

endin



instr sample_player
  iamp = p5
  aindx line .0, p3, ftsr(p4) * p3
  asig table aindx, p4
  asig *= linseg:a( iamp, p3*.9, iamp, p3*.1, .0 )
  outs asig, asig

endin


instr reverb
  al, ar reverbsc gaRevL, gaRevR, .9, 12000
  outs al, ar
  
  clear gaRevL, gaRevR
endin


instr delay
  al vdelay gaDelL, 60/gkBPM * (1000/2), 4000
  ar vdelay gaDelR, 60/gkBPM * (1000/3), 4000
  outs al, ar
  gaDelL = al * .3
  gaDelR = ar * .3
endin


</CsInstruments>
<CsScore>


</CsScore>
</CsoundSynthesizer>
