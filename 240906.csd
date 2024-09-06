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



;; ================================================================================
;; Global Setup 
;; ================================================================================

gkBPM init 120

;; Audio Bus for Reverb/Delay
gaRevL init 0
gaRevR init 0
gaDelL init 0
gaDelR init 0


;; Wavetable / Global Variables
giWave1 ftgen 1, 0, 8192, 10, 1, 0.7, 0.5, 0.2, 0.3, 0.2, 0.4

giMelodyCount init 0
giBeats[] fillarray 0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, 7.0, 9.0, 10.5, 12.0
giMelody init 0
giMelodies[] fillarray 12,  11,   7,   4,   7,   0,   2,   4,    0,    2,   4

giStringMelodyCount init 0
giStringBeats[] fillarray    0,  1,  2,  3,  10, 11, 12, 13, 14, 15, 16, 32, 36, 44, 45, 46, 47, 48, 52
giStringMelodies[] fillarray 7,  5,  4, 12,  11, 12, 14, 12, 11, 12, 16, 17,  5,  4,  5,  4,  5,  4, -3



;; Startup Instrument
schedule "reverb", 0, -1
schedule "delay", 0, -1
schedule "schedule", 0, 0.1, 0


;; ================================================================================
;; Scheduler
;; ================================================================================

instr schedule
  
  ibeat = p4
  ibeatDur = 60/i(gkBPM)

  ;; ================================================================================
  ;; main theme
  ;; ================================================================================
  
  if ibeat % 16 == giBeats[giMelodyCount % lenarray(giBeats)] then
    giMelody = giMelodies[giMelodyCount % lenarray(giBeats)]
    giMelodyCount += 1
  endif
  
  ipan = rnd(1) < 0.5 ? 0.5 : rnd(1)

  schedule "melody_synth", 0, 0.06, cpsmidinn(giMelody + 70), ipan
  schedule "melody_synth", 0, 0.06, cpsmidinn(58 + 7), ipan
  schedule "melody_synth", 0, 0.06, cpsmidinn(58 + (int(ibeat / 8) % 2 == 0 ? 5 : 4)), ipan
  schedule "melody_synth", 0, 0.06, cpsmidinn(58), ipan

  
  ;; ================================================================================
  ;; string melody
  ;; ================================================================================
  
  ;; if ibeat % 64 == giStringBeats[giStringMelodyCount] then
  ;;   istringDur = (giStringMelodyCount == (lenarray(giStringBeats) - 1) ? 60 : giStringBeats[giStringMelodyCount + 1]) - giStringBeats[giStringMelodyCount] 
  ;;   schedule "strings", 0, istringDur*ibeatDur, cpsmidinn(giStringMelodies[giStringMelodyCount] + 70)
  ;;   giStringMelodyCount = (giStringMelodyCount + 1) % lenarray(giStringBeats)
  ;; endif

  
  
  if ibeat % 8.0 == 0 then
    ;; ================================================================================
    ;; bass
    ;; ================================================================================

    ibassNote[] fillarray 0, -3, -7, -3  
    ibass = ibassNote[ int(ibeat / 16) % lenarray(ibassNote) ]
    schedule "bass", 0, 1.5, cpsmidinn(ibass + 46)
    schedule "bass", ibeatDur*3.0, 0.25, cpsmidinn(ibass + 46)
    schedule "bass", ibeatDur*4.0, 2, cpsmidinn(ibass + 46)

    ;; ================================================================================
    ;; fm-piano
    ;; ================================================================================

    ;; schedule "fm_piano", 0, 2, cpsmidinn(ibass+58)
    ;; schedule "fm_piano", 0, 2, cpsmidinn(58 + 7)
    ;; inotes[] fillarray 11, 14, 17, 19
    ;; inoteIndx = int(random(0, lenarray(inotes)))
    ;; idel = int(random(1,4))
    ;; schedule "fm_piano", ibeatDur * idel , 2, cpsmidinn(58 + inotes[inoteIndx])
    ;; inoteIndx = (inoteIndx + int(random(1, lenarray(inotes)-1))) % lenarray(inotes)
    ;; idel += int(random(0,4))
    ;; schedule "fm_piano", ibeatDur * idel, 2, cpsmidinn(58 + inotes[inoteIndx])
    ;; inoteIndx = (inoteIndx + int(random(1, lenarray(inotes)-1))) % lenarray(inotes)
    ;; idel += int(random(0,4))
    ;; schedule "fm_piano", ibeatDur * idel, 2, cpsmidinn(58 + inotes[inoteIndx])
    
  endif


  ;; ================================================================================
  ;; pad
  ;; ================================================================================
  
  if ibeat % 8 == 0 then
    schedule "pad", .0, 8, cpsmidinn(58)
    inotes[] fillarray 4, 5, 7, 11
    schedule "pad", .0, 8, cpsmidinn(inotes[int(random(0, lenarray(inotes)))] + 58)
    inotes[] fillarray 5, 7, 11, 16, 17, 19
    schedule "pad", .0, 8, cpsmidinn(inotes[int(random(0, lenarray(inotes)))] + 70)
  endif

  ;; ================================================================================
  ;; kick
  ;; ================================================================================
  
  if ibeat % 4 == 0 then
    schedule "kick", 0, 0.1
  endif
  
  ;; ================================================================================
  ;; sine noise
  ;; ================================================================================
  
  if ibeat % 16 == 0 then
    schedule "sine", ibeatDur*.75, 0.2, 9000
  endif

  ;; ================================================================================
  ;; recursive call schedule
  ;; ================================================================================

    schedule "schedule", ibeatDur*.25, 0.1, ibeat+.25
  
endin



;; ================================================================================
;; Instruments
;; ================================================================================

instr melody_synth
  aenv linseg .1, p3*.9, .1, p3*.1, .0
  ifreq = p4
  asig foscil aenv*0.8, ifreq, 1, 1, linseg:k(1, p3*.9,1, p3*.1, 0.2) * 1.3
  al, ar pan2 asig, p5
  outs al, ar
  
  gaRevL += al *.3
  gaRevR += ar *.3
  gaDelL += al *.3
  gaDelR += ar *.3
endin




instr kick
  kfreq expon 280, p3, 20
  asig oscil line:a(0.3, p3, 0), kfreq
  outs asig, asig
endin



instr sine
  aenv linseg .1, p3*.9,.1, p3*.1, 0
  asig oscil aenv*.4, p4
  outs asig, asig
  gaRevL += asig*.1
  gaRevR += asig*.1
endin



instr fm_piano
  kenv linseg .07, p3, .0
  kindx line 1, p3, 0
  al foscil kenv, p4, 1, 1, 1
  ar foscil kenv, p4+2, 1, 1, 1
  outs al, ar
  gaRevL += al*.2
  gaRevR += ar*.2
  gaDelL += al*.2
  gaDelR += ar*.2
endin



instr bass
  kfreq linseg p4+100, p3*.04, p4, p3*.8, p4
  asig foscil line:k(0.1, p3, 0)*1.2, kfreq, 1, 0.5, oscil:k(0.5, 1.3)+0.7
  outs asig, asig
endin



instr pad
  kenv linseg .0, p3*.2, 1, p3*.6, 1, p3*.2, .0
  al = reson(noise(0.007*kenv,0.0), p4, 0.8) * 0.007
  ar = reson(noise(0.007*kenv,0.0), p4+2, 0.8) * 0.007
  outs al,ar
  gaRevL += al*.3
  gaRevR += ar*.3
endin



instr strings
  aenv linseg .0, p3*.05, .1, p3*.75, .1, p3*.2, .0
  aenv *= .4
  al oscil aenv, p4, giWave1
  ar oscil aenv, p4+.1, giWave1
  icutoff = 2600
  outs moogladder(al, icutoff, 0.2), moogladder(ar, icutoff, 0.2)

  gaRevL += al*.4
  gaRevR += ar*.4
  gaDelL += al*.3
  gaDelR += ar*.3
endin



;; ================================================================================
;; Effect
;; ================================================================================

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
