<CsoundSynthesizer>
<CsOptions>
-odac -m4
</CsOptions>
<CsInstruments>

sr = 44100
ksmps = 10
nchnls = 2
0dbfs = 1.0
seed 0

gkTempo init (113 / 60) * 4

giSin ftgen 1, 0, 1024, 10, 1
giSample ftgen 2, 0, 0, 1, "samples/kick1.aif", 0, 4, 1


schedule "clock", 0, -1
schedule "reverb", 0, -1

instr clock
  ktrig metro gkTempo
  kcount init 0
  if ktrig == 1 then
    schedulek "schedule", .0, .1, kcount
    kcount += 1
  endif
endin

instr schedule
  icount = p4
  if icount % 1 == 0 then
    schedule "_init_sound", .0, .3
    schedule "_bass", .0, .3
  endif
  if icount % 1 == 0 then
    schedule "_hihat", .0, random(.03, .13)
  endif
  if icount % 4 == 0 then
    schedule "_init_sound", .0, (1/i(gkTempo)) * 6.
    schedule "_kick", .0, 1.0
  endif
endin

instr _init_sound
  ifreq = 110 * int(random(1,3))
  iratio = .5 * int(random(1,14))
  indx = 2.
  iattk = random(.0, 1.0) > .5 ? .001 : .9
  aenv linseg .0, p3*iattk, .2, p3*(1-iattk), .0
  amod oscil ifreq * iratio * indx, ifreq * iratio
  asig oscil aenv, ifreq + amod
  al, ar pan2 asig, .5 + random(-.4, .4)
  outs al, ar
  chnmix al*.3, "reverbL"
  chnmix ar*.3, "reverbR"
endin

instr _kick
  aindx line .0, p3, ftsr(giSample)*p3
  asig table aindx, giSample
  asig = asig*.7
  outs asig, asig
endin

instr _hihat
  asig noise line:a(.24, p3, .0), .0
  outs asig, asig
endin

instr _bass
  asig vco line:a(.2, p3, .0), 110, 1, 0, giSin
  asig = lowpass2(asig, 400 + 400*int(random(1,10)), 4)
  outs asig, asig
  chnmix asig*.2, "reverbL"
  chnmix asig*.2, "reverbR"
endin

instr reverb
  al, ar reverbsc chnget:a("reverbL"), chnget:a("reverbR"), .9, 12000
  outs al, ar
  chnclear "reverbL", "reverbR"
endin


</CsInstruments>
<CsScore>


</CsScore>
</CsoundSynthesizer>
