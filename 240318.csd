<CsoundSynthesizer>
<CsOptions>
-odac -m4
</CsOptions>
<CsInstruments>

sr = 44100
ksmps = 256
nchnls = 2
0dbfs = 1.0
seed 0

gkTempo init 60
gkResolution init 48

#define METRO(N) # (i(gkResolution) / (1 / ($N))) #

gaRevL init 0
gaRevR init 0
gaDelL init 0
gaDelR init 0

gkRain init .6



schedule "reverb", 0, -1
schedule "delay", 0, -1
schedule "clock", 0, -1
schedule "rain", 0, -1




instr rain
  kenv lag gkRain, 8.0
  al, ar diskin "samples/rain1.ogg", 1, 0, 1
  outs al*kenv, ar*kenv
endin


instr clock
  ktrig metro gkTempo / 60 * gkResolution
  kcount init 0
  if ktrig == 1 then
    schedulek "schedule", .0, .1, kcount
    kcount += 1
  endif
endin


instr schedule
  icount = p4
  iscale[] fillarray 0, 2, 4, 6, 7, 9, 11
  if icount % $METRO(4) == 0 then
    schedule "sawPad", 0, 6, 48
    schedule "sawPad", 0, 6, 67
    schedule "sawPad", 0, 6, 71
    schedule "ambient", 0, 6, 48
    schedule "ambient", 0, 6, 55
    schedule "ambient", 0, 6, iscale[random(0, lenarray(iscale))] + 60
    schedule "ambient", 0, 6, iscale[random(0, lenarray(iscale))] + 72
  endif
  if icount % $METRO(1) == 0 && rnd(1.0) > .3 then
    schedule "bell", 0, 4, iscale[random(0, lenarray(iscale))] + 72
  endif
  if icount % $METRO(8) == 0 then
    schedule "pulse", 0, .1, 67
    schedule "pulse", 4, .1, 67
    imax = floor(random(3, 8))
    icnt = 0
    while icnt < imax do
      schedule "pulse", 7 + ((1/imax)*icnt), .1, iscale[random(0, lenarray(iscale))] + 72
      icnt = icnt + 1
    od
  endif
endin


instr pulse
  kenv line .1, p3, .0
  asig vco2 kenv, cpsmidinn(p4), 2, .5
  outs asig, asig
  gaRevL += asig*.3
  gaRevR += asig*.3
  gaDelL += asig*.3
  gaDelR += asig*.3
endin


instr bell
  kenv line .036, p3, .0
  ifreq = cpsmidinn(p4)
  a1 foscil kenv, ifreq, 1, 1, .7
  a2 foscil kenv, ifreq, 1, 7, .7
  a3 foscil kenv, ifreq, 1, 14, .7
  asig = (a1 + a2 + a3) * .7
  outs asig, asig
  gaRevL += asig*.3
  gaRevR += asig*.3
  gaDelL += asig*.3
  gaDelR += asig*.3
endin


instr sawPad
  kenv linseg .0, p3*.2, .02, p3*.6, .02, p3*.2, .0
  ifreq = cpsmidinn(p4)
  al vco2 kenv*2, ifreq
  ar vco2 kenv*2, ifreq + 2
  outs butterlp(al, 1000), butterlp(ar, 1000)
endin


instr ambient
  kenv linseg .0, p3*.2, .02, p3*.6, .02, p3*.2, .0
  ifreq = cpsmidinn(p4)
  a1 foscil kenv, ifreq, 1, 1, 1
  a2 foscil kenv, ifreq, 1, 2, .7
  a3 foscil kenv, ifreq, 1, 7, .4
  klfo randi .3, .4
  klfo = abs(klfo) + .3
  a4 foscil kenv*klfo, ifreq, 1, 14, .3
  amod oscil 1000, 4000
  a5 oscil kenv, ifreq + amod
  asig = (a1 + a2 + a3 + a4 + a5) * 1.4
  al, ar pan2 asig, oscil:k(.3, .4) + .5
  outs al, ar
  gaRevL += al*.3
  gaRevR += ar*.3
endin


instr reverb
  al, ar reverbsc gaRevL, gaRevR, .9, 12000
  outs al, ar
  gaRevL = 0
  gaRevR = 0
endin


instr delay
  al vdelay gaDelL, 60/gkTempo * (1000/2), 4000
  ar vdelay gaDelR, 60/gkTempo * (1000/3), 4000
  outs al, ar
  gaDelL = al * .3
  gaDelR = ar * .3
endin


</CsInstruments>
<CsScore>


</CsScore>
</CsoundSynthesizer>
