
;; ================================================================================
;; load system

;; Need Common Music package. You can get it
;; https://github.com/byulparan/CommonMusic2
;; or
;; https://github.com/ormf/cm

(ql:quickload '(:cl-audiovisual :cm))

;; ================================================================================
;; define packages
(in-package :sc-user)
(named-readtables:in-readtable :sc)
(use-package '(:sc-extensions :glsl))


;; ================================================================================
;; bootup scsynth server
(av:run-cl-collider)


(bpm 160.0) 

;; ================================================================================
;; load sample
(defvar *hihat* (buffer-read-channel "./samples/hihat.wav" :channels [0]))
(defvar *kick* (buffer-read-channel "./samples/kick.wav" :channels [0]))
(defvar *snare* (buffer-read-channel "./samples/snare.wav" :channels [0]))
(defvar *snare2* (buffer-read-channel "./samples/snare2.wav" :channels [0]))


;; reverb bus
(defvar *reverb-bus* (bus-audio :chanls 2))


;; ================================================================================
;; define synth and effect

(proxy :main-mix
  (replace-out.ar 0 (* .5 (in.ar 0 2)))
  :to 0
  :pos :tail)

(proxy :reverb
  (g-verb.ar (in.ar *reverb-bus* 2) 100 6.0)
  :to 1
  :pos :after)

(defsynth sample (bufnum (start 0) (end 1) (rate 1) (gain 1))
  (let* ((dur (* (buf-dur.ir bufnum) (abs (- end start))))
	 (sig (pan2.ar (buf-rd.ar 1 bufnum (* (line.ar start end (* dur (reciprocal rate)) :act :free)
					      (buf-frames.ir bufnum))))))
    (setf sig (* sig gain))
    (out.ar 0 sig)))

(defsynth fm ((freq 440) (ratio 1) (indx 0) (mod 0 :ar) (dur 1.0) (attk 0) (rel .01) (pan .0) (amp 1.0) (reverb .0))
  (let* ((env (env-gen.kr (env [0 .2 .2 0] (* dur [attk (- 1.0 (+ attk rel)) rel])) :act :free))
	 (sig (pan2.ar (pm-osc.ar freq (* freq ratio) indx mod (* amp env)) pan)))
    (out.ar 0 sig)
    (out.ar *reverb-bus* (* sig reverb))))



;; ================================================================================
;; Graphics


(gfx:clear-pipeline)

(gfx:defvar-g eps .001)
(gfx:defvar-g max-step 100)
(gfx:defvar-g max-depth 100.0)
(gfx:defvar-g objcol (vec3 .0))

(gfx:defun-g rot ((a :float))
  (let* ((s (sin a))
	 (c (cos a)))
    (m! c s (- s) c)))

(gfx:defun-g sdf ((pos :vec3))
  (let* ((d 40.0)
	 (s 2.0)
	 (p (vec3 .0)))
    (setf (xz pos) (* (xz pos) (rot (+ itime .5))))
    (setf (y pos) (- (y pos) 8.0))
    (setf (yz pos) (* (yz pos) (rot itime)))
    (setf p (- (abs pos) (* ivolume0 2.0)))
    (setf d (min d (- (length p) .5)))
    (setf d (min d (max (- (length (* 1.0 (xz p))) .1) (y p))))
    (setf d (min d (max (- (length (* 1.0 (yz p))) .1) (x p))))
    (setf d (min d (max (- (length (* 1.0 (xy p))) .1) (z p))))
    (setf p (abs pos))
    (setf (x p) (- (x p) 4.0))
    (setf d (min d (- (length p) .7)))
    (setf d (min d (max (- (length (yz p)) .1) (x p))))
    (setf d (min d (length (- (yz p)
			      (abs (* (sin (- (* (x p) .5)
					      (* itime 10.0)))
				      .8))))))
    (setf p (abs pos))
    (setf (y p) (- (y p) 4.0))
    (setf d (min d (- (length p) .7)))
    (setf d (min d (- (length (xz p)) .1)))
    (setf d (min d (length (- (xz p)
			      (abs (* (sin (- (* (y p) .5)
					      (* itime 10.0)))
				      .8))))))
    (setf objcol (v! 1.0 1.0 1.0))
    d))

(gfx:define-shader graphics
  (sl:with-raymarch (uv rd (ro camera) (ta lookat) uvn 2.0)
    (let* ((col (vec3 0.0))
	   (md 0.0))
      (dotimes (i 100)
	(let* ((p (+ ro (* rd md)))
	       (d (sdf p)))
	  (setf d (max .01 (abs d)))
	  (incf md d)
	  (when (> md max-depth) (break))
	  (incf col (* .01 objcol))))
      (+
       (* .4 (sl:texture! ichannel0 uvn))
       (v! (* 4.0 ivolume0 (pow col (vec3 2.0))) 1.0)))))




(gfx:start-shader graphics
  :textures ((:previous-frame)))

(gfx:move-camera :eye-x 0.0 :eye-y 4.0 :eye-z 15.0 :center-y 6.0)


;; ================================================================================
;; Sound

(schedule drumkit (1)
  (with-lambda (1/4)
    (when (cm:odds .5)
      (pp 'sample :bufnum *hihat* :gain .6))
    (case (beat-count 16)
      ((0 6 10) (pp 'sample :bufnum *kick*))
      ((4 12) (pp 'sample :bufnum *snare* :gain 1.3))
      (t (when (cm:odds .4)
	   (dotimes (i (if (cm:odds .9) 1 2))
	     (pp (* i 1/8) 'sample :bufnum *snare2* :gain .3 :end (trunc (rrand .5 1.0) .1) :rate (trunc (rrand .9 1.1) .1))))))))


(schedule bass (16)
  (with-lambda ((cm:next #.(cm:new cm:cycle :of [1.5 1.5 1])))
    (pp 'fm :freq (midicps (nth-beat 16 [41 40])) :ratio .5 :indx 1. :dur (* dur .8) :amp 1.3)))


(schedule pad (16)
  (with-lambda (16)
    (dolist (freq (midicps [55 60 64 (pc:quantize (rrand 67 76))]))
      (loop for detune in [0 2]
	    for pan in [-1 1]
	    do
	       (pp 'fm :freq (+ freq detune) :indx 1. :dur 22 :attk .3 :rel .3 :amp .6 :reverb .1
		 :pan pan)))))




