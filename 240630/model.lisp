;; 
;; 
;; Now, cl-glsl works with classimp. but classimp not support recently version of assimp.
;; so I build assimp v5.2.5 and use it.
;;
;; Model from https://sketchfab.com/3d-models/iclone-7-raptoid-mascot-free-download-56a3e10a73924843949ae7a9800c97c7
;;


;; packages
(ql:quickload '(:cl-audiovisual))
(in-package :sc-user)
(named-readtables:in-readtable :sc)
(use-package '(:glsl :sc-extensions))


;; bootup scsynth
(av:run-cl-collider :monitor-rate 100.0)


;; graphics setup 
(defparameter *model* (gfx:load-model "./scene.gltf"))

(defvar *animation* 0)

(defun select-animation (index)
  (setf *animation* index)
  (av:set-data :icontrol1 index))

(load "draw-model.lisp")

(gfx:start-shader scene
  :textures (("./Machi_baseColor.jpeg")) 
  :gl-canvas scene-view)

(gfx:move-camera  :eye-x 3.4 :eye-y 1.2 :eye-z 2.5
		  :center-x .3 :center-y .5 :center-z -.5)





(define-code time-trig (time index)
  (let* ((timeline (mod (in.kr av:*icontrol0*)
			(ai:duration (aref (ai:animations (gfx:model-scene *model*)) index)))))
    (* (< time timeline)
       (> (+ time .2) timeline)
       (= index (in.kr av:*icontrol1*)))))


;; scene 0
(proxy :idle
  (pan2.ar
   (mix
    (+ (pm-osc.ar 100 700 1 .0 (env.kr .3 .6 .3 (time-trig 3.0 0) .1))
       (pm-osc.ar 2000 700 1 .0 (env.kr .3 .1 .3 (time-trig 4.7 0) .1))
       (pm-osc.ar 100 100 1 .0 (env.kr .3 1.1 .3 (time-trig 5.5 0) .1))
       (pm-osc.ar (r+ (lf-pulse.kr 6.) 200 400) 100 1 .0
		  (env.kr .3 1.1 .6 (time-trig 6.5 0) .1))
       (let* ((glow-trig (time-trig 8.6 0)))
	 (pm-osc.ar 100 (t-line.kr 50 300 1.0 glow-trig) 1 (sin-osc.ar 500 0 .4)
		    (env.kr .3 1.0 .2 glow-trig .1)))))))




;; scene 1
(proxy :grawling
  (let* ((trig (time-trig .1 1))
	 (glow-trig (time-trig 1. 1))
	 (freq [100 (ti-rand.kr 120 150 trig) (ti-rand.kr 200 600 trig)])
	 (ratio (r+ (sin-osc.kr .3) 1 4))
	 (indx (t-line.kr 1 4 1 glow-trig))
	 (mod (sin-osc.ar 3000 .0 (t-line.kr .1 1. .4 glow-trig) ))
	 (env (* (env.kr .0 1.2 .1 glow-trig  .1)
		 (asr.kr 1 1 1 (= (in.kr av:*icontrol1*) 1)))))
    (pan2.ar
     (mix
      (+ (pm-osc.ar (* freq (t-line.kr .5 1 .6 trig)) freq 1 .0 (env.kr .3 .6 .3 trig .1))
	 (pm-osc.ar freq (* freq ratio) indx mod env))))))


;; scene 2
(proxy :scared
  (pan2.ar
   (mix
    (+
     (let* ((trig (+ (time-trig .4 2) (time-trig 1.0 2))))
       (pm-osc.ar (ti-rand.kr 100 1000 trig) (t-line.kr 100 1000 1. trig) 1. (sin-osc.ar 200)
		  (env.kr .0 .2 .1 trig .1)))
     (let* ((trig (time-trig .8 2)))
       (pm-osc.ar (ti-rand.kr 600 1000 trig) (t-line.kr 1000 100 1.3 trig) 1. 0
		  (* (env.kr .8 1.3 .1 trig .04) (abs (lf-tri.kr 4.3)))))
     (let* ((trig (time-trig 4.0 2)))
       (pm-osc.ar (ti-rand.kr 500 1000 trig) (t-line.kr 100 1000 1.2 trig) 1.
		  (sin-osc.ar 5200 .0 (lf-pulse.kr 7.1))
		  (env.kr .0 1.2 .7 trig .1)))))))


;; scene 3
(proxy :running
  (let* ((trig (+ (time-trig .1 3)
		  (time-trig .5 3)))
	 (freq (t-line.kr 220 40 .1 trig :exp)))
    (pan2.ar (mix (pm-osc.ar freq (* freq (ti-rand.kr 1 4 trig))
			     (t-line.kr 1 (t-rand.kr 1.4 2.2 trig) .02 trig)  0
			     (env.kr .0 .6 .1 trig .1))))))





;;
;; Jam!
;;
(proxy :run-timestamp
  (out.kr av:*icontrol0* (var-lag.kr (cnt 1) (div 1))))



(select-animation 0)





