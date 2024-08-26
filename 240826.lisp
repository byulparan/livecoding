(ql:quickload '(:cl-collider :sc-extensions :arrow-macros))

(in-package :sc-user)
(named-readtables:in-readtable :sc)
(use-package '(:sc-extensions :arrow-macros))

(setf *s* (make-external-server "localhost" :port 57140))
(server-boot *s*)

(bpm 96.0)

(defvar *reverb* (bus-audio :chanls 2))
(defvar *delay* (bus-audio :chanls 2))

(proxy :reverb
  (g-verb.ar (in.ar *reverb* 2) 100 6.0))

(proxy :delay
  (comb-n.ar (in.ar *delay* 2) 2.0 [1/3 1/2] 4.0))


(proxy :fm-solar
  (let* ((freq (loop for i below 4 collect (exp-rand.ir 50 3000)))
	 (trig (dup (lambda (i) (rand-tr (tr 4) .2)) (length freq)))
	 (ratio (loop for tr in trig
		      collect (t-line.kr (ti-rand.kr 1 4 tr) (t+ tr [.1 .2 .5 .7  1.0])
					 (t-exp-rand.kr .01 .1 tr) tr :exp)))
	 (indx (t-exp-rand.kr 1 4 trig))
	 (mod (sin-osc.ar (* 1000 (ti-rand.kr 1 8 (tr .25))) 0 (t-exp-rand.kr .0 2.0 trig)))
	 (sig nil))
    (setf sig
      (-> (pm-osc.ar freq (* freq ratio) indx mod .1)
	(* (env.kr .0 (div (t+ (mix trig) [1 2 4 8])) .3 trig 1))
	(pan2.ar (coin.kr trig .7 .0 (t-rand.kr -.8 .8 trig)))
	(mix)))
    (out.ar (coin.kr (mix trig) .7 *reverb* *delay*) (* sig (trig.kr (rand-tr (mix trig) .2)) .1))
    sig))



