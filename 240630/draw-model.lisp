(in-package :sc-user)
(named-readtables:in-readtable :sc)


(gfx:defpipeline draw-scene ((mvp :mat4) (itime :float) (bones :mat4 200) (material :sampler-2d))
  (:vertex (:in ((pos :vec3) (coord :vec2) (norm :vec3)  (bone-ids :vec4 2) (bone-weights :vec4 2))
	    :out ((v-pos :vec3) (v-coord :vec2) (v-norm :vec3) ))
	   (let* ((bone-transform (m! 1.0 0.0 0.0 0.0
				      0.0 1.0 0.0 0.0
				      0.0 0.0 1.0 0.0
				      0.0 0.0 0.0 1.0)))
	     (setf bone-transform (* (aref bones (int (x (aref bone-ids 0)))) (x (aref bone-weights 0))))
	     (incf bone-transform (* (aref bones (int (y (aref bone-ids 0)))) (y (aref bone-weights 0))))
	     (incf bone-transform (* (aref bones (int (z (aref bone-ids 0)))) (z (aref bone-weights 0))))
	     (incf bone-transform (* (aref bones (int (w (aref bone-ids 0)))) (w (aref bone-weights 0))))
	     (incf bone-transform (* (aref bones (int (x (aref bone-ids 1)))) (x (aref bone-weights 1))))
	     (incf bone-transform (* (aref bones (int (y (aref bone-ids 1)))) (y (aref bone-weights 1))))
	     (incf bone-transform (* (aref bones (int (z (aref bone-ids 1)))) (z (aref bone-weights 1))))
	     (incf bone-transform (* (aref bones (int (w (aref bone-ids 1)))) (w (aref bone-weights 1))))
	     (setf v-pos  (xyz (* (sl:scale (vec3 .009)) bone-transform (v! pos 1.0)))
		   v-coord coord
		   v-norm (normalize (xyz  (* bone-transform (v! norm 0.0)))))
	     (* mvp (v! v-pos 1.0))))
  (:fragment (:in ((v-pos :vec3) (v-coord :vec2) (v-norm :vec3)))
	     (let* ((li (v! 0.0 10.0 10.0))
		    (l (normalize (- li v-pos)))
		    (dif (* 1.6 (max (dot v-norm l) 0.0)))
		    (amb (v! .1 .1 .1)))
	       (v! (+ amb (* 1.0 (s~ (sl:texture! material (+  v-coord (* .0 itime)) t)
				     xyz))) 1.0))))



(defclass scene-view (gfx:gl-canvas)
  ())

(defmethod gfx:draw ((view scene-view))
  (let* ((w (gfx:width view))
	 (h (gfx:height view)))
    (gl:viewport 0 0 w h)
    (gl:clear-color .0 .0 .0 1.0)
    (gl:enable :depth-test)
    (gl:clear :color-buffer-bit :depth-buffer-bit)
    (let* ((mvp (sb-cga:matrix* (gfx:projection-matrix view)
				(gfx:view-matrix view)))
	   (meshes (gfx:model-meshes *model*))
	   (time (gfx:get-internal-seconds)))
      (gfx:animation *model* *animation* (av:get-data :icontrol0))
      (loop for mesh in (gfx:model-meshes *model*)
	    for i from 0
	    when (gfx:mesh-bones mesh)
	      do
	       (gfx:with-shader (view 'draw-scene (gfx:mesh-gpu-stream mesh))
		 (let* ((bone-transforms (gfx:mesh-bone-transforms mesh)))
		   (gfx:set-uniform 'mvp mvp)
		   (gfx:set-uniform 'itime time)
		   (gfx:set-uniform 'bones bone-transforms)
		   (gfx:set-uniform 'material 0)
		   (%gl:draw-elements :triangles
				      (gfx:gpu-stream-length (gfx:mesh-gpu-stream mesh))
				      :unsigned-int 0)))))))



(gfx:define-shader scene
  (vec4 .0))

