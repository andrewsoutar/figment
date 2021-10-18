(uiop:define-package #:com.andrewsoutar.figment/wayland
  (:use #:cl #:cffi #:com.andrewsoutar.cl-wayland-client)
  (:use #:com.andrewsoutar.figment/utils))
(cl:in-package #:com.andrewsoutar.figment/wayland)

(defclass recording-registry (wl-registry)
  ((globals :type list :accessor globals :initform ())))
(defmethod wl-registry-global ((self recording-registry) name interface version)
  (push (list name interface version) (globals self)))
(defmethod wl-registry-global-remove ((self recording-registry) name)
  (setf (globals self) (delete name (globals self) :key #'first)))

(defun registry-find-or-lose (registry interface)
  (or (find interface (globals registry) :key #'second :test #'equal)
      (error "Wayland: compositor does not provide required global \"~A\"" interface)))

(defun registry-bind-with-class (registry interface class &rest args &key min-version max-version)
  (remf args :min-version)
  (remf args :max-version)
  (destructuring-bind (name 2interface version) (registry-find-or-lose registry interface)
    (declare (ignore 2interface))
    (when min-version
      (unless (>= version min-version)
        (error "Wayland: interface ~A: version ~A required, but only ~A found" interface min-version version)))
    (wl-registry-bind registry name (apply #'make-instance class :version (min version max-version) args))))

(defclass invoking-callback (wl-callback)
  ((fun :type (function ((unsigned-byte 32)) *) :accessor fun :initarg :fun)))
(defmethod wl-callback-done ((self invoking-callback) data)
  (funcall (fun self) data))

(defmethod xdg-wm-base-ping :after (self serial)
  (xdg-wm-base-pong self serial))

(defun roundtrip (display)
  (let (done)
    (with-cleanups (((wl-display-sync display
                                      (make-instance 'invoking-callback :fun
                                                     (lambda (x) (declare (ignore x)) (setf done t))))
                     #'wayland-destroy))
      (do () (done)
        (wl-display-dispatch display)))))


(defclass main-window-wl-surface (wl-surface)
  ((pending-configures :type list :accessor pending-configures :initform ())
   (open-p :type boolean :accessor open-p :initform t)
   (front-buffer :type (cons foreign-poiner wl-buffer) :accessor front-buffer)
   (back-buffer :type (cons foreign-pointer wl-buffer) :accessor back-buffer)))

(defun close-main-window (main-window)
  (setf (open-p main-window) nil))

;; (defmethod wayland-destroy :before ((self main-window-wl-surface)))

(defclass main-window-xdg-surface (xdg-surface)
  ((surface :type main-window-wl-surface :reader surface :initarg :surface)
   (pending-configures :type list :accessor pending-configures :initform ())))
(defmethod xdg-surface-configure ((self main-window-xdg-surface) serial)
  (setf (pending-configures (surface self))
        (list* (lambda () (xdg-surface-ack-configure self serial))
               (append (shiftf (pending-configures self) ())
                       (rest (pending-configures (surface self)))))))

(defclass main-window-toplevel (xdg-toplevel)
  ((xdg-surface :type main-window-xdg-surface :reader xdg-surface :initarg :xdg-surface)))
(defmethod xdg-toplevel-configure ((self main-window-toplevel) width height states)
  (declare (ignore states))
  (push (list :resize :width width :height height) (pending-configures (xdg-surface self))))
(defmethod xdg-toplevel-close ((self main-window-toplevel))
  (close-main-window (surface (xdg-surface self))))

(defclass main-window-toplevel-decorations (zxdg-toplevel-decoration-v1)
  ((xdg-surface :type main-window-xdg-surface :reader xdg-surface :initarg :xdg-surface)))
(defmethod zxdg-toplevel-decoration-v1-configure ((self main-window-toplevel-decorations) mode)
  (declare (ignore mode))
  (push (list :decor (lambda (mode) (zxdg-toplevel-decoration-v1-set-mode self mode)))
        (pending-configures (xdg-surface self))))


(defun tmpfile (&optional size)
  (multiple-value-bind (fd pathname) (sb-posix:mkstemp "/dev/shm/tmp.XXXXXXXX")
    (unwind-protect (progn
                      (sb-posix:unlink pathname)
                      (when size
                        (sb-posix:ftruncate fd size))
                      (shiftf fd nil))
      (when fd (sb-posix:close fd)))))


(defun maybe-render (main-window)
  (let ((configures (shiftf (pending-configures main-window) nil)))
    (when configures
      ;; We need to re-render
      (destructuring-bind (done-callback &rest configures) configures
        (let (did-set-decor)
          (dolist (configure configures)
            (ecase (first configure)
              (:resize)
              (:decor (unless did-set-decor
                        (funcall (second configure) :server-side)
                        (setf did-set-decor t))))))
        (funcall done-callback))
      (destructuring-bind (mem-buf . wl-buf) (back-buffer main-window)
        (declare (type foreign-pointer mem-buf)
                 (type wl-buffer wl-buf))
        (loop for y from 0 below 400 do
          (loop for x from 0 below 600 do
            (setf (mem-aref mem-buf :uint32 (+ x (* y 600)))
                  (if (zerop (mod (+ (floor x 8) (floor y 8)) 2))
                      #xFF666666
                      #xFFEEEEEE))))
        (wl-surface-attach main-window wl-buf 0 0))
      (wl-surface-damage-buffer main-window 0 0 600 400)
      (wl-surface-commit main-window)
      (rotatef (front-buffer main-window) (back-buffer main-window)))))

(defun main (display-name)
  (with-cleanups ((display (wl-display-connect display-name) #'wl-display-disconnect)
                  (registry (wl-display-get-registry display (make-instance 'recording-registry))
                            #'wayland-destroy))
    (roundtrip display)
    (let* ((width 600)
           (height 400)
           (n-buffers 2)
           (depth 4)
           (stride (* width depth))
           (buffer-size (* height stride))
           (pool-size (* buffer-size n-buffers)))
      (with-cleanups
          ((compositor (registry-bind-with-class registry "wl_compositor" 'wl-compositor
                                                 :min-version 4 :max-version 4)
                       #'wayland-destroy)
           (main-window-surface
            (wl-compositor-create-surface compositor (make-instance 'main-window-wl-surface))
            #'wayland-destroy)

           (wm-base (registry-bind-with-class registry "xdg_wm_base" 'xdg-wm-base :max-version 3) #'wayland-destroy)
           (xdg-surface
            (xdg-wm-base-get-xdg-surface wm-base
                                         (make-instance 'main-window-xdg-surface :surface main-window-surface)
                                         main-window-surface)
            #'wayland-destroy)
           (toplevel
            (xdg-surface-get-toplevel xdg-surface (make-instance 'main-window-toplevel :xdg-surface xdg-surface))
            #'wayland-destroy)

           (wl-shm (registry-bind-with-class registry "wl_shm" 'wl-shm :max-version 1) #'wayland-destroy)
           (shm-fd (tmpfile pool-size) #'sb-posix:close)
           (shm-pool-mem (sb-posix:mmap nil pool-size (logior sb-posix:prot-read sb-posix:prot-write)
                                        sb-posix:map-shared shm-fd 0)
                         (lambda (buf) (sb-posix:munmap buf pool-size)))

           (shm-pool (wl-shm-create-pool wl-shm (make-instance 'wl-shm-pool) shm-fd pool-size) #'wayland-destroy)
           (shm-buf1 (wl-shm-pool-create-buffer shm-pool (make-instance 'wl-buffer) 0
                                                width height stride :xrgb8888)
                     #'wayland-destroy)
           (shm-buf2 (wl-shm-pool-create-buffer shm-pool (make-instance 'wl-buffer) buffer-size
                                                width height stride :xrgb8888)
                     #'wayland-destroy)

           (decor (registry-bind-with-class registry "zxdg_decoration_manager_v1" 'zxdg-decoration-manager-v1
                                            :max-version 1)
                  #'wayland-destroy)
           (decoration-toplevel (zxdg-decoration-manager-v1-get-toplevel-decoration
                                 decor (make-instance 'zxdg-toplevel-decoration-v1) toplevel)
                                #'wayland-destroy))
        (setf (front-buffer main-window-surface) (cons (mem-aptr shm-pool-mem :char buffer-size) shm-buf2)
              (back-buffer main-window-surface) (cons (mem-aptr shm-pool-mem :char 0) shm-buf1))
        (wl-surface-commit main-window-surface)
        (do () ((not (open-p main-window-surface)))
          (wl-display-dispatch display)
          (maybe-render main-window-surface))))))
