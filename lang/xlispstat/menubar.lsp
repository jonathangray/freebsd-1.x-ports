;;;
;;; Fake menu bar for UNIX systems with graphics
;;; This is a complete hack but at least provides enough functionality
;;; to do the examples in the book.
;;;

#+macintosh(error "this will mess up the macintosh menu system")

(provide "menubar")

(defun make-fake-menu-bar ()
  (cond ((and (boundp '*fake-menu-bar*) *fake-menu-bar*)
         (send *fake-menu-bar* :show-window))
    (t (let* ((ascent (send graph-window-proto :text-ascent))
              (descent (send graph-window-proto :text-descent))
              (gap (floor (/ ascent 2)))
              (width 400))
         (setf *fake-menu-bar*
               (send graph-window-proto :new 
                     :title "Menu Bar"
                     :menu-button nil 
                     :size (list width (+ ascent descent (* 2 gap))))))

       (send *fake-menu-bar* :add-slot 'menus)

       (defmeth *fake-menu-bar* :menus (&optional (menus nil set))
         (if set (setf (slot-value 'menus) menus))
         (slot-value 'menus))

       (defmeth *fake-menu-bar* :install-menu (menu)
	 (unless (member menu (send self :menus))
		 (send self :menus (append (send self :menus) (list menu)))
		 (send self :show-window)
		 (send self :redraw)))

       (defmeth *fake-menu-bar* :remove-menu (menu)
         (send self :menus (remove menu (send self :menus)))
         (send self :redraw))

       (defmeth *fake-menu-bar* :redraw ()
         (let* ((ascent (send self :text-ascent))
                (gap (floor (/ ascent 2)))
                (menus (send self :menus))
                (left gap)
                (bottom (+ gap ascent)))
           (apply #'send self :erase-rect (send self :view-rect))
           (dolist (m menus)
                   (let ((title (send m :title)))
                     (send self :draw-string title left bottom)
                     (setf left (+ left
                                   gap 
                                   (send self :text-width title)))))))

       (defmeth *fake-menu-bar* :do-click (x y m1 m2)
	 (let* ((loc (+ (list x y) (send self :location)))
		(gap (floor (/ (send self :text-ascent) 2)))
		(menus (send self :menus))
		(x (- x gap)))
	   (dolist (m menus)
		   (let ((w (send self :text-width (send m :title))))
		     (when (< 0 x w)
			   (apply #'send m :popup loc)
			   (return))
		     (setf x (- x gap w))))))
       (defun find-menu (name)
	 (dolist (m (send *fake-menu-bar* :menus))
		 (if (string-equal (string name) (send m :title))
		     (return m)))))))

(defmeth menu-proto :install ()
  (make-fake-menu-bar)
  (send *fake-menu-bar* :install-menu self))

(defmeth menu-proto :remove ()
  (send *fake-menu-bar* :remove-menu self))
