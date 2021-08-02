(defun pdf-cscroll-triplet-image-object (page)
  (interactive)
    (let* ((width (window-width nil t))
           (top (pdf-info-renderpage (1- page) width))
           (top-height (cdr (image-size (create-image top 'png t) t)))
           (middle-start-height top-height)
           (middle (pdf-info-renderpage page width))
           (middle-height (cdr (image-size (create-image middle 'png t) t)))
           (bottom-start-height (+ middle-start-height
                                   middle-height))
           (bottom (pdf-info-renderpage (1+ page) width))
           (bottom-height (cdr (image-size (create-image bottom 'png t) t)))
           (total-height (+ bottom-start-height
                            bottom-height))
           (svg (svg-create width total-height)))
      (mapc (lambda (z)
              (apply
               (lambda (x y z)
                 (svg-embed svg z "image/png" t
                            :width (format "%spx" width) :height (format "%spx" y)
                            :x "0px" :y (format "%spx" x))
                 (svg-line svg 0 x width x :stroke "black"))
               z))
            `((0 ,top-height ,top)
              (,middle-start-height ,middle-height ,middle) (,bottom-start-height ,bottom-height ,bottom)))
      (svg-image svg)))

(defun pdf-cscroll-goto-page (page)
  (remove-overlays (point-min) (point-max))
  (let* ((triplet (if (eq page 1) 2 page))
         (page-triplet (if-let (image-object
                                (cdar (cl-member triplet pdf-cscroll-cache :key #'car)))
                           image-object
                         (pdf-cscroll-triplet-image-object triplet)))
         (image-size (image-size page-triplet t))
         (ol (make-overlay (point-min) (point-max))))
    ;; (insert-image page-triplet)
    (overlay-put ol 'display page-triplet)
    (setq-local image-size image-size)
    (setq-local current-triplet triplet)
    ;; (setq-local current-page page)
    ;; (add-to-list 'pdf-cscroll-cache (cons triplet page-triplet))
    (unless (eq page 1)
      (image-set-window-vscroll (/ (cdr image-size) 3))
      (image-scroll-left))))

(defun pdf-cscroll-scroll-up ()
  (interactive)
  (when (= (window-vscroll nil pdf-view-have-image-mode-pixel-vscroll)
           (image-scroll-up 4))
    (pdf-cscroll-goto-page (1+ current-triplet))
    (image-set-window-vscroll (- (cdr image-size)
                                 (window-pixel-height)
                                 (/ (cdr image-size) 3)))))

(defun pdf-cscroll-scroll-down ()
  (interactive)
  (when (= (window-vscroll nil pdf-view-have-image-mode-pixel-vscroll)
           (image-scroll-down 4))
    (pdf-cscroll-goto-page (1- current-triplet))))

(defun pdf-cscroll-next-page ()
  (interactive)
  (let ((vscroll (window-vscroll nil pdf-view-have-image-mode-pixel-vscroll)))
    (pdf-cscroll-goto-page (1+ current-triplet))
    (image-set-window-vscroll vscroll)))

(defun pdf-cscroll-previous-page ()
  (interactive)
  (let ((vscroll (window-vscroll nil pdf-view-have-image-mode-pixel-vscroll)))
    (pdf-cscroll-goto-page (1- current-triplet))
    (image-set-window-vscroll vscroll)))

(define-derived-mode pdf-cscroll-mode special-mode "pdf-cscroll-mode"
  "Versatile pdf editor"
  (defvar-local pdf-cscroll-cache '())
  (pdf-cscroll-goto-page 1)
  (image-mode-setup-winprops))

(define-key pdf-cscroll-mode-map (kbd "C-n") 'pdf-cscroll-scroll-up)
(define-key pdf-cscroll-mode-map (kbd "C-p") 'pdf-cscroll-scroll-down)

(evilified-state-evilify-map pdf-cscroll-mode-map
  :mode pdf-cscroll-mode
  :bindings
  "j" 'pdf-cscroll-scroll-up
  "k" 'pdf-cscroll-scroll-down
  "J" 'pdf-cscroll-next-page
  "K" 'pdf-cscroll-previous-page)
