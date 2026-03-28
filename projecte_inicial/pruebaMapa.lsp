;;; ====================================================
;;;  Dibuix del mapa – XLISP-PLUS
;;;  Assignatura 21721 – Llenguatges de Programació
;;;
;;;  Ús: carregar el fitxer i cridar (pinta-mapa)
;;;  Unix: obrir la finestra primer amb (mode 0 0 640 375)
;;; ====================================================


;;; ---- Constants de mida i posició ----------------------

(setq m  18)    ; píxels per casella (20 col x 18 = 360px)
(setq xi 10)    ; x inicial (esquerra del mapa)
(setq nf 20)    ; nombre de files del mapa

; y inicial = cantonada inferior-esquerra de la fila 0 (dalt del mapa)
; El sistema de coordenades XLISP té y=0 a baix, y creix cap amunt.
; Fila 0 del mapa ha d'aparèixer a dalt, per tant té la y més alta.
(setq yi (+ 10 (* (- nf 1) m)))   ; = 10 + 19*18 = 352


;;; ---- El mapa (20 files x 20 columnes) -----------------
;;;
;;;  Cada casella és una llista: (tipus color element equip)
;;;    tipus  : agua | terra
;;;    color  : r | g | b         (només per terra)
;;;    element: lab | base | nil  (opcional)
;;;    equip  : e1 | e2           (opcional, només amb base)




;;; ---- Helpers d'accés a casella ------------------------

; Tipus: (car cel)       => agua | terra
; Color: (cadr cel)      => r | g | b  (nil si agua)
; Elem:  (caddr cel)     => lab | base | nil
; Equip: (cadddr cel)    => e1 | e2 | nil


;;; ---- Color RGB segons la casella ----------------------

(defun rgb-casella (cel)
  (cond
    ((eq (car cel) 'aigua) (list  92 168 208))  ; blau clar
    ((eq (cadr cel) 'r)   (list 212  96  96))  ; vermell
    ((eq (cadr cel) 'g)   (list  91 166  91))  ; verd
    ((eq (cadr cel) 'b)   (list  79 125 200))  ; blau fosc
    (t                    (list 160 160 160)))) ; gris (fallback)


;;; ---- Emplenar rectangle amb línies horitzontals -------
;;;  Pinta (m-1) línies des de (x,y) fins a (x+m-2, y) pujant

(defun ple (x y h)
  (cond ((<= h 0) nil)
        (t
         (move x y)
         (draw (+ x (- m 2)) y)
         (ple x (+ y 1) (- h 1)))))


;;; ---- Marcar element especial al centre ----------------

(defun pinta-elem (cx cy elem)
  (cond
    ; Lab: creu blanca diagonal (marca discreta)
    ((eq elem 'lab)
     (color 255 255 255)
     (move (- cx 4) (- cy 4)) (draw (+ cx 4) (+ cy 4))
     (move (+ cx 4) (- cy 4)) (draw (- cx 4) (+ cy 4)))

    ; Base: quadrat blanc petit + creu interior
    ((eq elem 'base)
     (color 255 255 255)
     (move (- cx 5) (- cy 5))
     (draw (+ cx 5) (- cy 5))
     (draw (+ cx 5) (+ cy 5))
     (draw (- cx 5) (+ cy 5))
     (draw (- cx 5) (- cy 5))
     (move (- cx 3) cy) (draw (+ cx 3) cy)
     (move cx (- cy 3)) (draw cx (+ cy 3)))

    (t nil)))


;;; ---- Pintar una casella individual --------------------

(defun pinta-cel (x y cel)
  (let* ((rgb   (rgb-casella cel))
         (elem  (caddr cel))
         ; centre de la casella per als elements
         (cx    (+ x (/ m 2)))
         (cy    (+ y (/ m 2))))
    ; 1. Farcit de color
    (color (car rgb) (cadr rgb) (caddr rgb))
    (ple x y (- m 1))
    ; 2. Contorn fosc (opcional, fa més llegible la quadrícula)
    (color 40 40 40)
    (move x y)
    (draw (+ x (- m 1)) y)
    (draw (+ x (- m 1)) (+ y (- m 1)))
    (draw x (+ y (- m 1)))
    (draw x y)
    ; 3. Element especial (lab / base)
    (pinta-elem cx cy elem)))


;;; ---- Pintar una fila (d'esquerra a dreta) -------------

(defun pinta-fila (fila x y)
  (cond ((null fila) nil)
        (t
         (pinta-cel x y (car fila))
         (pinta-fila (cdr fila) (+ x m) y))))


;;; ---- Pintar totes les files (de dalt a baix) ----------
;;;  yi és la y de la fila 0 (dalt del mapa, y més alta)
;;;  Cada fila baixa m píxels (y decreix en XLISP)

(defun pinta-files (files y)
  (cond ((null files) nil)
        (t
         (pinta-fila (car files) xi y)
         (pinta-files (cdr files) (- y m)))))


;;; ---- Funció principal ---------------------------------

(defun pinta-mapa (mapa)
  (cls)
  (pinta-files mapa yi))


;;; ====================================================
;;;  Crida: (pinta-mapa)
;;;  Unix (abans):  (mode 0 0 640 375)
;;; ====================================================