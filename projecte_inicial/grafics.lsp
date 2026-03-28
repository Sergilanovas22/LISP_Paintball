;; Pràctica final de Llenguatges de Programació.
;; LISP - Paintball.
;; Estudiants: ABC, XYZ.
;; Professor: XXX.
;; Lliurament: primera convocatòria.
;; Fitxer del mòdul gràfic.
;; <Descripció de les funcions d'aquest fitxer>

;; Documentació d'això...

(defun pinta (mapa)
    (dolist (row mapa)
    (dolist (cell row)
      (print-cell cell))
    (terpri))
    "Pinta l'estat de la partida en un torn segons l'estat passat per paràmetre."
    42)

(defun print-cell (cell)
  (let ((color (cell-color cell))
        (label (cell-label cell))
        (special (or (member 'lab cell) (member 'base cell))))
    (format t "~a~a~a ~a~a~a"
            color
            (if special *bold* "")
            (if special *yellow* "")
            label
            *reset*
            " ")))

(defun cell-label (cell)
  "Etiqueta de 2 chars para mostrar en la celda."
  (cond ((equal (car cell) 'aigua) "~~")
        ((member 'lab  cell) "LB")
        ((member 'base cell) "BS")
        (t (string-upcase (symbol-name (cadr cell))))))

(defun cell-color (cell)
  "Devuelve el código ANSI de fondo según el tipo de celda."
  (cond ((equal (car cell) 'aigua) *bg-cyan*)
        ((equal (cadr cell) 'r)    *bg-red*)
        ((equal (cadr cell) 'g)    *bg-green*)
        ((equal (cadr cell) 'b)    *bg-blue*)
        (t "")))