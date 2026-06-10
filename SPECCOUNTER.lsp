;;; =============================================================================
;;;                   NASSLISP SPEC COUNTER v4
;;; Creator: Daniel Nass                                              10.06.2026
;;; Counts AutoCAD Plant 3D Specs and their total pipe lengths
;;; Usage: Load the .lsp file using APPLOAD, then type "SPEC" in the command line
;;; =============================================================================

(defun C:SPEC ( / ss i ent obj objClass specName rawLen cutLen specList totalLen )
  (vl-load-com)
  (setq specList '())

  (princ "\nSelecting all pipe objects in drawing...")
  (setq ss (ssget "_X" (list (cons 0 "ACPPPIPE"))))

  (if (null ss)
    (progn
      (princ "\nNo AcPpDb3dPipe objects found in this drawing.")
      (princ)
      (exit)
    )
  )

  (princ (strcat "\nFound " (itoa (sslength ss)) " pipe(s). Reading properties..."))

  (setq i 0)
  (repeat (sslength ss)
    (setq ent (ssname ss i)
          obj (vlax-ename->vla-object ent))

    ;; --- Spec name ---
    (setq specName nil)
    (if (vlax-property-available-p obj 'Spec)
      (setq specName (vlax-get obj 'Spec))
    )
    (if (or (null specName) (= specName ""))
      (setq specName "<No Spec>")
    )

    ;; --- Cut length: use vlax-get, same as RohrSumme pattern 
    (setq rawLen nil
          cutLen 0.0)
    (cond
      ((vlax-property-available-p obj 'CutLength)
       (setq rawLen (vlax-get obj 'CutLength)))
      ((vlax-property-available-p obj 'Length)
       (setq rawLen (vlax-get obj 'Length)))
    )
    (if rawLen
      (if (= (type rawLen) 'STR)
        (setq cutLen (atof rawLen))
        (setq cutLen (float rawLen))
      )
    )

    ;; --- Accumulate ---
    (setq specList (P3D-Accumulate specList specName cutLen))

    (setq i (1+ i))
  )

  ;; --- Sort by spec name ---
  (setq specList (vl-sort specList '(lambda (a b) (< (car a) (car b)))))

  ;; --- Print results ---
  (princ (strcat "\n" (P3D-RepeatChar "=" 65)))
  (princ "\n        PLANT 3D SPEC COUNTER - RESULTS")
  (princ (strcat "\n" (P3D-RepeatChar "=" 65)))
  (princ (strcat "\n  "
    (P3D-PadRight "SPEC"        28)
    (P3D-PadLeft  "COUNT"        7)
    (P3D-PadLeft  "TOTAL (mm)"  14)
    (P3D-PadLeft  "TOTAL (m)"   12)
  ))
  (princ (strcat "\n  " (P3D-RepeatChar "-" 20

  (foreach entry specList
    (setq specName (car   entry)
          count    (cadr  entry)
          totalLen (caddr entry))
    (princ (strcat "\n  "
      (P3D-PadRight specName 28)
      (P3D-PadLeft  (itoa count) 7)
      (P3D-PadLeft  (strcat (rtos totalLen 2 2) " mm") 14)
      (P3D-PadLeft  (strcat (rtos (/ totalLen 1000.0) 2 3) " m") 12)
    ))
  )

  (princ (strcat "\n  " (P3D-RepeatChar "-" 60)))
  (princ (strcat "\n  Total unique specs : " (itoa (length specList))))
  (princ (strcat "\n  Total pipes        : " (itoa (sslength ss))))
  (princ (strcat "\n" (P3D-RepeatChar "=" 65)))
  (princ)
)


;;; Accumulate spec entries  ("SpecName" count totalLength)
(defun P3D-Accumulate ( lst specName addLen / entry rest )
  (setq entry (assoc specName lst))
  (if entry
    (append
      (vl-remove entry lst)
      (list (list specName (1+ (cadr entry)) (+ (caddr entry) addLen)))
    )
    (append lst (list (list specName 1 addLen)))
  )
)


;;; Formatting helpers
(defun P3D-PadRight ( str width / pad )
  (setq pad (- width (strlen str)))
  (if (< pad 0) (setq pad 0))
  (strcat str (P3D-RepeatChar " " pad))
)

(defun P3D-PadLeft ( str width / pad )
  (setq pad (- width (strlen str)))
  (if (< pad 0) (setq pad 0))
  (strcat (P3D-RepeatChar " " pad) str)
)

(defun P3D-RepeatChar ( ch n / result )
  (setq result "")
  (repeat n (setq result (strcat result ch)))
  result
)

(princ "\nThank you for choosing NASSLISP. SPEC-COUNTER.v4 loaded. Type SPEC to run.")
(princ)
