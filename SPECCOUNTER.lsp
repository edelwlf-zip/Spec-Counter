;;; =============================================================================
;;;                   NASSLISP SPEC COUNTER.lsp
;;; Counts AutoCAD Plant 3D Specs and their total pipe lengths
;;; Usage: Load the .lsp file using APPLOAD, then type "SPEC" in the command line
;;; =============================================================================

(defun C:SPEC ( / ss i ent entData
                             specName specList specEntry
                             pipeLength totalLen
                             objClass propVal )

  (princ "\n--- Plant 3D Spec Counter ---\n")

  (setq specList '())

  (setq ss (ssget "_X"
              (list
                (cons 0 "ADEOBJ,*")          ; catches ADE/Plant3D objects
                ;; Broad filter – we'll refine by DXF class below
              )
            )
  )

  (if (null ss)
    (setq ss (ssget "_X"))
  )

  (if (null ss)
    (progn
      (princ "\nNo entities found in the drawing.")
      (princ)
      (exit)
    )
  )

  (setq i 0)
  (while (< i (sslength ss))
    (setq ent     (ssname ss i)
          entData (entget ent)
          objClass (vla-get-objectname (vlax-ename->vla-object ent))
    )

    (if (or (wcmatch objClass "*Pipe*")
            (wcmatch objClass "*PIPE*")
            (wcmatch objClass "AcPp*"))
      (progn
        (setq specName  (P3D-GetProperty ent "Spec")
              pipeLength (P3D-GetPipeLength ent)
        )

        (if (or (null specName) (= specName ""))
          (setq specName "<No Spec>")
        )
        (if (null pipeLength)
          (setq pipeLength 0.0)
        )

        (setq specList (P3D-Accumulate specList specName pipeLength))
      )
    )

    (setq i (1+ i))
  )

  (if (null specList)
    (progn
      (princ "\nNo Plant 3D pipe objects with Spec data were found.")
    )
    (progn
      (princ (strcat "\n" (P3D-RepeatChar "=" 55)))
      (princ (strcat "\n  " (P3D-PadRight "SPEC NAME" 30)
                            (P3D-PadLeft  "COUNT" 8)
                            (P3D-PadLeft  "TOTAL LENGTH" 14)))
      (princ (strcat "\n  " (P3D-RepeatChar "-" 52)))

      (foreach entry specList
        (setq specName (car   entry)
              count    (cadr  entry)
              totalLen (caddr entry))
        (princ
          (strcat "\n  "
            (P3D-PadRight specName 30)
            (P3D-PadLeft  (itoa count) 8)
            (P3D-PadLeft  (strcat (rtos totalLen 2 2) " [dwg units]") 14)
          )
        )
      )

      (princ (strcat "\n" (P3D-RepeatChar "=" 55)))
      (princ (strcat "\n  Total unique specs found: " (itoa (length specList))))
    )
  )

  (princ "\n")
  (princ)   ; suppress extra return value
)


(defun P3D-GetProperty ( ent propName / vObj propSets ps p val )
  (setq val nil)
  (if (setq vObj (vlax-ename->vla-object ent))
    (progn
      ;; Try AcPp property sets (Plant 3D stores data in property sets)
      (if (vlax-property-available-p vObj 'PropertySets)
        (progn
          (setq propSets (vlax-get vObj 'PropertySets))
          (if propSets
            (vlax-for ps propSets
              (vlax-for p ps
                (if (= (strcase (vlax-get p 'Name)) (strcase propName))
                  (setq val (vlax-get p 'Value))
                )
              )
            )
          )
        )
      )

      ;; Fallback: try direct property on the object
      (if (and (null val)
               (vlax-property-available-p vObj (read propName)))
        (setq val (vlax-get vObj (read propName)))
      )

      ;; Second fallback: scan DXF group codes for XDATA / custom data
      (if (null val)
        (setq val (P3D-GetXDataProp ent propName))
      )
    )
  )
  (if val (vl-princ-to-string val) nil)
)

;;XData helper

(defun P3D-GetXDataProp ( ent propName / xd pair found result )
  (setq xd    (assoc -3 (entget ent '("*")))
        found nil)
  (if xd
    (foreach appEntry (cdr xd)
      (foreach pair (cdr appEntry)
        (if (and (= (car pair) 1000)
                 (wcmatch (strcase (cdr pair))
                          (strcase (strcat "*" propName "*"))))
          (setq found T)
        )
        (if (and found (= (car pair) 1000) (not (wcmatch (cdr pair) (strcat "*" propName "*"))))
          (progn
            (setq result (cdr pair)
                  found  nil)
          )
        )
      )
    )
  )
  result
)

;;helper

(defun P3D-GetPipeLength ( ent / vObj len )
  (setq len 0.0)
  (setq vObj (vlax-ename->vla-object ent))
  (if vObj
    (progn
      ;; Plant 3D pipes expose 'Length' directly
      (cond
        ((vlax-property-available-p vObj 'Length)
         (setq len (vlax-get vObj 'Length)))

        ((vlax-property-available-p vObj 'PipeLength)
         (setq len (vlax-get vObj 'PipeLength)))

        ;; 3D polyline / spline fallback
        ((vlax-property-available-p vObj 'Length)
         (setq len (vlax-get vObj 'Length)))

        (T (setq len 0.0))
      )
    )
  )
  (if (numberp len) (float len) 0.0)
)

;;helper

(defun P3D-Accumulate ( lst specName addLen / entry rest newEntry )
  (setq entry (assoc specName lst))
  (if entry
    ;; Update existing entry
    (progn
      (setq rest    (vl-remove entry lst)
            newEntry (list specName
                           (1+ (cadr entry))
                           (+ (caddr entry) addLen)))
      (cons newEntry rest)
    )
    ;; Add new entry
    (cons (list specName 1 addLen) lst)
  )
)

;;helper

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

(princ "\nThank you for choosing NASSLISP. SPEC-COUNTER loaded. Type SPEC to run.")
(princ)