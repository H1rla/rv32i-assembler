(in-package "RV32I-ASSEMBLER")

(defparameter *registers*
  (let ((ht (make-hash-table :test #'equal)))
    (loop for (name . num) in
          '(("x0" . 0) ("x1" . 1) ("x2" . 2) ("x3" . 3)
            ("x4" . 4) ("x5" . 5) ("x6" . 6) ("x7" . 7)
            ("x8" . 8) ("x9" . 9) ("x10" . 10) ("x11" . 11)
            ("x12" . 12) ("x13" . 13) ("x14" . 14) ("x15" . 15)
            ("x16" . 16) ("x17" . 17) ("x18" . 18) ("x19" . 19)
            ("x20" . 20) ("x21" . 21) ("x22" . 22) ("x23" . 23)
            ("x24" . 24) ("x25" . 25) ("x26" . 26) ("x27" . 27)
            ("x28" . 28) ("x29" . 29) ("x30" . 30) ("x31" . 31))
          do (setf (gethash name ht) num))
    ht))

(defun reg->num (reg)
  (if (null reg)
      nil
      (or (gethash reg *registers*)
          (error "Unknown register: ~A" reg))))


(defun sign-extend-12 (imm)
  (if (>= imm 2048)
      (- imm 4096)
      imm))

(defun my-split (str)
  (let ((start 0)
        (result '()))
    (loop for i from 0 below (length str)
          for c = (char str i)
          when (my-delimiter-p c)
          do (progn
               (push (subseq str start i) result)
               (setf start (+ i 1))))
    (push (subseq str start (length str)) result)
    (remove-if #'(lambda (s) (string= s "")) (nreverse result))))

(defun my-delimiter-p (c)
  (or (char= c #\Space)
      (char= c #\, )
      (char= c #\( )
      (char= c #\) )))

(defun my-pipeline (lst filter-p tranform-fn)
  (mapcar tranform-fn (remove-if-not filter-p lst)))


