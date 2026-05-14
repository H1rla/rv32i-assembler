(in-package "RV32I-ASSEMBLER")

(defparameter *registers*
  (let ((ht (make-hash-table :test #'equal)))
    (loop for (name . num) in
          '(("x0" . 0)  ("x1" . 1)  ("x2" . 2)  ("x3" . 3)
            ("x4" . 4)  ("x5" . 5)  ("x6" . 6)  ("x7" . 7)
            ("x8" . 8)  ("x9" . 9)  ("x10" . 10) ("x11" . 11)
            ("x12" . 12) ("x13" . 13) ("x14" . 14) ("x15" . 15)
            ("x16" . 16) ("x17" . 17) ("x18" . 18) ("x19" . 19)
            ("x20" . 20) ("x21" . 21) ("x22" . 22) ("x23" . 23)
            ("x24" . 24) ("x25" . 25) ("x26" . 26) ("x27" . 27)
            ("x28" . 28) ("x29" . 29) ("x30" . 30) ("x31" . 31))
          do (setf (gethash name ht) num))
    ht))

;;; レジスタ名 → 番号（nilや不正な名前はエラー）
(defun reg->num (reg)
  (cond
    ((null reg)
     (error "Missing register operand"))
    ((not (stringp reg))
     (error "Register must be a string, got: ~A" reg))
    (t
     (or (gethash reg *registers*)
         (error "Unknown register: ~A (expected x0-x31)" reg)))))

;;; 即値バリデーション
(defun validate-imm (imm min max context)
  (unless (integerp imm)
    (error "~A: immediate must be an integer, got: ~A" context imm))
  (unless (<= min imm max)
    (error "~A: immediate ~A out of range (~A to ~A)" context imm min max))
  imm)

;;; 12bit符号付き即値（-2048〜2047）
(defun validate-imm-12 (imm context)
  (validate-imm imm -2048 2047 context))

;;; 20bit符号なし即値（0〜1048575）LUI/AUIPC用
(defun validate-imm-20 (imm context)
  (validate-imm imm 0 1048575 context))

;;; 21bit符号付き即値（-1048576〜1048575）JAL用
(defun validate-imm-21 (imm context)
  (validate-imm imm -1048576 1048575 context))

;;; 13bit符号付き即値（-4096〜4094、2の倍数）B型用
(defun validate-imm-branch (imm context)
  (validate-imm imm -4096 4094 context)
  (unless (zerop (logand imm 1))
    (error "~A: branch immediate must be even, got: ~A" context imm))
  imm)

;;; 12bit符号拡張
(defun sign-extend-12 (imm)
  (if (>= imm 2048)
      (- imm 4096)
      imm))

;;; 文字列を安全にparseして整数に変換
(defun parse-imm (str context)
  (unless (stringp str)
    (error "~A: expected immediate string, got: ~A" context str))
  (handler-case
      (parse-integer str)
    (error ()
      (error "~A: invalid immediate value: ~A" context str))))

;;; デリミタ判定
(defun my-delimiter-p (c)
  (or (char= c #\Space)
      (char= c #\,)
      (char= c #\()
      (char= c #\))))

;;; 文字列分割
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

;;; パイプライン
(defun my-pipeline (lst filter-p tranform-fn)
  (mapcar tranform-fn (remove-if-not filter-p lst)))
