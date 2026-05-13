(in-package "RV32I-ASSEMBLER")

(defparameter *load-instructions*
  '("lw" "lh" "lb" "lhu" "lbu"))


(defparameter *instruction-types*
  (let ((ht (make-hash-table :test #'equal)))
    (loop for (name . type) in
          '(("add" . :R) ("sub" . :R) ("and" . :R) ("or" . :R)
            ("xor" . :R) ("sll" . :R) ("srl" . :R) ("sra" . :R)
            ("slt" . :R) ("sltu" . :R)
            ("addi" . :I) ("andi" . :I) ("ori" . :I) ("xori" . :I)
            ("slti" . :I) ("sltiu" . :I) ("slli" . :I) ("srli" . :I)
            ("srai" . :I) ("lw" . :I) ("lh" . :I) ("lb" . :I)
            ("lhu" . :I) ("lbu" . :I) ("jalr" . :I)
            ("sw" . :S) ("sh" . :S) ("sb" . :S)
            ("beq" . :B) ("bne" . :B) ("blt" . :B)
            ("bge" . :B) ("bltu" . :B) ("bgeu" . :B)
            ("lui" . :U) ("auipc" . :U)
            ("jal" . :J)
            ("ecall" . :ECALL))
          do (setf (gethash name ht) type))
    ht))

(defun get-instruction-type (op)
  (gethash op *instruction-types*))


(defun my-read-file (filename)
  (with-open-file (stream filename :direction :input)
    (loop
      for line = (read-line stream nil nil)
      while line 
      collect line)))

(defun load-inst-p (op)
  (member op *load-instructions* :test #'string=))

(defun parse-line (tokens)
  (let* ((op   (car tokens))
         (type (get-instruction-type op)))
    (cond
      ((eq type :R) 
       (list :type :R
             :op op
             :rd (cadr tokens)
             :rs1 (caddr tokens)
             :rs2 (cadddr tokens)))
      ((and (eq type :I)
            (load-inst-p op))
       (list :type :I
             :op op
             :rd (cadr tokens)
             :imm (caddr tokens)
             :rs1 (cadddr tokens)))
      ((eq type :I)
       (list :type :I
             :op op
             :rd (cadr tokens)
             :rs1 (caddr tokens)
             :imm (cadddr tokens)))
      ((eq type :B)
       (list :type :B
             :op op
             :rs1 (cadr tokens)
             :rs2 (caddr tokens)
             :imm (cadddr tokens)))
      ((eq type :U)
       (list :type :U
             :op op
             :rd (cadr tokens)
             :imm (caddr tokens)))
      ((eq type :S)
       (list :type :S
             :op op
             :rs2 (cadr tokens)
             :imm (caddr tokens)
             :rs1 (cadddr tokens)))
      ((eq type :J)
       (list :type :J
             :op op
             :rd (cadr tokens)
             :imm (caddr tokens)))
      ((eq type :ECALL)
       (list :type :ECALL :op op)))))


(defun parse (filename)
  (remove nil
    (mapcar (lambda (line)
              (let ((trimmed (string-trim " " line)))
                (when (and (> (length trimmed) 0)
                           (not (char= (char trimmed 0) #\;)))
                  (let ((result (parse-line (my-split trimmed))))
                    (unless result
                      (error "Unknown instruction: ~A" trimmed))
                    result))))
            (my-read-file filename))))
















