(in-package "RV32I-ASSEMBLER")

(defparameter *load-instructions*
  '("lw" "lh" "lb" "lhu" "lbu"))

(defparameter *instruction-types*
  (let ((ht (make-hash-table :test #'equal)))
    (loop for (name . type) in
          '(("add"  . :R) ("sub"  . :R) ("and"  . :R) ("or"   . :R)
            ("xor"  . :R) ("sll"  . :R) ("srl"  . :R) ("sra"  . :R)
            ("slt"  . :R) ("sltu" . :R)
            ("addi" . :I) ("andi" . :I) ("ori"  . :I) ("xori" . :I)
            ("slti" . :I) ("sltiu". :I) ("slli" . :I) ("srli" . :I)
            ("srai" . :I) ("lw"   . :I) ("lh"   . :I) ("lb"   . :I)
            ("lhu"  . :I) ("lbu"  . :I) ("jalr" . :I)
            ("sw"   . :S) ("sh"   . :S) ("sb"   . :S)
            ("beq"  . :B) ("bne"  . :B) ("blt"  . :B)
            ("bge"  . :B) ("bltu" . :B) ("bgeu" . :B)
            ("lui"  . :U) ("auipc". :U)
            ("jal"  . :J)
            ("ecall". :ECALL))
          do (setf (gethash name ht) type))
    ht))

(defun get-instruction-type (op)
  (gethash op *instruction-types*))

;;; ファイル読み込み（存在チェック付き）
(defun my-read-file (filename)
  (unless (probe-file filename)
    (error "Input file not found: ~A" filename))
  (with-open-file (stream filename :direction :input)
    (loop for line = (read-line stream nil nil)
          while line
          collect line)))

(defun load-inst-p (op)
  (member op *load-instructions* :test #'string=))

;;; ラベル行かどうか判定（末尾が: かつスペースなし）
(defun label-line-p (trimmed)
  (and (> (length trimmed) 1)
       (char= (char trimmed (1- (length trimmed))) #\:)
       (not (find #\Space trimmed))))

;;; ラベル名を取得（末尾の:を除去）
(defun extract-label (trimmed)
  (subseq trimmed 0 (1- (length trimmed))))

;;; トークン数チェック
(defun check-token-count (tokens expected op)
  (unless (>= (length tokens) expected)
    (error "~A: expected ~A operands, got ~A (tokens: ~A)"
           op (1- expected) (1- (length tokens)) tokens)))

(defun parse-line (tokens)
  (when (null tokens) (return-from parse-line nil))
  (let* ((op   (car tokens))
         (type (get-instruction-type op)))
    (unless type
      (error "Unknown instruction: ~A" op))
    (cond
      ((eq type :R)
       (check-token-count tokens 4 op)
       (list :type :R
             :op   op
             :rd   (cadr tokens)
             :rs1  (caddr tokens)
             :rs2  (cadddr tokens)))

      ((and (eq type :I) (load-inst-p op))
       (check-token-count tokens 4 op)
       (list :type :I
             :op   op
             :rd   (cadr tokens)
             :imm  (caddr tokens)
             :rs1  (cadddr tokens)))

      ((eq type :I)
       (check-token-count tokens 4 op)
       (list :type :I
             :op   op
             :rd   (cadr tokens)
             :rs1  (caddr tokens)
             :imm  (cadddr tokens)))

      ((eq type :B)
       (check-token-count tokens 4 op)
       (list :type :B
             :op   op
             :rs1  (cadr tokens)
             :rs2  (caddr tokens)
             :imm  (cadddr tokens)))

      ((eq type :U)
       (check-token-count tokens 3 op)
       (list :type :U
             :op   op
             :rd   (cadr tokens)
             :imm  (caddr tokens)))

      ((eq type :S)
       (check-token-count tokens 4 op)
       (list :type :S
             :op   op
             :rs2  (cadr tokens)
             :imm  (caddr tokens)
             :rs1  (cadddr tokens)))

      ((eq type :J)
       (check-token-count tokens 3 op)
       (list :type :J
             :op   op
             :rd   (cadr tokens)
             :imm  (caddr tokens)))

      ((eq type :ECALL)
       (list :type :ECALL :op op)))))

;;; ラベルか即値かを判定して数値に変換
(defun resolve-imm (imm-str current-addr label-table)
  (let ((label-addr (gethash imm-str label-table)))
    (if label-addr
        ;; ラベルなら相対アドレスを計算
        (- label-addr current-addr)
        ;; 数値ならそのままparse
        (handler-case
            (parse-integer imm-str)
          (error ()
            (error "Unknown label or invalid immediate: ~A" imm-str))))))

;;; 2パス方式でパース
;;; 1パス目：ラベルテーブル構築 + 命令収集
;;; 2パス目：ラベルを相対アドレスに解決
(defun parse (filename)
  (let ((lines       (my-read-file filename))
        (label-table (make-hash-table :test #'equal))
        (raw-insts   '())
        (addr        0))

    ;; 1パス目
    (dolist (line lines)
      (let ((trimmed (string-trim " " line)))
        (cond
          ;; 空行・コメント行
          ((or (= (length trimmed) 0)
               (char= (char trimmed 0) #\;)))
          ;; ラベル行
          ((label-line-p trimmed)
           (let ((label (extract-label trimmed)))
             (when (gethash label label-table)
               (error "Duplicate label: ~A" label))
             (setf (gethash label label-table) addr)))
          ;; 命令行
          (t
           (let ((parsed (parse-line (my-split trimmed))))
             (when parsed
               (push (cons addr parsed) raw-insts)
               (incf addr 4)))))))

    ;; 2パス目：ラベルを解決
    (mapcar (lambda (entry)
              (let ((inst-addr (car entry))
                    (inst      (cdr entry)))
                (let ((imm-str (getf inst :imm)))
                  (if imm-str
                      (let* ((resolved (resolve-imm imm-str inst-addr label-table))
                             (new-inst (copy-list inst)))
                        (setf (getf new-inst :imm)
                              (write-to-string resolved))
                        new-inst)
                      inst))))
            (nreverse raw-insts))))
