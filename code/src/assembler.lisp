(in-package "RV32I-ASSEMBLER")

(defun my-write-file (filename lines)
  (with-open-file (stream filename
                          :direction :output
                          :if-exists :supersede)
    (loop for line in lines
          do (write-line line stream))))

(defun encode (parsed)
  (let* ((type (getf parsed :type))
        (op (getf parsed :op))
        (rd (reg->num (getf parsed :rd)))
        (rs1 (reg->num (getf parsed :rs1)))
        (info (get-opcode-binary op)))
    (unless type
      (error "Unkown instruction: ~A" op))
    (cond
      ((eq type :R)
       (encode-r (getf info :funct7)
                 (reg->num (getf parsed :rs2))
                 rs1
                 (getf info :funct3)
                 rd
                 (getf info :opcode)))

      ((eq type :I)
       (encode-i (sign-extend-12 (parse-integer (getf parsed :imm)))
                 rs1
                 (getf info :funct3)
                 rd
                 (getf info :opcode)))
      
      ((eq type :S)
       (encode-s (sign-extend-12 (parse-integer (getf parsed :imm)))
                 (reg->num (getf parsed :rs2))
                 rs1
                 (getf info :funct3)
                 (getf info :opcode)))

      ((eq type :B)
       (encode-b (sign-extend-12 (parse-integer (getf parsed :imm)))
                 (reg->num (getf parsed :rs2))
                 rs1
                 (getf info :funct3)
                 (getf info :opcode)))

      ((eq type :U)
       (encode-u (sign-extend-12 (parse-integer (getf parsed :imm)))
                 rd
                 (getf info :opcode)))

      ((eq type :J)
       (encode-j (sign-extend-12 (parse-integer (getf parsed :imm)))
                 rd
                 (getf info :opcode)))
      ((eq type :ECALL)
       #x00000073)
      )))

(defun assemble (input output)
  (write-binary output 
                 (mapcar #'encode (parse input))))

(defun write-binary (filename instructions)
  (with-open-file (stream filename
                          :direction :output
                          :if-exists :supersede
                          :element-type '(unsigned-byte 8))
    (dolist (inst instructions)
      (write-byte (logand inst #xFF) stream)
      (write-byte (logand (ash inst -8) #xFF) stream)
      (write-byte (logand (ash inst -16) #xFF) stream)
      (write-byte (logand (ash inst -24) #xFF) stream))))






















