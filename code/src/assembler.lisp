(in-package "RV32I-ASSEMBLER")


(defun encode (parsed)
(let* ((type (getf parsed :type))
       (op   (getf parsed :op))
       (info (get-opcode-binary op)))
  (unless type
    (error "Unknown instruction: ~A" op))
  (unless info
    (error "No encoding info for instruction: ~A" op))
  (cond
    ((eq type :R)
     (encode-r (getf info :funct7)
               (reg->num (getf parsed :rs2))
               (reg->num (getf parsed :rs1))
               (getf info :funct3)
               (reg->num (getf parsed :rd))
               (getf info :opcode)))

    ((eq type :I)
     (let* ((imm-str (getf parsed :imm))
            (imm-raw (parse-imm imm-str op))
            (imm     (sign-extend-12
                      (validate-imm-12 imm-raw op))))
       (encode-i imm
                 (reg->num (getf parsed :rs1))
                 (getf info :funct3)
                 (reg->num (getf parsed :rd))
                 (getf info :opcode))))

    ((eq type :S)
     (let* ((imm-str (getf parsed :imm))
            (imm-raw (parse-imm imm-str op))
            (imm     (sign-extend-12
                      (validate-imm-12 imm-raw op))))
       (encode-s imm
                 (reg->num (getf parsed :rs2))
                 (reg->num (getf parsed :rs1))
                 (getf info :funct3)
                 (getf info :opcode))))

    ((eq type :B)
     (let* ((imm-str (getf parsed :imm))
            (imm-raw (parse-imm imm-str op))
            (imm     (validate-imm-branch imm-raw op)))
       (encode-b imm
                 (reg->num (getf parsed :rs2))
                 (reg->num (getf parsed :rs1))
                 (getf info :funct3)
                 (getf info :opcode))))

    ((eq type :U)
     (let* ((imm-str (getf parsed :imm))
            (imm-raw (parse-imm imm-str op))
            (imm     (validate-imm-20 imm-raw op)))
       (encode-u imm
                 (reg->num (getf parsed :rd))
                 (getf info :opcode))))

    ((eq type :J)
     (let* ((imm-str (getf parsed :imm))
            (imm-raw (parse-imm imm-str op))
            (imm     (validate-imm-21 imm-raw op)))
       (encode-j imm
                 (reg->num (getf parsed :rd))
                 (getf info :opcode))))

    ((eq type :ECALL)
     #x00000073)

    (t (error "Unhandled instruction type: ~A for ~A" type op)))))



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
      (write-byte (logand (ash inst -8)  #xFF) stream)
      (write-byte (logand (ash inst -16) #xFF) stream)
      (write-byte (logand (ash inst -24) #xFF) stream))))
