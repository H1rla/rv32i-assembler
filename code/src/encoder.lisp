(in-package "RV32I-ASSEMBLER")

(defparameter *instruction-table*
  (let ((ht (make-hash-table :test #'equal)))
    (loop for (name . info) in
          '(("add"  . (:type :R :funct3 #b000 :funct7 #b0000000 :opcode #b0110011))
            ("sub"  . (:type :R :funct3 #b000 :funct7 #b0100000 :opcode #b0110011))
            ("and"  . (:type :R :funct3 #b111 :funct7 #b0000000 :opcode #b0110011))
            ("or"   . (:type :R :funct3 #b110 :funct7 #b0000000 :opcode #b0110011))
            ("xor"  . (:type :R :funct3 #b100 :funct7 #b0000000 :opcode #b0110011))
            ("sll"  . (:type :R :funct3 #b001 :funct7 #b0000000 :opcode #b0110011))
            ("srl"  . (:type :R :funct3 #b101 :funct7 #b0000000 :opcode #b0110011))
            ("sra"  . (:type :R :funct3 #b101 :funct7 #b0100000 :opcode #b0110011))
            ("slt"  . (:type :R :funct3 #b010 :funct7 #b0000000 :opcode #b0110011))
            ("sltu" . (:type :R :funct3 #b011 :funct7 #b0000000 :opcode #b0110011))
            ("addi" . (:type :I :funct3 #b000 :opcode #b0010011))
            ("andi" . (:type :I :funct3 #b111 :opcode #b0010011))
            ("ori"  . (:type :I :funct3 #b110 :opcode #b0010011))
            ("xori" . (:type :I :funct3 #b100 :opcode #b0010011))
            ("slti" . (:type :I :funct3 #b010 :opcode #b0010011))
            ("sltiu". (:type :I :funct3 #b011 :opcode #b0010011))
            ("slli" . (:type :I :funct3 #b001 :opcode #b0010011))
            ("srli" . (:type :I :funct3 #b101 :opcode #b0010011))
            ("srai" . (:type :I :funct3 #b101 :opcode #b0010011))
            ("lw"   . (:type :I :funct3 #b010 :opcode #b0000011))
            ("lh"   . (:type :I :funct3 #b001 :opcode #b0000011))
            ("lb"   . (:type :I :funct3 #b000 :opcode #b0000011))
            ("lhu"  . (:type :I :funct3 #b100 :opcode #b0000011))
            ("lbu"  . (:type :I :funct3 #b101 :opcode #b0000011))
            ("jalr" . (:type :I :funct3 #b000 :opcode #b1100111))
            ("sw"   . (:type :S :funct3 #b010 :opcode #b0100011))
            ("sh"   . (:type :S :funct3 #b001 :opcode #b0100011))
            ("sb"   . (:type :S :funct3 #b000 :opcode #b0100011))
            ("beq"  . (:type :B :funct3 #b000 :opcode #b1100011))
            ("bne"  . (:type :B :funct3 #b001 :opcode #b1100011))
            ("blt"  . (:type :B :funct3 #b100 :opcode #b1100011))
            ("bge"  . (:type :B :funct3 #b101 :opcode #b1100011))
            ("bltu" . (:type :B :funct3 #b110 :opcode #b1100011))
            ("bgeu" . (:type :B :funct3 #b111 :opcode #b1100011))
            ("lui"  . (:type :U :opcode #b0110111))
            ("auipc". (:type :U :opcode #b0010111))
            ("jal"  . (:type :J :opcode #b1101111))
            ("ecall" . (:type :ECALL :opcode #b1110011)))
          do (setf (gethash name ht) info))
    ht))

(defun get-opcode-binary (op)
  (gethash op *instruction-table*))


(defun encode-r (funct7 rs2 rs1 funct3 rd opcode)
  (logior 
         (ash funct7 25)
         (ash rs2    20)
         (ash rs1    15)
         (ash funct3 12)
         (ash rd      7)
         opcode))

(defun encode-i (imm rs1 funct3 rd opcode)
  (logior 
         (ash imm 20)
         (ash rs1 15)
         (ash funct3 12)
         (ash rd 7)
         opcode))


(defun encode-s (imm rs2 rs1 funct3 opcode)
  (let ((imm-high (ash imm -5))
        (imm-low (logand imm #x1F)))
    (logior 
         (ash imm-high 25)
         (ash rs2 20)
         (ash rs1 15)
         (ash funct3 12)
         (ash imm-low 7)
         opcode)))


(defun encode-b (imm rs2 rs1 funct3 opcode)
  (let ((imm12 (logand (ash imm -12) #x1))
        (imm11 (logand (ash imm -11) #x1))
        (imm10_5 (logand (ash imm -5) #x3F))
        (imm4_1 (logand (ash imm -1) #xF)))
    (logior 
         (ash imm12 31)
         (ash imm10_5 25)
         (ash rs2 20)
         (ash rs1 15)
         (ash funct3 12)
         (ash imm4_1 8)
         (ash imm11 7)
         opcode)))

(defun encode-u (imm rd opcode)
  (logior 
         (ash imm 12)
         (ash rd 7)
         opcode))

(defun encode-j (imm rd opcode)
  (let ((imm20    (logand (ash imm -20) #x1))
        (imm19_12 (logand (ash imm -12) #xFF))
        (imm11    (logand (ash imm -11) #x1))
        (imm10_1  (logand (ash imm -1) #x3FF)))
    (logior 
         (ash imm20 31)
         (ash imm10_1 21)
         (ash imm11 20)
         (ash imm19_12 12)
         (ash rd 7)
         opcode)))



  
(defmacro define-r-inst (op funct3 funct7)
  `(defun ,(intern (format nil "ENCODE-~A" (string-upcase op))) (rd rs1 rs2)
     (encode-r ,funct7 rs2 rs1 ,funct3 rd #b0110011)))

(defmacro define-i-inst (op funct3)
  `(defun ,(intern (format nil "ENCODE-~A" (string-upcase op))) (rd rs1 imm)
     (encode-i imm rs1 ,funct3 rd #b0010011)))

(defmacro define-s-inst (op funct3)
  `(defun ,(intern (format nil "ENCODE-~A" (string-upcase op))) (rs2 rs1 imm)
     (encode-s imm rs2 rs1 ,funct3 #b0100011)))

(defmacro define-b-inst (op funct3)
  `(defun ,(intern (format nil "ENCODE-~A" (string-upcase op))) (rs1 rs2 imm)
     (encode-b imm rs2 rs1 ,funct3 #b1100011)))

(defmacro define-u-inst (op opcode)
  `(defun ,(intern (format nil "ENCODE-~A" (string-upcase op))) (rd imm)
     (encode-u imm rd ,opcode)))

(defmacro define-j-inst (op opcode)
  `(defun ,(intern (format nil "ENCODE-~A" (string-upcase op))) (rd imm)
     (encode-j imm rd ,opcode)))




;; R-type
(define-r-inst "add"  #b000 #b0000000)
(define-r-inst "sub"  #b000 #b0100000)
(define-r-inst "and"  #b111 #b0000000)
(define-r-inst "or"   #b110 #b0000000)
(define-r-inst "xor"  #b100 #b0000000)
(define-r-inst "sll"  #b001 #b0000000)
(define-r-inst "srl"  #b101 #b0000000)
(define-r-inst "sra"  #b101 #b0100000)
(define-r-inst "slt"  #b010 #b0000000)
(define-r-inst "sltu" #b011 #b0000000)

;; I-type
(define-i-inst "addi"  #b000)
(define-i-inst "andi"  #b111)
(define-i-inst "ori"   #b110)
(define-i-inst "xori"  #b100)
(define-i-inst "slti"  #b010)
(define-i-inst "sltiu" #b011)

;; S-type
(define-s-inst "sw" #b010)
(define-s-inst "sh" #b001)
(define-s-inst "sb" #b000)

;; B-type
(define-b-inst "beq"  #b000)
(define-b-inst "bne"  #b001)
(define-b-inst "blt"  #b100)
(define-b-inst "bge"  #b101)
(define-b-inst "bltu" #b110)
(define-b-inst "bgeu" #b111)

;; U-type
(define-u-inst "lui"   #b0110111)
(define-u-inst "auipc" #b0010111)

;; J-type
(define-j-inst "jal" #b1101111)
