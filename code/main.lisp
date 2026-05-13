(require "asdf")
(load "/home/hira/quicklisp/setup.lisp")
(load "/home/hira/sec/project/lisp/lisp_mashine/asm_rv32i/code/rv32i-assembler.asd")
(asdf:load-system "rv32i-assembler")

(in-package "RV32I-ASSEMBLER")

(defun main ()
  (let ((args sb-ext:*posix-argv*))
    (let ((input  (second args))
          (output (third args)))
      (format t "Load: ~A~%" input)
      (assemble input output)
      (format t "Done: write ~A~%" output))))

(main)
