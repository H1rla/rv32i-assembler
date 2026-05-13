(asdf:defsystem "rv32i-assembler"
  :description "RV32I assembler in Common Lisp"
  :version "0.1"
  :components((:file "package")
              (:module "src"
               :depends-on ("package")
               :components((:file "utils")
                           (:file "parser"  :depends-on ("utils"))
                           (:file "encoder")
                           (:file "assembler" :depends-on ("parser" "encoder"))))))

