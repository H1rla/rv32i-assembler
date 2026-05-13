; R-type
add x1, x2, x3
sub x4, x5, x6
and x7, x8, x9
or x10, x11, x12
xor x13, x14, x15
sll x1, x2, x3
srl x4, x5, x6
sra x7, x8, x9
slt x1, x2, x3
sltu x4, x5, x6

; I-type 即値演算
addi x1, x2, 10
addi x1, x2, -1
andi x1, x2, 255
ori x1, x2, 15
xori x1, x2, 7
slti x1, x2, 5
sltiu x1, x2, 5

; I-type ロード
lw x1, 4(x2)
lh x1, 2(x2)
lb x1, 1(x2)
lhu x1, 2(x2)
lbu x1, 1(x2)

; S-type
sw x2, 8(x1)
sh x2, 4(x1)
sb x2, 1(x1)

; B-type
beq x1, x2, 16
bne x1, x2, 16
blt x1, x2, 16
bge x1, x2, 16
bltu x1, x2, 16
bgeu x1, x2, 16

; U-type
lui x1, 4660
auipc x1, 4660

; J-type
jal x1, 100
jalr x1, x2, 4
