#!/bin/bash
# run_tests.sh

ASM_DIR="/home/hira/sec/project/lisp/lisp_mashine/asm_rv32i"
EMU_DIR="/home/hira/sec/project/lisp/lisp_mashine/emu_rv32i"

run_test() {
    local name="$1"
    local src="$2"
    echo "=== $name ==="
    echo "$src" > "$ASM_DIR/input.s"
    (cd "$ASM_DIR" && ./asm.sh) 2>&1
    echo "--- result ---"
    time "$EMU_DIR/emu.sh" "$ASM_DIR/output.bin"
    echo ""
}

# 1. ベンチマーク（ラベルなし・即値でループ）
# addi x1,x0,0 → addi x2,x0,200000 → addi x1,x1,1 → bne x1,x2,-4
# bneの即値：-4バイト（1命令戻る）→ -4
run_test "benchmark (1M instructions)" \
'lui x2, 49
addi x2, x2, -704
addi x1, x0, 0
addi x1, x1, 1
bne x1, x2, -4
addi x17, x0, 10
ecall'
# 2. 命令網羅テスト（分岐なし）
run_test "test_all (no branch)" \
'addi x1, x0, 10
addi x2, x0, 20
add x3, x1, x2
sub x4, x2, x1
and x5, x1, x2
or x6, x1, x2
xor x7, x1, x2
addi x8, x0, 512
sw x3, 0, x8
lw x9, 0, x8
addi x10, x0, 65
addi x17, x0, 11
ecall
addi x17, x0, 10
ecall'

# 3. メモリストレステスト
# sw/lw 100000回ループ
# 命令列: addi x1,x0,0 / addi x2,x0,100000 / addi x3,x0,512
#         sw x1,0,x3 / lw x4,0,x3 / addi x1,x1,1 / bne x1,x2,-12
# bne即値：-12（3命令戻る）

run_test "stress_mem (SW/LW 100k loop)" \
'lui x2, 24
addi x2, x2, 1696
addi x1, x0, 0
addi x3, x0, 512
sw x1, 0, x3
lw x4, 0, x3
addi x1, x1, 1
bne x1, x2, -12
addi x17, x0, 10
ecall'
