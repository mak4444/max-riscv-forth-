rem https://github.com/riscv-collab/riscv-gnu-toolchain/releases/tag/2024.04.12
rem Ctrl-a C q - quit
set PATH="C:\Program Files\qemu";%PATH%
qemu-system-riscv32 -nographic -serial mon:stdio -machine virt -bios forth.elf