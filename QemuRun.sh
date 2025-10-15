#https://github.com/riscv-collab/riscv-gnu-toolchain/releases/tag/2024.04.12
#sudo apt install qemu-system-misc
qemu-system-riscv32 -nographic -serial mon:stdio -machine virt -bios forth.elf