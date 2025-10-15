rem export PATH=$PATH:/home/max/work/Embedded/RISC-V/riscv/bin/
set PATH="C:\SysGCC\risc-v\bin";C:\SysGCC\risc-v\libexec\gcc\riscv64-unknown-elf\10.1.0;%PATH%
rem riscv64-unknown-elf-readelf -W -h forth.elf
riscv64-unknown-elf-readelf -S -W  forth.elf