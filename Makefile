# compile and test jiffy replacement.
# use the command line argument "NTSC" to test on an NTSC vic-20
# otherwise this code will run on an emulated PAL machine
all: 
	cl65 -t none --start-addr 0xe000 kernal.asm -o jiffy.bin
	cl65 -t none boot.asm irq.asm -o boot -Ln labels.txt
	c1541 -format "boot,boot" d64 boot.d64 -write boot.prg 0

jiffy:
	xvic -default -pal -memory all -kernal jiffy.bin -dos1541 JiffyDOS_C1541.bin -truedrive -8 boot.d64
test:
	xvic -default -pal -memory all -kernal jiffy.bin -cartA boot -8 minipaint.d64
ntsc:
	xvic -default -ntsc -memory all -kernal jiffy.bin -dos1541 JiffyDOS_C1541.bin -truedrive -8 boot.d64
