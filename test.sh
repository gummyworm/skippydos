# compile and test jiffy replacement.
# use the command line argument "NTSC" to test on an NTSC vic-20
# otherwise this code will run on an emulated PAL machine
cl65 -t none --start-addr 0xe000 kernal.asm -o jiffy.bin
read
if [ $1 "ntsc" ]
then
    xvic -default -ntsc -memory all -kernal jiffy.bin -dos1541 JiffyDOS_C1541.bin -truedrive -8 TGA_v7.d64
        
else
    xvic -default -pal -memory all -kernal jiffy.bin -dos1541 JiffyDOS_C1541.bin -truedrive -8 TGA_v7.d64

