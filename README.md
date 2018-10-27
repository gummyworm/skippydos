# Skippydos
40 column KERNAL for the Vic 20 with JiffyDOS support

The original incarnation of this project was developed from June 6th 2011 to 
September 21st 2012. 


## Features
 * Holding the Commodore key upon boot will  prevent autostart cartridges from starting.
   Holding the CTRL key upon boot will force initialization of BASIC pointers and VIC registers to the values of an unexpanded machine.

## Details
SkippyDOS consists of two parts: a KERNAL replacement and a cartridge.
The KERNAL replaces the builtin KERNAL and resides, like any Vic 20 KERNAL, at 
address $e000. The cartridge contains the 40 column font as well as some utility
methods to render to the bitmap.
Because a bitmap is used to render the screen (by necessity, the VIC is 
incapable of generating 4x8 characters), a RAM expansion is required to run
this KERNAL. The VIC can only see internal RAM, so $1000-$2000 is reserved
for this purpose.  This leaves the available RAM for the user to the space
between $2000 and the utility cartridge at $a000.

| Address    | Function |
|------------|----------|
|$1000-$2000 | bitmap   |
|$2000-$a000 | user RAM |
|$a000-$c000 | OS cart  |
|$c000-$e000 | BASIC    |
|$e000-$ffff | KERNAL   |
