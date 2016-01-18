# Apple Disk II address field scanner

This application is my lame attempt to understand the old Apple Disk II interface.  This included writing the read, decode, and physical routines to move the disk arm.

## Notes
* `Makefile` is very rudimentary as I am not a Makefile type person and build is so fast it doesn't matter
* Pay attention to versions
* `Makefile` makes assumptions about where things are placed, correct as necessary

## Current setup(s)

### Windows

> Please note that this mixes the Cygin Unix-y path flavor with the Windows path flavor.  Java (as it is a Windows application) requires Windows pathing instead of Unix pathing.  See the `Makefile` configuration section.

* Cygwin 64-bit
** git
** gcc (to compile Merlin32)
** make
* Java (Windows version)
* AppleCommander 

## Software required to build:
* [Merlin32](http://www.brutaldeluxe.fr/products/crossdevtools/merlin/index.html) by Brutal Deluxe (version 1.0)
* The command-line variant of [AppleCommander](https://sites.google.com/site/drjohnbmatthews/applecommander) (please note this project is using 1.3.5.14-ac and that the command-line options do vary across versions)
* Either a real Apple II or an emulator
