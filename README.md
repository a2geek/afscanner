# Apple Disk II address field scanner

This application is my lame attempt to understand the old Apple Disk II interface.  This included writing the read, decode, and physical routines to move the disk arm.

## Screen Shots

The initial page of the application will be the About Page.  This is primarily to give the user a chance to swap disks but maybe lets them read up on what is available.  (New as of V2.0)

![About Page](images/AboutPage.png)

For close to normal disks, the Address Field Scanner can be used to spot-check which sectors are available on the track.  If this pages displays empty, you may want to peruse the raw disk data on the Browse page.  (New as of V1.0)

![Address Field Page](images/AddressFieldPage.png)

The Browse Buffer Page displays the disk-level bytes.  If the standard address field header is located, those bytes will be hilighted to make them stand out.  (New as of V2.0)

![Browse Buffer Page](images/BrowseBufferPage.png)

The Graphical Disk Display is a rudimentary component to give a 'picture' of the disk.  This counts sync bytes (generally $FF); note that timing is not considered so these may be data bytes as well (8 bit versus 10 bits on disk).

![Graphical Disk Display](images/GraphicalDiskDisplay.png)

The Buffer Count Page totals the number of each type of "disk byte" in the buffer for easy reference.

![Buffer Count Page](images/BufferCountPage.png)

## Notes
* `Makefile` is very rudimentary as I am not a Makefile type person and build is so fast it doesn't matter
* Pay attention to versions
* `Makefile` makes assumptions about where things are placed, correct as necessary

## Current setup(s)

### Windows

> Please note that this mixes the Cygin Unix-y path flavor with the Windows path flavor.  Java (as it is a Windows application) requires Windows pathing instead of Unix pathing.  See the `Makefile` configuration section.

* Cygwin 64-bit
 * git
 * gcc (to compile Merlin32)
 * make
* Java (Windows version)
* AppleCommander 
* Merlin32

### Mac OS X

* Java
* AppleCommander
* Text editor
* Merlin32

## Software required to build:
* [Merlin32](http://www.brutaldeluxe.fr/products/crossdevtools/merlin/index.html) by Brutal Deluxe (version 1.0)
* The command-line variant of [AppleCommander](https://sites.google.com/site/drjohnbmatthews/applecommander) (please note this project is using 1.3.5.14-ac and that the command-line options do vary across versions)
* Either a real Apple II or an emulator
