ASM = Merlin32.exe -V ~/Merlin32_v1.0/Merlin32_v1.0//Library/ 
AC = java -jar "C:\Users\Rob\From DOODLE4\Java Applications\AppleCommander releases\AppleCommander-1.3.5.14-ac.jar"



build:
	$(ASM) afscanner.s
	$(AC) -pro140 dev.po DEV
	cat afscanner | $(AC) -p dev.po afscanner BIN 0x8000
