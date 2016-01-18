# External programs:
ASM = Merlin32.exe -V ~/Merlin32_v1.0/Merlin32_v1.0//Library/ 
AC = java -jar "C:\Users\Rob\From DOODLE4\Java Applications\AppleCommander releases\AppleCommander-1.3.5.14-ac.jar"

# Local stuff:
PGM = afscanner
SYS = afscannr.system
TYPE = SYS
ADDR = 0x2000
TMPL = template.po
DISK = dev.po


build:
	$(ASM) $(PGM).s
	cp $(TMPL) $(DISK)
	cat $(PGM) | $(AC) -p $(DISK) $(SYS) $(TYPE) $(ADDR)
	$(AC) -k $(DISK) $(SYS)
	$(AC) -ll $(DISK)
	zip $(PGM).zip $(DISK)
