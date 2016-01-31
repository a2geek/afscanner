# External dependencies:
include local.config

# Local stuff:
PGM = afscanner
SYS = afscannr.system
TYPE = SYS
ADDR = 0x2000
TMPL = template.po
DISK = afdisk.po


afscanner: afscanner.s
	$(ASM) $(PGM).s
	cp $(TMPL) $(DISK)
	cat $(PGM) | $(AC) -p $(DISK) $(SYS) $(TYPE) $(ADDR)
	$(AC) -k $(DISK) $(SYS)
	$(AC) -ll $(DISK)
	zip $(PGM).zip $(DISK)

test:	afscanner
	$(EMUL) $(DISK)

clean:
	rm $(PGM) $(DISK) $(PGM).zip
	rm _FileInformation.txt afscanner_Output.txt

