# External dependencies:
include local.config

# Local stuff:
SRC = afscanner.s diskii.s page-about.s page-browse.s page-count.s page-field.s page-graphics.s page-test.s page.s util.s 
PGM = afscanner
SYS = afscannr.system
TYPE = SYS
ADDR = 0x2000
TMPL = template.po
DISK = afdisk.po


afscanner: $(SRC)
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

