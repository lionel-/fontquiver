
all: clean
large: clean bitstream-vera dejavu


DEJAVU_VERSION= `cat dejavu-VERSION`
DEJAVU_FILE= dejavu-fonts-ttf-$(DEJAVU_VERSION).tar.bz2
DEJAVU_URL= http://sourceforge.net/projects/dejavu/files/dejavu/$(DEJAVU_VERSION)/$(DEJAVU_FILE)

dejavu:
	wget $(DEJAVU_URL)
	mkdir dejavu-fonts
	tar xjvf $(DEJAVU_FILE) -C dejavu-fonts --strip-components=1
	rm $(DEJAVU_FILE)
	cp dejavu-fonts/LICENSE dejavu-LICENCE
	webify --no-eot --no-svg dejavu-fonts/ttf/*.ttf

clean:
	find . \
		! -name "." ! -name ".." \
		! -name "README.md" \
		! -name "Makefile" \
		! -name "*-VERSION" | \
		xargs rm -rf
