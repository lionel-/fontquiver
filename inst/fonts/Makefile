
all: clean bitstream-vera
large: clean bitstream-vera dejavu


BITSTREAM_VERA_VERSION= `cat bitstream-vera-VERSION`
BITSTREAM_VERA_FILE= ttf-bitstream-vera-$(BITSTREAM_VERA_VERSION).tar.bz2
BITSTREAM_VERA_URL= http://ftp.gnome.org/pub/GNOME/sources/ttf-bitstream-vera/$(BITSTREAM_VERA_VERSION)/$(BITSTREAM_VERA_FILE)

bitstream-vera:
	wget $(BITSTREAM_VERA_URL)
	mkdir bitstream-vera-fonts
	tar xjvf $(BITSTREAM_VERA_FILE) -C bitstream-vera-fonts --strip-components=1
	rm $(BITSTREAM_VERA_FILE)
	cp bitstream-vera-fonts/COPYRIGHT.TXT bitstream-vera-LICENCE
	webify --no-eot --no-svg bitstream-vera-fonts/*.ttf


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
