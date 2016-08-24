
all: clean bitstream-vera


BITSTREAM_VERA_VERSION = `cat bitstream-vera-VERSION`
BITSTREAM_VERA_FILE_BASE = ttf-bitstream-vera-$(BITSTREAM_VERA_VERSION)
BITSTREAM_VERA_URL_BASE = http://ftp.gnome.org/pub/GNOME/sources/ttf-bitstream-vera/
BITSTREAM_VERA_URL = $(BITSTREAM_VERA_URL_BASE)$(BITSTREAM_VERA_VERSION)/$(BITSTREAM_VERA_FILE_BASE).tar.bz2

bitstream-vera:
	wget $(BITSTREAM_VERA_URL)
	mkdir bitstream-vera-fonts
	tar xjvf $(BITSTREAM_VERA_FILE_BASE).tar.bz2 -C bitstream-vera-fonts --strip-components=1
	rm $(BITSTREAM_VERA_FILE_BASE).tar.bz2
	mv bitstream-vera-fonts/COPYRIGHT.TXT bitstream-vera-LICENCE
	webify bitstream-vera-fonts/*.ttf


clean:
	find . ! -name . ! -name .. ! \
		-name README.md ! \
		-name Makefile ! \
		-name bitstream-vera-VERSION | \
		xargs rm -rf
