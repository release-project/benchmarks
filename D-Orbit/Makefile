## orbit-int Makefile
##
## Author: Patrick Maier <P.Maier@hw.ac.uk>
##

RM=rm
ERLC=erlc
ERLC_OPTS=+debug_info

BASENAMES = $(basename $(wildcard *.erl))
BEAMFILES = $(addsuffix .beam,$(BASENAMES))

.DELETE_ON_ERROR:

.PHONY: all clean emacsclean objclean distclean

all: $(BEAMFILES)

%.beam: %.erl
	$(ERLC) $(ERLC_OPTS) $<

emacsclean:
	$(RM) -f *~

clean: objclean

objclean:
	$(RM) -f *.beam

distclean: objclean emacsclean
