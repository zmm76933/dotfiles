# -*- mode: makefile -*-
EMACS	?= emacs
EL		= init-ddskk.el
EL		+= early-init.el
ELC		= $(EL:%.el=%.elc)

all: $(ELC) init.elc
$(EL): init.el
init.el:
	@mkdir -p ~/.cache/emacs
	@if [ ! -d ~/.cache/emacs/eln-cache ]; then \
		echo ";; mkdir ~/.cache/emacs/eln-cache"; mkdir ~/.cache/emacs/eln-cache ;\
	fi
	@if [ ! -L eln-cache ]; then \
		echo ";; ln -sf ~/.cache/emacs/eln-cache . "; ln -sf ~/.cache/emacs/eln-cache . ;\
	fi
	@mkdir -p elpa
	@mkdir -p el-get
	@mkdir -p share
	$(EMACS) -q -l init.el --batch --eval '(kill-emacs)'
%.elc: %.el
	$(EMACS) -q -l init.el -batch -f batch-byte-compile $<
#	@rm -f $<

clean:
	rm -fr auto-save-list *.elc *~

distclean: clean
	rm -fr elpa
	rm -fr el-get
	rm -f eln-cache

skk-jisyo:
	mkdir -p ~/.cache/emacs/skk-jisyo
	wget "https://github.com/skk-dev/dict/raw/master/SKK-JISYO.L" \
		-O ~/.cache/emacs/skk-jisyo/SKK-JISYO.L
