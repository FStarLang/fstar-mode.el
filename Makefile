EMACS ?= emacs
EL := fstar-mode.el
ELC := $(EL:.el=.elc)
PACKAGE_DIR := $(shell cask package-directory)
ERROR_ON_WARN ?= t

$(ELC):%.elc:%.el
	cask exec ${EMACS} -L . -Q --batch \
        --eval "(setq byte-compile-error-on-warn ${ERROR_ON_WARN})" \
        -f batch-byte-compile $<

$(PACKAGE_DIR): Cask
	cask install
	touch $(PACKAGE_DIR)

build: $(PACKAGE_DIR) $(ELC)
