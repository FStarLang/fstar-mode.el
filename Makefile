EMACS ?= emacs
EL := fstar-mode.el
ELC := $(EL:.el=.elc)
PACKAGE_DIR := $(shell cask package-directory)
ERROR_ON_WARN ?= t
SANDBOX ?= sandbox

.PHONY: sandbox

$(ELC):%.elc:%.el
	cask exec ${EMACS} -L . -Q --batch \
        --eval "(setq byte-compile-error-on-warn ${ERROR_ON_WARN})" \
        -f batch-byte-compile $<

$(PACKAGE_DIR): Cask
	cask install
	touch $(PACKAGE_DIR)

build: $(PACKAGE_DIR) $(ELC)

sandbox:
	mkdir -p sandbox
	$(EMACS) -Q --eval '(setq user-emacs-directory "$(SANDBOX)")' -l package \
				--eval "(add-to-list 'package-archives '(\"gnu\" . \"http://elpa.gnu.org/packages/\") t)" \
				--eval "(add-to-list 'package-archives '(\"melpa\" . \"https://melpa.org/packages/\") t)" \
				--eval "(package-refresh-contents)" --eval "(package-initialize)" \
				--eval "(package-install-file \"fstar-mode.el\")"

clean:
	cask clean-elc
