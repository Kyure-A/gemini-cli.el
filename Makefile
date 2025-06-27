
# Makefile for gemini-cli.el

# Variables
EMACS = emacs -Q --batch
LISP_FILES = gemini-cli.el
CHECKDOC_LISP = -l checkdoc

.PHONY: all compile checkdoc clean

# Targets
all: compile checkdoc

compile: $(LISP_FILES:.el=.elc)

%.elc: %.el
	$(EMACS) -f batch-byte-compile $<

checkdoc:
	$(EMACS) $(CHECKDOC_LISP) -f checkdoc-file $(LISP_FILES)

clean:
	rm -f *.elc
