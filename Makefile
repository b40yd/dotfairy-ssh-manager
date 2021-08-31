CASK ?= cask
EMACS ?= emacs
SOURCES = ssh-manager.el
TARGETS = ssh-manager.elc
TESTS = ssh-manager-test.el

# If the first argument is "test"...
ifeq (test, $(firstword $(MAKECMDGOALS)))
  # use the rest as arguments for "test"
  SELECTOR := $(wordlist 2, $(words $(MAKECMDGOALS)), $(MAKECMDGOALS))
  # ...and turn them into do-nothing targets
  $(eval $(SELECTOR):;@:)
endif

all: clean compile lint

%.elc:%.el
	$(CASK) exec $(EMACS) -Q -batch -L . -f batch-byte-compile $(SOURCES)

compile:$(TARGETS)

lint:
	$(CASK) exec $(EMACS) -Q -batch	\
	--eval "(require 'package)"	\
	--eval "(push '(\"melpa\" . \"https://melpa.org/packages/\") package-archives)"	\
	--eval "(package-initialize)" \
	--eval "(package-refresh-contents)" -l package-lint.el -f package-lint-batch-and-exit $(SOURCES)

test:$(TARGETS)
ifeq ($(SELECTOR),)
	$(CASK) exec $(EMACS) -Q -batch -L . $(addprefix -l , $(TESTS)) -f ert-run-tests-batch-and-exit
else
	$(CASK) exec $(EMACS) -Q -batch -L . $(addprefix -l , $(TESTS)) --eval "(ert-run-tests-batch-and-exit '$(SELECTOR))"
endif

help:
	@echo make
	@echo make compile
	@echo make test [SELECTOR]
	@echo make lint
	@echo make clean

clean:
	@rm -f *.elc

.PHONY: compile
