ENFER = "bin/enfer"
MODULES = $(patsubst modules/%/, %, $(sort $(dir $(wildcard modules/*/ modules/*/*/))))

all:
	@$(ENFER) refresh

## Shortcuts
a: autoloads
i: install
u: update
U: upgrade
r: autoremove
c: compile
cc: compile-core
cp: compile-plugins
re: recompile
d: doctor

quickstart:
	@$(ENFER) quickstart


## Package management
install:
	@$(ENFER) install
update:
	@$(ENFER) update
autoremove:
	@$(ENFER) autoremove
autoloads:
	@$(ENFER) autoloads
upgrade:
	@$(ENFER) upgrade

## Byte compilation
compile:
	@$(ENFER) compile
compile-core:
	@$(ENFER) compile :core
compile-private:
	@$(ENFER) compile :private
compile-plugins:
	@$(ENFER) compile :plugins
recompile:
	@$(ENFER) recompile
clean:
	@$(ENFER) clean
# compile-module
# compile-module/submodule
$(patsubst %, compile-%, $(MODULES)): | .local/autoloads.el
	@$(ENFER) $@ $(subst compile-, , $@)


## Unit tests
test:
	@$(ENFER) test
test-core:
	@$(ENFER) test :core
# test-module
# test-module/submodule
$(patsubst %, test-%, $(MODULES)):
	@$(ENFER) test $(subst test-, , $@)


## Utility tasks
# Runs Emacs from a different folder than ~/.emacs.d; only use this for testing!
run:
	@$(ENFER) run $(ARGS)
# Prints debug info about your current setup
info:
	@$(ENFER) info

# Diagnoses potential OS/environment issues
doctor:
	@$(ENFER) doctor

.PHONY: all compile test testi clean
