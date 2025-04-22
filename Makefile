MOD_NAME = dot

.PHONY: all
all: ./var/deps ~/.emacs ~/.emacs.d/custom.el ~/.bin/ex ~/.bin/EmacsX

.PHONY: nuke
nuke: ~/.emacs.d
	rm -rf ~/.emacs.d ./var/deps ~/.emacs ~/.bin/EmacsX

.PHONY: clean
clean: ./var/deps ~/.emacs
	rm -rf ./var/deps ~/.emacs

./var/deps:
	mkdir -p $@
	MOD_NAME=${MOD_NAME} ./bin/compile $@

~/.emacs: ./src/head.el ./src/tail.el ./src/mods ./etc/mods.list ./etc/mods.list.d/base-mods.list ./etc/mods.list.d/prog-mods.list
	MOD_NAME=${MOD_NAME} ./bin/install $@

~/.emacs.d:
	mkdir -p $@

~/.emacs.d/custom.el: ~/.emacs.d
	touch $@

~/.bin:
	mkdir -p $@

~/.bin/ex: ~/.bin
	ln -nfs ${PWD}/bin/wrapper/start-emacs.sh $@

~/.bin/EmacsX: ~/.bin
	ln -nfs ${PWD}/bin/wrapper/start-emacs.sh $@
