MOD_NAME = dot

.PHONY: all
all: ./var/deps ~/.emacs ~/.emacs.d/custom.el ~/.bin/xemacs ~/.bin/ec

.PHONY: nuke
nuke: ~/.emacs.d
	rm -rf ~/.emacs.d ./var/deps ~/.emacs ~/.bin/xemacs ~/.bin/ec

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

~/.bin/xemacs: ~/.bin
	ln -nfs ${PWD}/bin/wrapper/launch-emacs-standalone $@

~/.bin/ec: ~/.bin
	ln -nfs ${PWD}/bin/wrapper/launch-emacs-clientmode $@
