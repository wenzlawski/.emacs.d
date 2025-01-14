all:
	cargo build
	emacsclient -e '(fake-module-reload (dir-concat user-emacs-directory "site-lisp/echm/target/debug/echm-core.so"))'
