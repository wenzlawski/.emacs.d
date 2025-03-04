all:
	cargo build
	emacsclient -e '(fake-module-reload (dir-concat user-emacs-directory "site-lisp/ssms/target/debug/libssms_core.so"))'
