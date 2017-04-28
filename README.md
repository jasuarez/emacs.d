My Emacs configuration file(s).

To use them:

```
git clone --recursive https://github.com/jasuarez/emacs.d ~/.emacs.d
find ~/.emacs.d -name *.elc -exec rm {} \;
emacs -batch -eval '(byte-recompile-directory "~/.emacs.d" 0)'
emacs -batch -u $USER -f package-utils-upgrade-all
