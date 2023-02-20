BINDIR="$HOME/.local/bin"
"$BINDIR/guild" compile -L . tabdef.scm conf.scm db.scm lib/util.scm main.scm && \
"$BINDIR/guile" -L . main.scm
