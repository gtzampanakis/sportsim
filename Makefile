all:\
	compiled/tabdef.scm.go\
	compiled/conf.scm.go\
	compiled/db.scm.go\
	compiled/lib/util.scm.go\
	compiled/test-runner.scm.go\
	compiled/tests/test-db.scm.go\
	compiled/tests/test-date.scm.go\
	compiled/tests/test-util.scm.go\
	compiled/tests/test-bst.scm.go

clean:
	rm -rf compiled/*

compiled/tabdef.scm.go: tabdef.scm
	guild compile -L . tabdef.scm -o compiled/tabdef.scm.go

compiled/conf.scm.go: conf.scm
	guild compile -L . conf.scm -o compiled/conf.scm.go

compiled/db.scm.go: db.scm
	guild compile -L . db.scm -o compiled/db.scm.go

compiled/lib/util.scm.go: lib/util.scm
	guild compile -L . lib/util.scm -o compiled/lib/util.scm.go

compiled/test-runner.scm.go: test-runner.scm
	guild compile -L . test-runner.scm -o compiled/test-runner.scm.go

compiled/tests/test-db.scm.go: tests/test-db.scm
	guild compile -L . tests/test-db.scm -o compiled/tests/test-db.scm.go

compiled/tests/test-date.scm.go: tests/test-date.scm
	guild compile -L . tests/test-date.scm -o compiled/tests/test-date.scm.go

compiled/tests/test-util.scm.go: tests/test-util.scm
	guild compile -L . tests/test-util.scm -o compiled/tests/test-util.scm.go

compiled/tests/test-bst.scm.go: tests/test-bst.scm
	guild compile -L . tests/test-bst.scm -o compiled/tests/test-bst.scm.go

run: all
	guile --no-auto-compile -L . main.scm

test: all
	guile --no-auto-compile -L . test-runner.scm
