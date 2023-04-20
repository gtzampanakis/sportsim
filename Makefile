all:\
	compiled/tabdef.scm.go\
	compiled/conf.scm.go\
	compiled/db.scm.go\
	compiled/lib/util.scm.go\
	compiled/main.scm.go\
	compiled/test-runner.scm.go\
	compiled/tests/test-date.scm.go\
	compiled/tests/test-util.scm.go

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

compiled/main.scm.go: main.scm
	guild compile -L . main.scm -o compiled/main.scm.go

compiled/test-runner.scm.go: test-runner.scm
	guild compile -L . test-runner.scm -o compiled/test-runner.scm.go

compiled/tests/test-date.scm.go: tests/test-date.scm
	guild compile -L . tests/test-date.scm -o compiled/tests/test-date.scm.go

compiled/tests/test-util.scm.go: tests/test-util.scm
	guild compile -L . tests/test-util.scm -o compiled/tests/test-util.scm.go

run: all
	guile --no-auto-compile -L . main.scm

run-tests: all
	guile --no-auto-compile -L . test-runner.scm
