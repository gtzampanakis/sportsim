all: $(patsubst %.scm,%.scm.go,$(wildcard *.scm))

clean:
	find . -name '*.scm.go' -delete

%.scm.go: %.scm
	guild compile -L . $< -o $@

run: all
	guile --no-auto-compile -L . main.scm

test: all
	guile --no-auto-compile -L . test-runner.scm
