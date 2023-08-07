all: $(patsubst %.scm,%.scm.go,$(wildcard *.scm))

clean:
	find . -name '*.scm.go' -delete

%.scm.go: %.scm
	guild compile -L . $< -o $@

run:
	guile -L . main.scm

test:
	guile -L . test-runner.scm
