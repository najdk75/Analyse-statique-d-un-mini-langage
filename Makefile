.PHONY: reports all test

# Par defaut on ne fait que compiler, pas tester
all: binary

test: tests.report.svg

CRAM=$(shell find "tests" -name "*.t")
SUITES=$(shell find "tests" -type d | grep 'tests/' | sed -e 's/$$/.report.svg/')
REPORTS=$(shell find "tests" -type d | grep 'tests/'| sed -e 's/$$/.report/')

tests/%.report: polish.ml $(CRAM)
	dune test "tests/$*" 2> $@ || true

tests/%.report.svg: tests/%.report tests/generic_badge.svg
	./buildbadge tests/$* > $@

tests.report: $(REPORTS) $(SUITES)
	cat $(REPORTS) > tests.report

tests.report.svg: tests.report
	./buildbadge tests > tests.report.svg

binary:
	dune build polish.exe

byte:
	dune build polish.bc

clean:
	dune clean
	-rm -f $(SUITES)
	-rm -f $(REPORTS)
