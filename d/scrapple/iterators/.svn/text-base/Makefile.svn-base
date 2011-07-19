all: tests big sample iterators.html


iterators.html: iterators.d
	dmd -w -D -o- iterators.d
	chmod +r ./iterators.html

big: tests.d iterators.d
	dmd -w -O -inline -release -debug -ofbig iterators.d big.d

tests: tests.d iterators.d
	dmd -w -O -debug -oftests iterators.d tests.d

sample: sample.d iterators.d
	dmd -w -O -debug -ofsample iterators.d sample.d

clean:
	rm -f sample sample.o
	rm -f big big.o
	rm -f tests tests.o
	rm -f iterators.o
