.PHONY: all build seed run test clean

all: build seed

build:
	cobc -x -o bankmain BANKMAIN.cbl
	cobc -x -o seeddata SEEDDATA.cbl

seed: build
	./seeddata

run: seed
	./bankmain

test: clean
	bash test.sh

clean:
	rm -f bankmain seeddata CUSTMSTR.dat ACCTMSTR.dat TXNJRNL.dat RPTFILE.txt ACCTTMP.dat CUSTTMP.dat
