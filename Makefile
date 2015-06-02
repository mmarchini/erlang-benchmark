all: quickSort parallelQuickSort 
	@echo "Everything Done!"

# QuickSort #
#############

quickSort: quickSort_compile quickSort_run quickSort_gatherData
	@echo "QuickSort Done!"

quickSort_compile: src/quickSort.erl 
	@mkdir -p build/quickSort
	@erlc -o build/quickSort src/quickSort.erl 

quickSort_run:
	@cd build/quickSort; erl -noshell -s quickSort test_loop 10 10 100 -s init stop

quickSort_gatherData:
	@mkdir -p results/
	@sed -ne 's/Time: \(.*\)/\1/p' build/quickSort/*.log > results/quickSort.csv

# ParallelQuickSort #
#####################

parallelQuickSort: parallelQuickSort_compile parallelQuickSort_run parallelQuickSort_gatherData
	@echo "Parallel QuickSort Done!"

parallelQuickSort_compile: src/parallelQuickSort.erl 
	@mkdir -p build/parallelQuickSort
	@erlc -o build/parallelQuickSort src/parallelQuickSort.erl 

parallelQuickSort_run:
	@cd build/parallelQuickSort; erl -noshell -s parallelQuickSort test_loop 10 10 100 -s init stop

parallelQuickSort_gatherData:
	@mkdir -p results/
	@sed -ne 's/Time: \(.*\)/\1/p' build/parallelQuickSort/*.log > results/parallelQuickSort.csv
