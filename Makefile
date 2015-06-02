all: compile run_tests
	@echo "Done!"

compile: src/quickSort.erl 
	@mkdir -p build/
	@erlc -o build/ src/quickSort.erl 

run_tests:
	@cd build/; erl -noshell -s quickSort test_loop 10, 10, 10 -s init stop
