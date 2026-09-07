# Compile every module.
build:
	./mill __.compile

# Compile and run the test suite.
test:
	./mill pyrocosm.test.assembly
	java -cp out/pyrocosm/test/assembly.dest/out.jar pyrocosm.Tests

# Run the gallery in the terminal, or serve it on http://localhost:8080/.
demo:
	./mill pyrocosm.demo.assembly
	java -jar out/pyrocosm/demo/assembly.dest/out.jar terminal

serve:
	./mill pyrocosm.demo.assembly
	java -jar out/pyrocosm/demo/assembly.dest/out.jar serve

# Publish the libraries to the local ~/.ivy2, for fume/flame to build against.
publishLocal:
	./mill pyrocosm.__.publishLocal

dev:
	./mill -w pyrocosm.model.compile

.PHONY: build test demo serve publishLocal dev
