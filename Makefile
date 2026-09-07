# Compile every module.
build:
	./mill __.compile

# Compile and run the test suite through fume, which discovers the suites from the index the
# beneficence plugin writes; a probably suite has no main class of its own.
test:
	./mill pyrocosm.test.assembly
	fume run

# The gallery as an Ethereal executable (the daemon launcher every Soundness application uses),
# then run interactively in the terminal, or once, statically, at a width.
gallery:
	./mill pyrocosm.demo.assembly
	java -Dbuild.executable=gallery -jar out/pyrocosm/demo/assembly.dest/out.jar

demo: gallery
	./gallery

static: gallery
	./gallery static 100

serve: gallery
	./gallery serve

# Publish the libraries to the local ~/.ivy2, for fume/flame to build against.
publishLocal:
	./mill pyrocosm.__.publishLocal

dev:
	./mill -w pyrocosm.model.compile

.PHONY: build test gallery demo static serve publishLocal dev
