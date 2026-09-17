# Compile every module.
build:
	./mill __.compile

# Compile and run the test suite through fume (the release pinned in etc/tools; `make tools`
# installs it), which discovers the suites from the index the beneficence plugin writes; a
# probably suite has no main class of its own. CI runs the same command.
test:
	./mill pyrocosm.test.assembly
	fume run -c out/pyrocosm/test/assembly.dest/out.jar $(TESTS)

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

# Publish the libraries to the local ~/.ivy2, for fume/flame to build against a version that is
# not yet released.
publishLocal:
	./mill pyrocosm.__.publishLocal

# Stage the release jars (each with its POM and ivy.xml embedded) without publishing them.
stage:
	./mill release.stage

# Install every library pinned in etc/refs — releases and snapshots alike, transitively —
# into the local ivy repository, as CI does, so the build resolves exactly the pinned jars rather
# than whatever a sibling checkout's `publishLocal` last installed under the same version. A
# snapshot not yet on GitHub is built from the sibling checkout named by the pin's commit.
sync-deps:
	./etc/shared sync-deps.sh

# Install the commands pinned in etc/tools (fume) through their releases' installers.
tools:
	./etc/shared tools.sh

# Publish HEAD's libraries as a snapshot — a `snapshot-<hex>` pre-release named by the filtered
# tree of the commit, at version `<pyrocosmVersion>-<hex>` — for a dependent repository to pin in
# its etc/refs before the next release. `LOCAL=1` stages and installs without publishing.
# The last line printed is the pin. See snapshot.sh in propensive/.github.
snapshot:
	./etc/shared snapshot.sh pyrocosm "$$(sed -n 's/.*val pyrocosmVersion = "\(.*\)".*/\1/p' build.mill)"

# Delete snapshot pre-releases older than DAYS (default 60) days.
snapshot-prune:
	./etc/shared snapshot-prune.sh pyrocosm $(DAYS)

# Release to GitHub Releases: `make release VERSION=X.Y.Z`, after bumping `pyrocosmVersion` in
# build.mill and committing. See etc/ci/release.sh.
release:
	@if [ -z "$(VERSION)" ]; then echo "Usage: make release VERSION=X.Y.Z" >&2; exit 1; fi
	./etc/ci/release.sh "$(VERSION)"

dev:
	./mill -w pyrocosm.model.compile

.PHONY: build test gallery demo static serve publishLocal stage sync-deps tools snapshot snapshot-prune release dev
