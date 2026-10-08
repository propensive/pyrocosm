# Compile every module.
build:
	./mill __.compile

# Compile and run the test suite through fume (the release pinned in etc/tools; `make tools`
# installs it), which discovers the suites from the index the beneficence plugin writes; a
# probably suite has no main class of its own. CI runs the same command.
test:
	./mill pyrocosm.test.assembly
	fume run -c out/pyrocosm/test/assembly.dest/out.jar $(TESTS)

# The gallery as an Ethereal executable, packaged by the pinned `xek` builder (fetched into
# dist/xek and verified against etc/xek.tsv) exactly as fume, flame and flair are; then
# run interactively in the terminal, or once, statically, at a width.
gallery: xek-fetch
	./mill pyrocosm.demo.assembly
	dist/xek build --java-min 25 --java 25 out/pyrocosm/demo/assembly.dest/out.jar gallery

# Fetch the pinned `xek` builder into dist/xek.
xek-fetch:
	./etc/shared xek-fetch.sh

demo: gallery
	./gallery

static: gallery
	./gallery static 100

serve: gallery
	./gallery serve

# Two gallery daemons on one machine, each the other's neighbour on the swarm: the second is
# the same executable under another name, so that it has a daemon of its own. The first serves
# in the background on 8081, the second in the foreground on 8082; `make serve-pair-stop`
# ends both.
serve-pair: gallery
	cp gallery dist/gallery2
	./gallery serve 8081 > /dev/null 2>&1 &
	dist/gallery2 serve 8082

serve-pair-stop:
	./gallery quit; dist/gallery2 quit

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

# Check every source against Consequent Style and the project's own rules with flair (the
# release pinned in etc/tools; `make tools` installs it), as configured in
# .pyrocosm/flair/config.tel. Findings are warnings and the count is not yet zero, so CI does
# not run this; PATHS restricts the check to files beneath them.
check:
	flair check $(PATHS)

# Install the commands pinned in etc/tools (fume) through their releases' installers.
tools:
	./etc/shared tools.sh

# Publish HEAD's libraries as a snapshot — a `snapshot-<hex>` pre-release named by the filtered
# tree of the commit, at version `<next version>-<hex>` — for a dependent repository to pin in
# its etc/refs before the next release. `LOCAL=1` stages and installs without publishing.
# The last line printed is the pin. See snapshot.sh in propensive/.github.
snapshot:
	./etc/shared snapshot.sh pyrocosm "$$(./mill show pyrocosm.model.publishVersion | tr -d '"')"

# Delete snapshot pre-releases older than DAYS (default 60) days.
snapshot-prune:
	./etc/shared snapshot-prune.sh pyrocosm $(DAYS)

# Releases are cut by tagging, not by make. Tag a commit CI has passed, with `git tag -s
# X.Y.Z && git push --tags`: the tag fires .github/workflows/release.yml, which runs the shared
# release.sh in propensive/.github. That gates on a signed tag, on CI already being green on that
# very commit, and on every pin being a release; then uploads the jars into a draft, checks every
# asset's digest against the local file, and only then makes the release visible. If anything
# fails, the release and the tag are both deleted. What this repository needs beyond the common
# path is declared in etc/release. This target survives only to say so.
release:
	@echo "Releases are triggered by tags, not by make. Once CI has passed on the commit:" >&2
	@echo "" >&2
	@echo "    git tag -s X.Y.Z && git push --tags" >&2
	@echo "" >&2
	@echo "See propensive/.github." >&2
	@exit 1

dev:
	./mill -w pyrocosm.model.compile

.PHONY: check build test gallery xek-fetch demo static serve publishLocal stage sync-deps tools snapshot snapshot-prune release dev
