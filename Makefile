# Compile every module.
build:
	./mill __.compile

# Compile and run the test suite through fume (the release pinned in etc/tools; `make tools`
# installs it), which discovers the suites from the index the beneficence plugin writes; a
# probably suite has no main class of its own. CI runs the same command.
test:
	./mill pyrocosm.test.assembly
	fume run -c out/pyrocosm/test/assembly.dest/out.jar $(TESTS)

# The gallery as an Ethereal executable, packaged by the pinned `xeq` builder script (fetched
# into dist/xeq and verified against etc/xeq.tsv) exactly as fume, flame and flair are; then
# run interactively in the terminal, or once, statically, at a width.
gallery: xeq-fetch
	./mill pyrocosm.demo.assembly
	dist/xeq build --jar out/pyrocosm/demo/assembly.dest/out.jar --out gallery

# Fetch the pinned `xeq` builder script into dist/xeq.
xeq-fetch:
	./etc/shared xeq-fetch.sh

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
# tree of the commit, at version `<pyrocosmVersion>-<hex>` — for a dependent repository to pin in
# its etc/refs before the next release. `LOCAL=1` stages and installs without publishing.
# The last line printed is the pin. See snapshot.sh in propensive/.github.
snapshot:
	./etc/shared snapshot.sh pyrocosm "$$(sed -n 's/.*val pyrocosmVersion = "\(.*\)".*/\1/p' build.mill)"

# Delete snapshot pre-releases older than DAYS (default 60) days.
snapshot-prune:
	./etc/shared snapshot-prune.sh pyrocosm $(DAYS)

# Releases are cut by tagging, not by make. Bump `pyrocosmVersion`, merge it, and then `git tag -s
# X.Y.Z && git push --tags`: the tag fires .github/workflows/release.yml, which runs the shared
# release.sh in propensive/.github. That gates on a signed tag, on CI already being green on that
# very commit, and on every pin being a release; then uploads the jars into a draft, checks every
# asset's digest against the local file, and only then makes the release visible. If anything
# fails, the release and the tag are both deleted. What this repository needs beyond the common
# path is declared in etc/release. This target survives only to say so.
release:
	@echo "Releases are triggered by tags, not by make. Bump pyrocosmVersion, merge it, then:" >&2
	@echo "" >&2
	@echo "    git tag -s X.Y.Z && git push --tags" >&2
	@echo "" >&2
	@echo "See propensive/.github." >&2
	@exit 1

dev:
	./mill -w pyrocosm.model.compile

.PHONY: check build test gallery xeq-fetch demo static serve publishLocal stage sync-deps tools snapshot snapshot-prune release dev
