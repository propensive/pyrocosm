#!/usr/bin/env bash
#
# Publish a Pyrocosm release to GitHub Releases: the four library jars (`pyrocosm-model`,
# `pyrocosm-compiler`, `pyrocosm-terminal`, `pyrocosm-web`), each embedding its POM and ivy.xml
# so that the jar alone is enough for a consumer. This is the Soundness release procedure,
# without Soundness's attestation and migration notes.
#
# A consumer installs the set into its local ivy repository with Soundness's sync script,
# pointed at this repository:
#
#     SOUNDNESS_RELEASE_REPO=propensive/pyrocosm python3 sync_releases.py X.Y.Z
#
# and, when it repackages a launcher with Burdock, names this repository among the hints
# (`--github propensive/pyrocosm`) so the jars on its classpath are matched to these assets by
# SHA-256 digest and externalized rather than inlined. The released bytes must therefore be the
# bytes a consumer compiled against: `stage` builds from clean, and the digest GitHub reports for
# every asset is checked against the local file before the release is published.
#
# Usage: ./etc/ci/release.sh X.Y.Z   (or `make release VERSION=X.Y.Z`)
# Requires: `gh` authenticated with push access to propensive/pyrocosm.

set -euo pipefail
cd "$(git rev-parse --show-toplevel)"

REPO="${PYROCOSM_RELEASE_REPO:-propensive/pyrocosm}"
VERSION="${1:-}"

if [[ -z "$VERSION" ]]; then
  echo "Usage: $0 X.Y.Z" >&2; exit 1
fi
if ! [[ "$VERSION" =~ ^[0-9]+\.[0-9]+\.[0-9]+$ ]]; then
  echo "release: VERSION must be X.Y.Z (got '$VERSION')" >&2; exit 1
fi
if ! command -v gh >/dev/null 2>&1; then
  echo "release: the GitHub CLI (gh) is required" >&2; exit 1
fi
if ! gh auth status >/dev/null 2>&1; then
  echo "release: gh is not authenticated; run 'gh auth login'" >&2; exit 1
fi
if ! git diff --quiet || ! git diff --cached --quiet; then
  echo "release: working tree is dirty; commit or stash first" >&2; exit 1
fi

# The version is pinned in build.mill and compiled into the POMs the consumers resolve, so it
# must agree with the tag. (The PYROCOSM_VERSION override cannot help: the Mill daemon freezes
# the build script's environment.)
PINNED=$(sed -n 's/.*val pyrocosmVersion = sys.env.getOrElse("PYROCOSM_VERSION", "\(.*\)").*/\1/p' build.mill)
if [[ "$PINNED" != "$VERSION" ]]; then
  echo "release: build.mill pins pyrocosmVersion=$PINNED, not $VERSION; bump and commit first" >&2
  exit 1
fi

HEAD_SHA=$(git rev-parse HEAD)

if git rev-parse "refs/tags/$VERSION" >/dev/null 2>&1; then
  echo "release: tag $VERSION already exists locally" >&2; exit 1
fi
if git ls-remote --exit-code --tags origin "refs/tags/$VERSION" >/dev/null 2>&1; then
  echo "release: tag $VERSION already exists on origin" >&2; exit 1
fi
if gh release view "$VERSION" --repo "$REPO" >/dev/null 2>&1; then
  echo "release: a release named $VERSION already exists on $REPO" >&2; exit 1
fi

drafted=""
tagged=""

rollback() {
  if [[ -n "$drafted" ]]; then
    gh release delete "$VERSION" --repo "$REPO" --yes >/dev/null 2>&1 || true
    echo "release: deleted the draft release $VERSION" >&2
  fi
  if [[ -n "$tagged" ]]; then
    git tag -d "$VERSION" >/dev/null 2>&1 || true
    echo "release: removed local tag $VERSION" >&2
  fi
}

fail() {
  echo "release: $1" >&2
  rollback
  exit 1
}

# Build from clean, so the staged jars are exactly what a consumer resolving this version will
# compile against, then stage them with their descriptors.
./mill clean >/dev/null
if ! ./mill __.compile; then
  fail "compiling failed"
fi
if ! ./mill release.stage; then
  fail "staging the release jars failed"
fi

STAGE_DIR="out/release/stage.dest"
mapfile -t jars < <(find "$STAGE_DIR" -maxdepth 1 -name '*.jar' | sort)
count=${#jars[@]}
if (( count == 0 )); then
  fail "release.stage produced no jars"
fi
for jar in "${jars[@]}"; do
  if [[ "$(basename "$jar")" != *"-$VERSION.jar" ]]; then
    fail "staged jar $(basename "$jar") does not carry version $VERSION"
  fi
done
echo "release: staged $count jars for $VERSION"

# The test suite, against the staged model: a release that fails its own tests is not one.
if ! ./mill pyrocosm.test.assembly; then
  fail "building the test suite failed"
fi
if ! java -cp out/pyrocosm/test/assembly.dest/out.jar pyrocosm.Tests; then
  fail "the test suite failed"
fi

git tag -s "$VERSION" -m "Version $VERSION"
tagged="yes"

notes="Pyrocosm $VERSION.
The four library modules, \`pyrocosm-model\`, \`pyrocosm-compiler\`, \`pyrocosm-terminal\` and \
\`pyrocosm-web\`, each attached as \`<artifactId>-$VERSION.jar\` with its POM and ivy.xml embedded \
under \`META-INF/maven/\`. Install the set into a local ivy repository with Soundness's sync \
script pointed here: \`SOUNDNESS_RELEASE_REPO=propensive/pyrocosm python3 sync_releases.py $VERSION\`. \
A launcher repackaged with Burdock externalizes them given \`--github propensive/pyrocosm\`."

if ! gh release create "$VERSION" --repo "$REPO" --draft --target "$HEAD_SHA" \
       --title "Pyrocosm $VERSION" --notes "$notes" >/dev/null; then
  fail "could not create the draft release"
fi
drafted="yes"

if ! gh release upload "$VERSION" --repo "$REPO" --clobber "${jars[@]}" >/dev/null; then
  fail "uploading the jars failed"
fi

# GitHub computes each asset's SHA-256 shortly after upload. Burdock and the sync script match
# by that digest, so wait for every one and confirm it is the digest of the local file.
for jar in "${jars[@]}"; do
  name=$(basename "$jar")
  local_digest=$(shasum -a 256 "$jar" | cut -d' ' -f1)
  digest=""
  for i in $(seq 1 60); do
    digest=$(gh api "repos/$REPO/releases/tags/$VERSION" \
      --jq ".assets[] | select(.name == \"$name\") | .digest // \"\"" 2>/dev/null || true)
    [[ -n "$digest" ]] && break
    sleep 5
  done
  if [[ "$digest" != "sha256:$local_digest" ]]; then
    fail "released digest '$digest' for $name does not match local sha256:$local_digest"
  fi
done
echo "release: every asset's digest matches"

if ! git push origin "refs/tags/$VERSION"; then
  fail "could not push tag $VERSION"
fi
if ! gh release edit "$VERSION" --repo "$REPO" --draft=false >/dev/null; then
  echo "release: tag $VERSION is pushed but the release is still a draft; publish it by hand" >&2
  exit 1
fi

echo "release: $VERSION published to https://github.com/$REPO/releases/tag/$VERSION ($count jars)."
