#!/usr/bin/env bash
#
# Install a Pyrocosm release into the local ivy repository (`~/.ivy2/local`, which coursier — and
# so Mill — consults by default), so a build resolves `mvn"dev.propensive:pyrocosm-model:X.Y.Z"`
# from the RELEASED jars rather than from a local compile. This is what the shared CI workflow
# does for every `extra_releases` pair: Soundness's own `sync_releases.py`, pointed here through
# `SOUNDNESS_RELEASE_REPO`. Each released jar carries its POM and ivy.xml under `META-INF/maven/`,
# so the jar is all that is downloaded.
#
# The script is fetched from the Soundness release this repository is built against (the
# `soundnessVersion` pin), and cached, so this needs the network only the first time for a
# given Soundness version.
#
# Usage: ./etc/ci/sync-releases.sh [X.Y.Z]     the pinned `pyrocosmVersion` when omitted
#        ./etc/ci/sync-releases.sh --staged    the jars of a local `./mill release.stage`
#         (or `make sync-releases [VERSION=X.Y.Z]`, `make sync-staged`)
#
# `--staged` is how a release candidate is tried in Fume or Flame before anything is tagged; the
# published path is how a release is verified afterwards, since it installs the exact bytes a
# consumer will resolve.
#
# NOTE: this overwrites whatever `make publishLocal` installed for the same version. Run
# `make publishLocal` again to go back to jars built from this working tree.
#
# Environment: PYROCOSM_RELEASE_REPO=owner/repo (default propensive/pyrocosm); GITHUB_TOKEN, if
# set, lifts the unauthenticated API rate limit; IVY_LOCAL overrides the destination.

set -euo pipefail
cd "$(git rev-parse --show-toplevel)"

REPO="${PYROCOSM_RELEASE_REPO:-propensive/pyrocosm}"
SOUNDNESS=$(grep 'val soundnessVersion' build.mill | grep -oE '[0-9]+\.[0-9]+\.[0-9]+' | tail -1)
PINNED=$(grep 'val pyrocosmVersion' build.mill | grep -oE '[0-9]+\.[0-9]+\.[0-9]+' | tail -1)

if [[ -z "$SOUNDNESS" ]]; then
  echo "sync-releases: could not read soundnessVersion from build.mill" >&2; exit 1
fi

SCRIPT="${TMPDIR:-/tmp}/sync_releases-$SOUNDNESS.py"

if [[ ! -f "$SCRIPT" ]]; then
  url="https://raw.githubusercontent.com/propensive/soundness/$SOUNDNESS/etc/ci/sync_releases.py"
  if ! curl -fsSL -o "$SCRIPT" "$url"; then
    rm -f "$SCRIPT"
    echo "sync-releases: could not fetch $url" >&2; exit 1
  fi
fi

if [[ "${1:-}" == "--staged" ]]; then
  exec env SOUNDNESS_RELEASE_REPO="$REPO" python3 "$SCRIPT" "$@"
fi

exec env SOUNDNESS_RELEASE_REPO="$REPO" python3 "$SCRIPT" "${1:-$PINNED}"
