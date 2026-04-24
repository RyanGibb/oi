`oi show --tree` prints the full per-package plan that `oi plan` used to
emit: a Plan header followed by per-package entries with layer hashes.

  $ export OI_DATA_DIR=$PWD/data
  $ export OI_CACHE_DIR=$PWD/cache
  $ export HOME=$PWD/home
  $ mkdir -p "$HOME"
  $ oi show --tree utop 2>&1 | grep -q "^Plan:"
  $ oi show --tree utop 2>&1 | grep -q "^  utop\."
  $ oi show --tree utop 2>&1 | grep -q "^    layer: "

The default view (no flags) prints a compact info block instead.

  $ oi show utop 2>&1 | grep -q "^Target:"
  $ oi show utop 2>&1 | grep -q "^Packages:"
