#!/bin/bash
# Migrate reporepo v1 -> v2: latest version of each package per handle,
# except compiler-family packages (ocaml-*, relocatable-compiler*,
# compiler-cloning*) which keep all versions because toolchains pin
# specific compiler releases.
#
# v2 keeps the same on-disk shape as v1
# (handle/packages/<pkg>/<pkg>.<ver>/opam) but only the highest
# version of each non-compiler package survives the copy. The schema
# bump exists so v2 opam files can carry x-d10-archive: "<sha>"
# pointing at consolidated source archives baked by [oi repo bump].
#
# Usage:
#   ./scripts/migrate-reporepo-v1-to-v2.sh [SRC] [DST]
#
# Defaults to migrating $XDG_DATA_HOME/oi/reporepo/v1 -> v2 in place.

set -euo pipefail

SRC="${1:-${XDG_DATA_HOME:-$HOME/.local/share}/oi/reporepo/v1}"
DST="${2:-${XDG_DATA_HOME:-$HOME/.local/share}/oi/reporepo/v2}"

if [ ! -d "$SRC" ]; then
  echo "no source at $SRC" >&2
  exit 1
fi

if [ -e "$DST" ]; then
  echo "destination $DST already exists; remove it first" >&2
  exit 1
fi

mkdir -p "$DST"

total_pkgs=0
total_versions=0
copied_versions=0

for handle_dir in "$SRC"/*/; do
  handle=$(basename "$handle_dir")
  out_dir="$DST/$handle"
  mkdir -p "$out_dir"

  if [ -f "$handle_dir/repo" ]; then
    cp "$handle_dir/repo" "$out_dir/repo"
  fi

  if [ ! -d "$handle_dir/packages" ]; then
    continue
  fi

  mkdir -p "$out_dir/packages"

  pkg_count=0
  ver_count=0
  copy_count=0

  for pkg_dir in "$handle_dir"/packages/*/; do
    pkg=$(basename "$pkg_dir")
    pkg_count=$((pkg_count + 1))
    versions=()
    while IFS= read -r v; do
      versions+=("$v")
    done < <(ls "$pkg_dir" 2>/dev/null || true)
    ver_count=$((ver_count + ${#versions[@]}))

    if [ ${#versions[@]} -eq 0 ]; then
      continue
    fi

    case "$pkg" in
      ocaml|ocaml-*|relocatable-compiler*|compiler-cloning*)
        # Compiler-family: keep all versions. Toolchains pin specific
        # OCaml versions (toolchain-ocaml-5-4 → ocaml.5.4.1) so
        # dropping older entries breaks the solve.
        for v in "${versions[@]}"; do
          if [ -d "$pkg_dir$v" ]; then
            mkdir -p "$out_dir/packages/$pkg"
            cp -a "$pkg_dir$v" "$out_dir/packages/$pkg/"
            copy_count=$((copy_count + 1))
          fi
        done
        ;;
      *)
        # Latest version only. sort -V is semver-aware and agrees
        # with lexicographic order on date-based versions like
        # 20260506.0.
        latest=$(printf '%s\n' "${versions[@]}" | sort -V | tail -1)
        src_ver_dir="$pkg_dir$latest"
        if [ -d "$src_ver_dir" ]; then
          mkdir -p "$out_dir/packages/$pkg"
          cp -a "$src_ver_dir" "$out_dir/packages/$pkg/"
          copy_count=$((copy_count + 1))
        fi
        ;;
    esac
  done

  total_pkgs=$((total_pkgs + pkg_count))
  total_versions=$((total_versions + ver_count))
  copied_versions=$((copied_versions + copy_count))

  echo "$handle: $copy_count/$pkg_count packages (dropped $((ver_count - copy_count)) older versions)"
done

echo
echo "v2 reporepo at $DST"
echo "  $total_pkgs packages walked, $total_versions versions seen, $copied_versions copied"
