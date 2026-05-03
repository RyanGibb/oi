#!/bin/sh
# local opam build for CI for Homebrew taps and so on

export OPAMROOT=`pwd`/_opamroot
export OPAMYES=1
export OPAMCONFIRMLEVEL=unsafe-yes
opam init -ny --disable-sandboxing
opam repo add avsm git+https://git.recoil.org/anil.recoil.org/opam-overlay --all
opam switch create --repos=avsm,default .
opam exec -- dune build --profile=release
