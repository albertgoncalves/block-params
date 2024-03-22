#!/usr/bin/env bash

set -eu

rustfmt --color always src/*.rs
cargo clippy --color always --all --all-targets -- \
    -W clippy::all \
    -W clippy::complexity \
    -W clippy::correctness \
    -W clippy::nursery \
    -W clippy::pedantic \
    -W clippy::perf \
    -W clippy::suspicious \
    -A dead_code \
    -D warnings
RUST_BACKTRACE=1 cargo run --color always
