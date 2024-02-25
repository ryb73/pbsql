#!/usr/bin/env bash
cargo build -r
cargo check
cargo test -- --show-output
