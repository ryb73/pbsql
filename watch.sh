#!/usr/bin/env bash
cargo watch -i src/snapshots -c -x check -x "insta test --review" -x "test -- --show-output"
