#!/usr/bin/env bash
cargo watch --why -c -i snapshots -i *.pending-snap -i *.snap.tmp -i lcov.info -i test-db -i watch.sh -x check -x "insta test --review" -x "llvm-cov --html -- --show-output" -x "llvm-cov report --lcov --output-path lcov.info"
