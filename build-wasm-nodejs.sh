#!/usr/bin/env bash
rm -rf pkg-nodejs pkg-bundler
wasm-pack build --no-pack --release --target nodejs -d pkg-nodejs
mv pkg-nodejs/cabinet_sql.js pkg-nodejs/cabinet_sql.cjs
wasm-pack build --no-pack --release --target bundler -d pkg-bundler
