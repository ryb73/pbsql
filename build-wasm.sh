#!/usr/bin/env bash
rm -rf pkg-nodejs pkg-bundler

wasm-pack build --no-pack --release --target nodejs -d pkg-nodejs
rm pkg-nodejs/.gitignore
mv pkg-nodejs/pbsql.js pkg-nodejs/pbsql.cjs

wasm-pack build --no-pack --release --target bundler -d pkg-bundler
rm pkg-bundler/.gitignore
