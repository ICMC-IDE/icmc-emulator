.PHONY: mif backend

mif:
	wasm-pack build --target web --reference-types --weak-refs --release --no-pack --out-dir ../build/mif mif

backend:
	wasm-pack build --target web --reference-types --weak-refs --release --no-pack --out-dir ../build/backend backend

build: mif backend
