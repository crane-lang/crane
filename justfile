# Build and run a Crane program.
run:
    cargo run -- run

# Run the tests.
test:
    cargo test

# Review `insta` snapshots.
review-snapshots:
    cargo insta test --review

# Remove obsolete `insta` snapshots.
remove-obsolete-snapshots:
    cargo insta test --unreferenced delete

clone-llvm:
    git clone --depth 1 --branch llvmorg-16.0.5 https://github.com/llvm/llvm-project.git llvm

build-llvm:
    pushd llvm
    cmake -S llvm -B build -G Ninja \
        -DCMAKE_BUILD_TYPE=Release \
        -DLLVM_ENABLE_PROJECTS="clang;lld" \
        -DCMAKE_INSTALL_PREFIX=/Users/maxdeviant/projects/crane/llvm-build
    ninja -C build
    popd

# Count the source lines of code with `cloc` (requires `nix-shell`).
cloc:
    nix-shell -p cloc --command "cloc --vcs=git ."
