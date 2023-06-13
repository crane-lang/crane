clone-llvm:
    git clone --depth 1 --branch llvmorg-16.0.5 https://github.com/llvm/llvm-project.git llvm

build-llvm:
    pushd llvm
    cmake -S llvm -B build -G Ninja -DCMAKE_BUILD_TYPE=Release -DCMAKE_INSTALL_PREFIX=/Users/maxdeviant/projects/crane/llvm-build
    ninja -C build
    popd
