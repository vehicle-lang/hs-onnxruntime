# Build

This package depends on `onnxruntime` (>=1.22).
This library can be built from source or installed via [Homebrew](https://brew.sh).
Build instructions are available at [Build ONNX Runtime for inferencing](https://onnxruntime.ai/docs/build/inferencing.html).

To build this package, you must let Cabal know where to find the directory that contains `libonnxruntime`.
Run the following command, replacing `<path/to/onnxruntime/lib>` with the appropriate paths for your machine.

```sh
cabal configure --extra-lib-dirs=</path/to/onnxruntime/lib>
```

If you are on Linux, you must ensure that the directory that contains `libonnxruntime` is on your `LD_LIBRARY_PATH`.
Run the following command, replacing `<path/to/onnxruntime/...>` with the appropriate paths for your machine.
You can either run this command for each new shell session or add it to your shell configuration.

```sh
export LD_LIBRARY_PATH="$LD_LIBRARY_PATH:<path/to/onnxruntime/lib>"
```
