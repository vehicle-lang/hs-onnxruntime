# Build

This package depends on `onnxruntime` (>=1.21).
To build this package, you must let Cabal know where to find the `lib` and `include` directories for `onnxruntime`.
Run the following command, replacing `<path/to/onnxruntime/...>` with the appropriate paths for your machine.

```sh
cabal configure \
    --extra-lib-dirs=</path/to/onnxruntime/lib> \
    --extra-include-dirs=</path/to/onnxruntime/include>
```

If you are on Linux, you must ensure that the `lib` directory for `onnxruntime` is on your `LD_LIBRARY_PATH`.
Run the following command, replacing `<path/to/onnxruntime/...>` with the appropriate paths for your machine.
You can either run this command for each new shell session or add it to your shell configuration.

```sh
export LD_LIBRARY_PATH="$LD_LIBRARY_PATH:<path/to/onnxruntime/lib>"
```
