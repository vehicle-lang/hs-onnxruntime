Project structure
=================

The project is setup as follows:

- We have a custom `Setup.hs` script that downloads the Onnxruntime Git
  repository into the `vendor/onnxruntime` folder. It then builds it,
  setting the build directory to that requested by Cabal.
  In the `.cabal` file we specify that the resulting libraries should be
  bundled using the `extra-bundled-libraries` field.

- We then have a custom C file in `cbits` and `include` folder that
  depends on the Onnx libraries that calls them to provide the functionality
  that we need.

- We then use C2HS to bind Haskell code to these interface C files in
  `src/OnnxRuntime/Bindings.chs`.

- Finally, `src/OnnxRuntime.hs` provides a high-level Haskell interface to the
  code bound above.
