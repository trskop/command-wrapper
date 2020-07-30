-- vim: filetype=dhall

let ExecOptions =
      { Type =
            ./Type.dhall sha256:f060f3aa27210e24ea2ff0fd222b3c0d4c376f79077d376c3b40596ad7d3d74a
          ? ./Type.dhall
      , default =
            ./default.dhall sha256:ecfa3148efb09aa997b99ed97df7f7f1e0184e251b56d9c5407021d35d008fdd
          ? ./default.dhall
      }

let interactive =
      ExecOptions.default ⫽ { interactive = True, allocateTty = True }

let consistency =
      assert : (ExecOptions ⫽ { default = interactive })::{=} ≡ interactive

in  interactive
