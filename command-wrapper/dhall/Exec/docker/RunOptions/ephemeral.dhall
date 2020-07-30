-- vim: filetype=dhall

let RunOptions =
      { Type =
            ./Type.dhall sha256:c43df7ef522e86341c59087914011ef5deadeba6b9363665a63ba5e15aa3647f
          ? ./Type.dhall
      , default =
            ./default.dhall sha256:44ddaa795338e389c4ec7ffdc4b43e6581e4718913f40408c0ada14a6a2179d0
          ? ./default.dhall
      }

let ephemeral = RunOptions.default ⫽ { remove = True }

let consistency =
      assert : (RunOptions ⫽ { default = ephemeral })::{=} ≡ ephemeral

in  ephemeral
