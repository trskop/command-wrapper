-- vim: filetype=dhall

let ExecOptions =
      { interactive =
            ../ExecOptions/interactive.dhall sha256:fb70fe482f04a1bd4a2b3e639328356b1212dc118a1015e646ad0de93fd95823
          ? ../ExecOptions/interactive.dhall
      }

let RunOptions =
      { Type =
            ./Type.dhall sha256:c43df7ef522e86341c59087914011ef5deadeba6b9363665a63ba5e15aa3647f
          ? ./Type.dhall
      }

let interactive = ExecOptions.interactive ∧ { remove = False }

let consistency =
      assert : (RunOptions ∧ { default = interactive })::{=} ≡ interactive

in  interactive
