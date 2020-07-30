-- vim: filetype=dhall

let RunOptions =
      { Type =
            ./Type.dhall sha256:c43df7ef522e86341c59087914011ef5deadeba6b9363665a63ba5e15aa3647f
          ? ./Type.dhall
      }

let ExecOptions =
      { default =
            ../ExecOptions/default.dhall sha256:ecfa3148efb09aa997b99ed97df7f7f1e0184e251b56d9c5407021d35d008fdd
          ? ../ExecOptions/default.dhall
      }

let default = ExecOptions.default ∧ { remove = False }

let consistency = assert : (RunOptions ∧ { default })::{=} ≡ default

in  default
