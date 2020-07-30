-- vim: filetype=dhall

let Options =
      { Type =
            ./Type.dhall sha256:b2526c288d5a0419d362c1375b879214396f6d1f6cd88b0416545724fd661b26
          ? ./Type.dhall
      }

let default = { workingDirectory = None Text, stackYaml = None Text }

let consistency = assert : (Options ∧ { default })::{=} ≡ default

in  default
