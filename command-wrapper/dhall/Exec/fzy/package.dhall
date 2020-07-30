-- vim: filetype=dhall

{ Options =
      ./Options/package.dhall sha256:2ca835d1d0317d78dfef48ef25c865a8bfcc3db6712639b6a1b9a7ff9617a9de
    ? ./Options/package.dhall
, command =
      ./command.dhall sha256:5e347c6290809fc216728f476666ed018d9487af6acc7dd81744d6563eb513d9
    ? ./command.dhall
}
