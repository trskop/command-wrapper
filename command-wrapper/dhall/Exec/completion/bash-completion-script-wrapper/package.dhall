-- vim: filetype=dhall

{ Options =
      ./Options/package.dhall sha256:f312be6c22f2723843c2fbb74b080f560c130200c3cd69168275550b17fd9ee7
    ? ./Options/package.dhall
, template =
      ./template sha256:2aa69e20f535ea334a04dfee311c5ff8fad778eb624f6437d0da303300e142ec
    ? ./template
}
