-- vim: filetype=dhall

{ Options =
      ./Options/package.dhall sha256:f312be6c22f2723843c2fbb74b080f560c130200c3cd69168275550b17fd9ee7
    ? ./Options/package.dhall
, template =
      ./template sha256:b234b76c929a178cc992cc4cb34e6cd8deaec043229c4f1e34142916da4bce25
    ? ./template
}
