-- vim: filetype=dhall
--
-- Dhall Prelude shared by the whole Exec library to guarantee that it's
-- consistent. In certain cases when some Dhall built-ins are removed or
-- changed it is important to bump the prelude to avoid issues with binary
-- cache containing deprecated primitives.

https://prelude.dhall-lang.org/v17.0.0/package.dhall sha256:10db3c919c25e9046833df897a8ffe2701dc390fa0893d958c3430524be5a43e
