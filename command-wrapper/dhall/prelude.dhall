-- vim: filetype=dhall
--
-- Dhall Prelude shared by the Command Wrapper Dhall libraries to guarantee
-- that it's  consistent and easier to update. In certain cases when some Dhall
-- built-ins are removed or changed it is important to bump the prelude to
-- avoid issues with binary cache containing deprecated primitives.

https://prelude.dhall-lang.org/v17.1.0/package.dhall sha256:10db3c919c25e9046833df897a8ffe2701dc390fa0893d958c3430524be5a43e
