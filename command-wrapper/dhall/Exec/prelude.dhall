-- vim: filetype=dhall
--
-- Dhall Prelude shared by the whole Exec library to guarantee that it's
-- consistent. In certain cases when some Dhall built-ins are removed or
-- changed it is important to bump the prelude to avoid issues with binary
-- cache containing deprecated primitives.

../prelude.dhall
