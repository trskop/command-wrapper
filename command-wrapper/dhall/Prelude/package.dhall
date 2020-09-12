-- vim: filetype=dhall
--
-- Dhall Prelude shared by the Command Wrapper Dhall libraries to guarantee
-- that it's  consistent and easier to update. In certain cases when some Dhall
-- built-ins are removed or changed it is important to bump the prelude to
-- avoid issues with binary cache containing deprecated primitives.

./v18.0.0.dhall ? ./import-v18.0.0.dhall
