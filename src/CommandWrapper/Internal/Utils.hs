-- |
-- Module:      CommandWrapper.Internal.Utils
-- Description: Utilities used by internal subcommands.
-- Copyright:   (c) 2018-2019 Peter Trško
-- License:     BSD3
--
-- Maintainer:  peter.trsko@gmail.com
-- Stability:   experimental
-- Portability: GHC specific language extensions.
--
-- Utilities used by internal subcommands.
module CommandWrapper.Internal.Utils
    ( runMain
    )
  where

import Data.Monoid (Endo)

import qualified Mainplate (noConfigToRead, runAppWith)


-- | Simplified version of 'Mainplate.runAppWith' that assumes that we don't
-- need to parse any additional configuration file. This should be true for
-- internal commands.
runMain
    :: Functor mode
    => IO (Endo (mode config))
    -> (Endo (mode config) -> IO (mode config))
    -> (mode config -> IO ())
    -> IO ()
runMain parseOptions =
    Mainplate.runAppWith parseOptions (pure . Mainplate.noConfigToRead)
