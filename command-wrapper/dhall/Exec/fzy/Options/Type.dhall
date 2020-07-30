-- vim: filetype=dhall

let CommonOptions =
        ../../CommonOptions/Type.dhall sha256:2df1fcace0ed3c72e1bcf3c0e6b9ae0269c36315ced7961c62f7ba59d426d6eb
      ? ../../CommonOptions/Type.dhall

let FzyOptions =
      -- How many lines of items to display?  Zero is treated as use default
      -- value, i.e. `--lines=LINES` option is not passed to `fzy`. In addition
      -- to that if value is between 0 < n < 3 then we use value 3. This is due
      -- to the fact that `fzy` doesn't allow values smaller than 3.
      { lines : Natural

      -- Input prompt to use.  `None Text` is interpreted as "use default",
      -- i.e. no `--prompt=PROMPT` option is passed to `fzy`.
      , prompt : Optional Text

      -- Show the scores of individual matches.  `False` means don't pass
      -- `--show-scores` option to `fzy`.
      , showScores : Bool

      -- Use different TTY device.  `None Text` is interpreted as "use default",
      -- i.e. no `--tty=TTY` is passed to `fzy`.  Default TTY device is
      -- `/dev/tty`.
      , tty : Optional Text

      -- Initial search query.  `None Text` means no initial query, i.e. no
      -- `--query=QUERY` option is passed to `fzy`.
      , query : Optional Text

      -- Print sorted matches for QUERY in non-interactive mode.  `None Text`
      -- means that `--show-matches=QUERY` option is not passed to `fzy`.
      , showMatches : Optional Text

      -- How many workers to use?  Based on `fzy` code `--workers=0` is treated
      -- as if it wasn't passed at all, i.e. it will use number of CPUs.  Here
      -- we use zero as "do not pass" `--workers=N` option to `fzy`, which
      -- gives the same result.
      , workers : Natural
      }

in  CommonOptions ⩓ FzyOptions
