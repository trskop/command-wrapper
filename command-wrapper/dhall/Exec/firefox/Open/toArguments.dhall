-- vim: filetype=dhall

let Open =
        ./Type.dhall sha256:a63c2ded38e71c92968a0e34a80d6c2b5a9f7dd498864b4e69ef35b5e930fb5f
      ? ./Type.dhall

let toArguments =
      λ(_ : Open) →
        merge
          { NewWindow = λ(url : Text) → [ "--new-window", url ]
          , NewTab = λ(url : Text) → [ "--new-tab", url ]
          , PrivateWindow = λ(url : Text) → [ "--private-window", url ]
          , Url = λ(url : Text) → [ url ]
          , Search = λ(term : Text) → [ "--search", term ]
          , BrowserWindow = [ "--browser" ]
          }
          _

let test0 =
        assert
      :   toArguments (Open.NewWindow "https://example.com")
        ≡ [ "--new-window", "https://example.com" ]

let test1 =
        assert
      : toArguments (Open.Url "https://example.com") ≡ [ "https://example.com" ]

in  toArguments : Open → List Text
