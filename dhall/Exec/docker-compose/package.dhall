-- vim: filetype=dhall

{ GlobalOptions =
      ./GlobalOptions sha256:53cba1247c3b9afd5a2a200a7ee7b819b7b7180a109fa8d46258dbc1ee496853
    ? ./GlobalOptions
, DownOptions =
      ./DownOptions sha256:760e4cd0b9b14aa5c3225f155da55eb8eef9c41a75a4535665ac5d7c6aceb182
    ? ./DownOptions
, UpOptions =
      ./UpOptions sha256:099f8f6f629b9bdb1861f8552debb0343900d053faab9a63d0c6bce8a919293b
    ? ./UpOptions
, Action =
      ./Action sha256:4a10c31e43e6ea8f8f22fa117c2b7ad1473b55eae02fd4b8782f1eb4a302e092
    ? ./Action
, command =
      ./command sha256:c4b953b8ccdbbb92bd57f09d6490f2446f1c75460595dc6f4acb9b98523526fe
    ? ./command
, noAction =
      ./noAction sha256:8903056c4b2af6122e003c43c040adc609dfbbac4a779bbf5b7c0cc21031e947
    ? ./noAction
, defaultGlobalOptions =
      ./defaultGlobalOptions sha256:5544ca02118ff7f8dc7891b269f84ce008b2bf6ba85d28b31585d2ac8c675f4e
    ? ./defaultGlobalOptions
, globalOptions =
      ./globalOptions sha256:11704b75be6ea8b64350c345a8dab20ba40f2a2ba260f563d152a10d95073654
    ? ./globalOptions
, completion =
      ./completion sha256:65064ebaf1f819b2abb975f8d66eb530daec1a981fc2c8a5fe6ad8886d162373
    ? ./completion
}
