-- vim: filetype=dhall

{ command =
      ./command sha256:337315e737384fccddbed939a7fe71fbe01563407fffcfc67a182c81fd2707ed
    ? ./command
, completion-script =
      ./completion-script sha256:806c6f8632ddddedfd3c93330f26f074078bc26c4ffd91a232e589385f198c51
    ? ./completion-script
, completion =
      ./completion sha256:99110f0a5d7cefc91b82d701eaf598c28da67ef436de704e571c4b336ac209a7
    ? ./completion
, exec =
      ./exec sha256:82271e1ab07cbbe5b4bc36b400ecc543adc10a190735269d0128b74da4190a4e
    ? ./exec
}
