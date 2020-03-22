-- vim: filetype=dhall

{ command =
      ./command sha256:337315e737384fccddbed939a7fe71fbe01563407fffcfc67a182c81fd2707ed
    ? ./command
, completion-script =
      ./completion-script sha256:806c6f8632ddddedfd3c93330f26f074078bc26c4ffd91a232e589385f198c51
    ? ./completion-script
, completion =
      ./completion sha256:27f46ab9cbf92a98fc56ff1b6c2f5960a8a737502c045439fb5abd258769333f
    ? ./completion
, exec =
      ./exec sha256:82271e1ab07cbbe5b4bc36b400ecc543adc10a190735269d0128b74da4190a4e
    ? ./exec
}
