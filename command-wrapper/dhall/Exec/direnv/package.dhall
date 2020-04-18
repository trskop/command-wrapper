-- vim: filetype=dhall

{ command =
      ./command sha256:337315e737384fccddbed939a7fe71fbe01563407fffcfc67a182c81fd2707ed
    ? ./command
, completion-script =
      ./completion-script sha256:806c6f8632ddddedfd3c93330f26f074078bc26c4ffd91a232e589385f198c51
    ? ./completion-script
, completion =
      ./completion sha256:26764bf658d3bbd849ac615adf73cbab1521a9bb2ab7d737987092c42f299f9f
    ? ./completion
, exec =
      ./exec sha256:82271e1ab07cbbe5b4bc36b400ecc543adc10a190735269d0128b74da4190a4e
    ? ./exec
}
