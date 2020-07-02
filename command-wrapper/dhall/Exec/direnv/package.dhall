-- vim: filetype=dhall

{ command =
      ./command sha256:337315e737384fccddbed939a7fe71fbe01563407fffcfc67a182c81fd2707ed
    ? ./command
, completion-script =
      ./completion-script sha256:806c6f8632ddddedfd3c93330f26f074078bc26c4ffd91a232e589385f198c51
    ? ./completion-script
, completion =
      ./completion sha256:00e8588ef6c33d59f28b9f408b7689af64b1ded99516a2b4ceaa13d97eb851e0
    ? ./completion
, exec =
      ./exec sha256:8466061177d8bdc334a56c0fac16b0c3f2fd99d5805c314075ac170901b415d2
    ? ./exec
}
