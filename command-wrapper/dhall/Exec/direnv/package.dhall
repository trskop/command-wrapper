-- vim: filetype=dhall

{ command =
      ./command.dhall sha256:e9c3b8f1b0a69c31bd7d6d4efd9f41144b03b3885c37b09b907a5a66beff2387
    ? ./command.dhall
, completion-script =
      ./completion-script.dhall sha256:806c6f8632ddddedfd3c93330f26f074078bc26c4ffd91a232e589385f198c51
    ? ./completion-script.dhall
, completion =
      ./completion.dhall sha256:ab2cbf9a1c7270233bc0ef84d2acb038e58d12a31ff0d9a17c7de4a1ddf2aa0a
    ? ./completion.dhall
, exec =
      ./exec.dhall sha256:c549864ab2b8b1897f576751af249e5539d400c29ba7d961f072e07b38299caf
    ? ./exec.dhall
}
