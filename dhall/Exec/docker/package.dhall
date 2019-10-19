-- vim: filetype=dhall

{ GlobalOptions =
      ./DockerGlobalOptions sha256:88e6fa8fe07e5d00621b8c6de87f845d885fd4c2fc9be493cdd8a66ddfa7f524
    ? ./DockerGlobalOptions
, ExecOptions =
      ./DockerExecOptions sha256:2db64f6364ae0f19c6950ce41c5407f1172ec2d2c9a011522d575aae2a5986b5
    ? ./DockerExecOptions
, RunOptions =
      ./DockerRunOptions sha256:df4fdf5b50b4b3bdcd0e021533de4d6333432518d41bbea6146d9b73847a7892
    ? ./DockerRunOptions
, globalOptions =
      ./globalOptions sha256:150c73831857ece522d3f066554ad2883e36ceb2f98bf85947c8eacf0e09cb0b
    ? ./globalOptions
, defaultGlobalOptions =
      ./defaultGlobalOptions sha256:9db912426285cf1cfded16ebaa0c59fe3f1c44059e9976604bd732bda4c4525d
    ? ./defaultGlobalOptions
, envOptions =
      ./envOptions sha256:d0426badaef8cd6535378bbd9b5caf073e2b7d794eae541a454ebcba4cb52531
    ? ./envOptions
, execOptions =
      ./execOptions sha256:cdea112398e5a0ecd70ab2aad318031d9bad79fab0c7fecb884b4ca90bb2d908
    ? ./execOptions
, defaultExecOptions =
      ./defaultExecOptions sha256:b938ff617d5bb809fc207331f5b2f9a0bd3841fd448f4ed0b6fe49c2a2d6bd68
    ? ./defaultExecOptions
, interactiveExecOptions =
      ./interactiveExecOptions sha256:d08a8b2f010ba5606d40da95161b95de4a71891ad2363e42b896fca96e3efb2b
    ? ./interactiveExecOptions
, runOptions =
      ./runOptions sha256:5fdea1f964ab7b935cf9a80e0f375fe0d0e75c3459a0bb3200d4492ee12d2895
    ? ./runOptions
, interactiveRunOptions =
      ./interactiveRunOptions sha256:99a56630de324a7e1a853892fbc380527839c8341d5f49d0fff4ee907e346163
    ? ./interactiveRunOptions
, ephemeralRunOptions =
      ./ephemeralRunOptions sha256:74589b0daa87479ee24f2e38e951ec01ced289556c95e5a8ae19f498e06edb3a
    ? ./ephemeralRunOptions
, prune =
      ./prune sha256:2230f97860f1868581aebdffb1f69bfc8eab0aac9bdf57b21e938937d6a85f23
    ? ./prune
, exec =
      ./exec sha256:0a7e8673c232b8f769b127111da64c7cb1c7694c2248485bea0c658eb0e962fd
    ? ./exec
, run =
      ./run sha256:da02a2440e0e3733ec8d287336c4de2c4e85eb718eabdf0773eeddc4b6fe9ece
    ? ./run
}
