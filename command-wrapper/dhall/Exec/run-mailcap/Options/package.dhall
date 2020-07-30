-- vim: filetype=dhall

{ Type =
      ./Type.dhall sha256:674f9d48d01ccb653cd071dd30af0766337d30d0c8fa11498ab3a6e91219479a
    ? ./Type.dhall
, default =
      ./default.dhall sha256:6c7eb164746af8d0c1106285fdb390265b46ae49928384734675638088522e32
    ? ./default.dhall
, toArguments =
      ./toArguments.dhall sha256:efdeb9c570b1b569da23b2abca3f0fc1eac3d7dbf14bb4afeda65afc96532af7
    ? ./toArguments.dhall
, toEnvironment =
      ./toEnvironment.dhall sha256:94f31edf777aa030686c2188d8836ba1c0814e5fa9c560feca369cb9602d333a
    ? ./toEnvironment.dhall
}
