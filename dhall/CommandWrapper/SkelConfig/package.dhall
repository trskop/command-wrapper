-- vim: filetype=dhall

{ Type =
      ./Type sha256:2968c22b6fd6f1f69db2461e38a0e95596839312ae1dd6c87c06bed3f5cbd1da
    ? ./Type
, default =
      ./default sha256:1eaa4299bdc1fe2a75491ecbdacbb88c447c0780822ef37266f4939436fd54f6
    ? ./default
, SkelLanguage =
      ./SkelLanguage sha256:9756f293780a9da48ed4c81914fd65f64ad1b2bbd1006ebafe3eded645806790
    ? ./SkelLanguage
, template =
      ./template/package.dhall sha256:44319b7cc88647ecb961dd78118ca291d4ecc78ac5fb70c1af7384dc74d03ffc
    ? ./template/package.dhall
}
