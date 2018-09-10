#!/usr/bin/env bash

yes | cp ./DucklingCli.hs ./duckling/exe
sed -i '/executable duckling-example-exe/i\
executable duckling-cli\
  main-is:             DucklingCli.hs\
  hs-source-dirs:      exe\
  ghc-options:         -threaded -rtsopts -with-rtsopts=-N\
  other-modules:       Duckling.Data.TimeZone\
  build-depends:       duckling\
                     , base\
                     , aeson\
                     , bytestring\
                     , directory             >= 1.2.2.0 && < 1.4\
                     , extra\
                     , filepath              >= 1.4.0.0 && < 1.5\
                     , text\
                     , text-show\
                     , time\
                     , timezone-olson        >= 0.1.7 && < 0.2\
                     , timezone-series\
                     , unordered-containers\
  default-language:    Haskell2010\
  default-extensions:  OverloadedStrings\
' ./duckling/duckling.cabal

cd duckling
stack build :duckling-cli
cp $(find ./ -name "duckling-cli" -type f | head -1) ../
