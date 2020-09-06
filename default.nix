{ mkDerivation, base, soxlib, stdenv, storablevector, tasty
, tasty-hunit, tasty-quickcheck
}:
mkDerivation {
  pname = "1bitmusic";
  version = "0.2.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [ base soxlib storablevector ];
  executableHaskellDepends = [ base ];
  testHaskellDepends = [ base tasty tasty-hunit tasty-quickcheck ];
  homepage = "https://github.com/victoradan/1bitmusic#readme";
  description = "A library for writing 1-bit music";
  license = stdenv.lib.licenses.bsd3;
}
