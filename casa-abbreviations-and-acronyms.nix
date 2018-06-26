{ mkDerivation, base, checkers, containers, fuzzy, hedgehog, lens
, monoid-subclasses, optparse-applicative, QuickCheck, stdenv
, tasty, tasty-hedgehog, tasty-hunit, tasty-quickcheck
, transformers
}:
mkDerivation {
  pname = "casa-abbreviations-and-acronyms";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base containers fuzzy lens monoid-subclasses
  ];
  executableHaskellDepends = [
    base fuzzy lens optparse-applicative
  ];
  testHaskellDepends = [
    base checkers hedgehog lens QuickCheck tasty tasty-hedgehog
    tasty-hunit tasty-quickcheck transformers
  ];
  homepage = "https://github.com/qfpl/casa-abbreviations-and-acronyms";
  description = "CASA Abbreviations and Acronyms";
  license = stdenv.lib.licenses.bsd3;
}
