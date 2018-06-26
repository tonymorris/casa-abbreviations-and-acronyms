{ mkDerivation, base, bytestring, containers, fuzzy, lens
, monoid-subclasses, optparse-applicative, stdenv, wreq
}:
mkDerivation {
  pname = "casa-abbreviations-and-acronyms";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    base bytestring containers fuzzy lens monoid-subclasses wreq
  ];
  executableHaskellDepends = [
    base fuzzy lens optparse-applicative
  ];
  homepage = "https://github.com/qfpl/casa-abbreviations-and-acronyms";
  description = "CASA Abbreviations and Acronyms";
  license = stdenv.lib.licenses.bsd3;
}
