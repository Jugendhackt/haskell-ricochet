{ mkDerivation, attoparsec, base, base32string, bytestring
, exceptions, fetchgit, hexstring, hspec, hspec-attoparsec
, hspec-expectations, network, network-attoparsec, network-simple
, socks, splice, stdenv, text, transformers
}:
mkDerivation {
  pname = "network-anonymous-tor";
  version = "0.10.0";
  src = fetchgit {
    url = "https://github.com/solatis/haskell-network-anonymous-tor";
    sha256 = "9adb606d73b04dfb261d6e899b7a25626c600f386cc9aa203a32633955a87807";
    rev = "464e63373aea426a5efd11577311fe5177b8e78a";
  };
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    attoparsec base base32string bytestring exceptions hexstring
    network network-attoparsec network-simple socks text transformers
  ];
  executableHaskellDepends = [
    base exceptions network network-simple splice
  ];
  testHaskellDepends = [
    attoparsec base base32string bytestring exceptions hspec
    hspec-attoparsec hspec-expectations network network-simple socks
    text transformers
  ];
  doCheck = false;
  homepage = "http://www.leonmergen.com/opensource.html";
  description = "Haskell API for Tor anonymous networking";
  license = stdenv.lib.licenses.mit;
}
