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
    sha256 = "223d79a8003a17c030ef51cdd9795c64fd3fbab91c2a5e580302c498cab9356c";
    rev = "d2bac315ffec642689480348d4447b042677ed20";
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
