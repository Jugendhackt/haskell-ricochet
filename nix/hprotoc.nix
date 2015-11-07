{ mkDerivation, alex, array, base, binary, bytestring, containers
, directory, filepath, haskell-src-exts, mtl, parsec
, protocol-buffers, protocol-buffers-descriptor, stdenv
, utf8-string
}:
mkDerivation {
  pname = "hprotoc";
  version = "2.1.7";
  sha256 = "11md7yvxnq2sx3svpz47ham0n4rsnl5pfy4vjh48j88dn9n2s0sq";
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    array base binary bytestring containers directory filepath
    haskell-src-exts mtl parsec protocol-buffers
    protocol-buffers-descriptor utf8-string
  ];
  libraryToolDepends = [ alex ];
  executableHaskellDepends = [
    array base binary bytestring containers directory filepath
    haskell-src-exts mtl parsec protocol-buffers
    protocol-buffers-descriptor utf8-string
  ];
  executableToolDepends = [ alex ];
  homepage = "https://github.com/k-bx/protocol-buffers";
  description = "Parse Google Protocol Buffer specifications";
  license = stdenv.lib.licenses.bsd3;
}
