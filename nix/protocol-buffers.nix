{ mkDerivation, array, base, binary, bytestring, containers
, directory, filepath, mtl, parsec, stdenv, syb, utf8-string
}:
mkDerivation {
  pname = "protocol-buffers";
  version = "2.1.7";
  sha256 = "0nnb4x07nrp51dldnbn46093k7vrw7l60vripn8xdwsy60l22zsg";
  libraryHaskellDepends = [
    array base binary bytestring containers directory filepath mtl
    parsec syb utf8-string
  ];
  homepage = "https://github.com/k-bx/protocol-buffers";
  description = "Parse Google Protocol Buffer specifications";
  license = stdenv.lib.licenses.bsd3;
}
