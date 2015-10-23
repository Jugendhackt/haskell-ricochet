{ mkDerivation, base, bytestring, containers, protocol-buffers
, stdenv
}:
mkDerivation {
  pname = "protocol-buffers-descriptor";
  version = "2.1.7";
  sha256 = "0rimfqjiqg067cicapsw9ppim7cwzyirc97m05g8mjcj1r7lq6lp";
  libraryHaskellDepends = [
    base bytestring containers protocol-buffers
  ];
  homepage = "https://github.com/k-bx/protocol-buffers";
  description = "Text.DescriptorProto.Options and code generated from the Google Protocol Buffer specification";
  license = stdenv.lib.licenses.bsd3;
}
