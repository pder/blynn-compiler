{ stdenv, fetchFromGitHub, git }:

stdenv.mkDerivation {
  pname = "mescc-tools-seed";
  version = "unstable-2021-01-01";

  nativeBuildInputs = [ git ];
  src = fetchFromGitHub {
    owner = "oriansj";
    repo = "mescc-tools-seed";
    rev = "0e6d8d6d3f261b550af1768b767f66b30fd07854";
    sha256 = "04nra450njaq40rs196m11nmmf64sf1zjw5ic059hqz49fvmqxz0";
    fetchSubmodules = true;
  };
  makeFlags = [ "PREFIX=${placeholder "out"}" ];

  dontStrip = true;

  installPhase = ''
    mkdir -p $out/bin
    cp ./bin/* $out/bin
  '';
}
