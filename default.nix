{ mkDerivation, base, directory, pipes, pipes-safe, stdenv
, system-fileio, system-filepath, unix
}:
mkDerivation {
  pname = "dirstream";
  version = "1.0.2";
  src = ./.;
  libraryHaskellDepends = [
    base directory pipes pipes-safe system-fileio system-filepath unix
  ];
  description = "Easily stream directory contents in constant memory";
  license = stdenv.lib.licenses.bsd3;
}
