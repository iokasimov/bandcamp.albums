{ mkDerivation, aeson, async, base, bytestring, directory, filepath
, http-client, joint, lens, stdenv, tagged, terminal-progress-bar
, text, transformers, wreq
}:
mkDerivation {
  pname = "bandcamp";
  version = "0.1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    aeson async base bytestring directory filepath http-client joint
    lens tagged terminal-progress-bar text transformers wreq
  ];
  executableHaskellDepends = [
    aeson async base bytestring directory filepath http-client joint
    lens tagged terminal-progress-bar text transformers wreq
  ];
  license = stdenv.lib.licenses.bsd3;
}
