final: prev: {
  ocamlPackages = prev.ocaml-ng.ocamlPackages_5_1.overrideScope (ofinal: oprev: let
    serde = {
      src = prev.fetchFromGitHub {
        owner = "serde-ml";
        repo = "serde";
        rev = serde.version;
        sha256 = "dSs3btcIrn/khsnVFTyRTE7yoj3qcJJujtCabKflhDM=";
      };

      version = "82a773bdbcaf4450dd0758e40d8b05c70b18e413";
      duneVersion = "3";
    };
  in {
    colors = oprev.buildDunePackage rec {
      pname = "colors";
      version = "0.0.1";

      duneVersion = "3";

      src = prev.fetchFromGitHub {
        owner = "leostera";
        repo = "colors";
        rev = version;
        sha256 = "XOIeeSHJMRuhOKIx4oR17gc0nRUSfwTt/K1qcuuXsws=";
      };
    };

    spices = oprev.buildDunePackage rec {
      pname = "spices";
      version = "0.0.2";

      duneVersion = "3";

      src = prev.fetchFromGitHub {
        owner = "leostera";
        repo = "minttea";
        rev = version;
        sha256 = "vezMp+dLg7y0sA6+8WPsXDjs8ncsqhTMFDHtmx9cFSU=";
      };

      propagatedBuildInputs = [ofinal.colors ofinal.tty];
    };

    rio = oprev.buildDunePackage rec {
      pname = "rio";
      version = "0.0.8";

      duneVersion = "3";

      src = prev.fetchFromGitHub {
        owner = "riot-ml";
        repo = "riot";
        rev = version;
        sha256 = "LAA3un6eb4kNCw/H9w23akPoaRobkKXP3jiBgFsugZQ=";
      };

      propagatedBuildInputs = [oprev.cstruct];
    };

    serde = oprev.buildDunePackage {
      pname = "serde";
      inherit (serde) version duneVersion src;

      propagatedBuildInputs = [ofinal.spices ofinal.rio oprev.qcheck];
    };

    serde_derive = oprev.buildDunePackage {
      pname = "serde_derive";
      inherit (serde) version duneVersion src;

      propagatedBuildInputs = [ofinal.serde oprev.ppx_deriving oprev.ppxlib];
    };
  });
}
