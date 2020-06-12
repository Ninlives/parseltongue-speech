let
  ribRevision = "4b3d374aec7cc16fc046b46bbac7eb577f326696";
  nixpkgsRev = "cfe68f2b68b7";
  name = "parseltongue";

  inherit (import (builtins.fetchTarball {
    url = "https://github.com/hercules-ci/gitignore/archive/7415c4f.tar.gz";
    sha256 = "1zd1ylgkndbb5szji32ivfhwh04mr1sbgrnvbrqpmfb67g2g3r9i";
  }) { })
    gitignoreFilter;

  sourceFilter = src:
    let ingnored = gitignoreFilter src;
    in path: type: ingnored path type && path != toString ./speak;

  rib = builtins.fetchTarball {
    url = "https://github.com/srid/rib/archive/${ribRevision}.tar.gz";
    sha256 = "15hlczps74iics137k4g4slq6aifpil2qdypnmzrsc9l56pc6wf9";
  };

  pkgs = import (builtins.fetchTarball {
    url = "https://github.com/nixos/nixpkgs/archive/${nixpkgsRev}.tar.gz";
    sha256 = "1m202ifx2iy06dqmgih84a1gzf1vj5kw17dqzv3sc2ikwz1d7rnp";
  }) {
    config = { };
    overlays = [ ];
  };

  root = pkgs.lib.cleanSourceWith {
    filter = sourceFilter ./.;
    src = ./.;
    name = "source";
  };

in (import rib { inherit pkgs root name; }).overrideAttrs
(a: { allowSubstitutes = false; })
