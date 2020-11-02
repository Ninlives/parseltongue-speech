let
  ribRevision = "4b3d374aec7cc16fc046b46bbac7eb577f326696";
  nixpkgsRev = "cfe68f2b68b7";
  name = "parseltongue";

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

  lib = pkgs.lib;

  root = pkgs.lib.sourceByRegex ./. [ ".*\\.cabal$" "^src(/.*)?" ];

in {

  speak = (import rib { inherit pkgs root name; }).overrideAttrs
    (a: { allowSubstitutes = false; });

  static = with pkgs;
    let
      hljs =
        "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/10.3.2";
      css = [
        (fetchurl {
          url = "${hljs}/styles/gruvbox-dark.min.css";
          sha256 = "06h5m45wj78yll5z12hj9k3j4gyzximijvzwrd19ynj64sw4c02g";
          postFetch = ''
            sed -e 's/#282828/#1d2021/g' -i $out
          '';
        })
      ];
      js = [
        (fetchurl {
          url = "${hljs}/highlight.min.js";
          sha256 = "0nggf9384i8n9c1cv22bgr468f3x51qg1k87ngdmwi9a48w9nrsh";
        })
        (fetchurl {
          url = "${hljs}/languages/shell.min.js";
          sha256 = "0y40x7hgsy40qs242ksnlp5bxcwxsnrkpkbgkc5xrfqc0iyr3p3w";
        })
        (fetchurl {
          url = "${hljs}/languages/nix.min.js";
          sha256 = "05fxfp89801z4169kq8k95cb2wixdnfclr86g9nw8nbqzvcrcs0g";
        })
        (writeText "hljs" "hljs.initHighlightingOnLoad();")

        (fetchurl {
          url = "https://cdnjs.cloudflare.com/ajax/libs/pangu/4.0.7/pangu.min.js";
          sha256 = "1qnviqn1kdqi4xy64c59a5n75wziyln1nva5qa18s48xlzks7v4g";
        })
        (writeText "pangu" ''
          // <<<javascript>>>
          pangu.spacingElementById('main');
          pangu.spacingElementByClassName('comment');
          pangu.spacingElementByTagName('p');

          document.addEventListener('DOMContentLoaded', () => {
            // listen to any DOM change and automatically perform spacing via MutationObserver()
            pangu.autoSpacingPage();
          });
          // >>>javascript<<<
        '')
      ];
    in runCommand "static" { } ''
      mkdir -p $out
      ${lib.concatMapStringsSep "\n" (c: "cat ${c} >> $out/style.css") css}
      ${lib.concatMapStringsSep "\n" (j: "cat ${j} >> $out/script.js") js}
    '';
}
