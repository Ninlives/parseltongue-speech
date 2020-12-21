let
  # Pinned version
  ribRevision = "4b3d374aec7cc16fc046b46bbac7eb577f326696";
  nixpkgsRev = "cfe68f2b68b7";
  name = "parseltongue";
  tsukuRev = "7ea1317e21d0578e163d6bc426fd5d5416faa9d7";
  dottedRev = "d8c8fac5d32174fb7d261e0d58ba0aa07098e7b8";

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

  inherit (pkgs)
    writeText writeShellScript fetchurl fetchzip runCommand coreutils pandoc
    fixedsys-excelsior;
  inherit (pkgs.python3Packages) pandocfilters fontforge;
  inherit (pkgs.writers) writePython3;
  inherit (pkgs.lib) concatMapStringsSep;

  # Main generator
  generator = (import rib {
    inherit pkgs name;
    root = ./code/generator;
  });

  # Scripts
  pangu = writePython3 "pangu" { libraries = [ pandocfilters ]; }
    ./code/filter/pangu.py;
  extractCode = writePython3 "extract" { libraries = [ pandocfilters ]; }
    ./code/filter/extract-code.py;

  shrinkFont = writePython3 "shrink" { libraries = [ fontforge ]; }
    ./code/font/minimize.py;
  overwriteFont = writePython3 "overwrite" { libraries = [ fontforge ]; }
    ./code/font/overwrite.py;

  # Fonts
  tsuku-font = (fetchurl {
    url =
      "https://raw.githubusercontent.com/Ninlives/nixos-config/${tsukuRev}/resources/fonts/NotoSansSC-Regular.otf";
    sha256 = "0i5i0sh5l7asa784f6h32ys8rs87mlfakbl0h45nzdjlfk4312xn";
  });
  fixedsys-font =
    "${fixedsys-excelsior}/share/fonts/truetype/${fixedsys-excelsior.name}.ttf";
  dotted-chinese-font = "${
      fetchzip {
        name = "dotted-chinese-font";
        url = "https://raw.githubusercontent.com/wixette/dotted-chinese-fonts/${dottedRev}/release/v_0_1/dotted_songti_v_0_1.zip";
        sha256 = "10aah24swi9am4lv9vm75nkr3hffmqsdmkxpi84dq0pzgqbp1ija";
      }
    }/DottedSongtiSquareRegular.otf";

  # Javascript and CSS
  static = let
    hljs = "https://cdnjs.cloudflare.com/ajax/libs/highlight.js/10.3.2";
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
    ];
  in runCommand "static" { } ''
    mkdir -p $out
    ${concatMapStringsSep "\n" (c: "cat ${c} >> $out/style.css") css}
    ${concatMapStringsSep "\n" (j: "cat ${j} >> $out/script.js") js}
  '';

  # Entry
  speak = writeShellScript "speak" ''
    # <<<sh>>>
    input=$1
    output=$2
    mktemp=${coreutils}/bin/mktemp
    content=$($mktemp -d)
    generated=$($mktemp -d)
    codes=$($mktemp)
    pandoc=${pandoc}/bin/pandoc

    for f in $input/*;do
      fname=$(${coreutils}/bin/basename $f)
      $pandoc -s "$f" -t json|${pangu}|$pandoc -s -f json -o "$content/$fname"
      $pandoc -s "$f" -t json|${extractCode} > "$codes"
    done

    ${generator}/bin/speech --input-dir "$content" --output-dir "$output"
    cp -r --no-preserve=all "${static}/." "$output/static"

    cat ${
      ./code/generator/src/Page.hs
    } "$content"/* | ${shrinkFont} ${tsuku-font}        "$output/static/pageFont.woff2"
    cat "$codes" | ${shrinkFont} ${fixedsys-font}       "$output/static/fixedsys.woff2"
    cat "$codes" | ${shrinkFont} ${dotted-chinese-font} "$output/static/song.woff2"
    # >>>sh<<<
  '';
in speak
