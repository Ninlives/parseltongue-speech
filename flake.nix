{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs";
  inputs.rib = {
    url = "github:srid/rib";
    flake = false;
  };
  inputs.data.url = "github:Ninlives/data";

  outputs = { self, nixpkgs, rib, data }:
    with nixpkgs.lib;
    let
      name = "parseltongue";
      dottedRev = "d8c8fac5d32174fb7d261e0d58ba0aa07098e7b8";

      pkgs = nixpkgs.legacyPackages.x86_64-linux;

      inherit (pkgs)
        writeText writeShellScript fetchurl fetchzip runCommand coreutils pandoc
        fixedsys-excelsior babelstone-han nixFlakes;
      inherit (pkgs.python3Packages) pandocfilters fontforge;
      inherit (pkgs.writers) writePython3;
      inherit (pkgs.lib) concatMapStringsSep;

      # Main generator
      generator = (scopedImport {
        import = f:
          if hasSuffix "x3z0q73fr4iryg7x3jspj84ld11kgjyl-source" f then
            (_: import f { inherit (nixpkgs) lib; })
          else
            import f;
        builtins = builtins // { currentSystem = "x86_64-linux"; };
      } rib {
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
      mainFont = data.content.resources + "/fonts/NotoSansSC-Regular.otf";
      fixedsysFont =
        "${fixedsys-excelsior}/share/fonts/truetype/${fixedsys-excelsior.name}.ttf";
      dottedChineseFont = "${
          fetchzip {
            name = "dotted-chinese-font";
            url =
              "https://raw.githubusercontent.com/wixette/dotted-chinese-fonts/${dottedRev}/release/v_0_1/dotted_songti_v_0_1.zip";
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
            url =
              "https://cdnjs.cloudflare.com/ajax/libs/pangu/4.0.7/pangu.min.js";
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
        } "$content"/* | ${shrinkFont} ${mainFont}        "$output/static/pageFont.woff2"
        cat "$codes" | ${shrinkFont} ${fixedsysFont}       "$output/static/fixedsys.woff2"
        cat "$codes" | ${shrinkFont} ${dottedChineseFont} "$output/static/song.woff2"
        # >>>sh<<<
      '';
    in {
      apps.x86_64-linux.speak = {
        type = "app";
        program = "${writeShellScript "speak" ''
          export PAHT=${makeBinPath [ coreutils nixFlakes ]}
          mkdir -p .cache
          nix build ${speak} --out-link .cache/speak
          if [[ -d "./docs" ]];then
          rm -rf "./docs"
          fi
          ${speak} "./markdown" "./docs"
        ''}";
      };
    };
}
