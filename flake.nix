{
  description = "ojoregex — OJO regex categorization tools (R package)";

  inputs = {
    nixpkgs.url = "github:rstats-on-nix/nixpkgs/r-daily";
    flake-parts.url = "github:hercules-ci/flake-parts";
    flake-parts.inputs.nixpkgs-lib.follows = "nixpkgs";
    # Suggests dependency; a private sibling package. Needed so `R CMD check`
    # does not flag it as "suggested but not available".
    ojodb = {
      url = "github:openjusticeok/ojodb/ojodb-v3";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = inputs@{ flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      systems = [ "x86_64-linux" "aarch64-linux" "aarch64-darwin" ];

      perSystem = { pkgs, system, ... }:
        let
          # Runtime deps (DESCRIPTION: Imports).
          rImports = with pkgs.rPackages; [
            cli
            dplyr
            lubridate
            purrr
            readr
            rlang
            stringi
            stringr
          ];

          # Suggests (public) so `R CMD check` finds them.
          rSuggests = with pkgs.rPackages; [
            googlesheets4
            gt
            here
            usethis
          ];

          rDev = with pkgs.rPackages; [
            knitr
            rmarkdown
            devtools
            roxygen2
            testthat
            rcmdcheck
            pkgload
            pkgbuild
          ];

          ojodb = inputs.ojodb.packages.${system}.ojodb;

          ojoregex = pkgs.rPackages.buildRPackage {
            name = "ojoregex";
            src = ./.;
            propagatedBuildInputs = rImports;
            nativeBuildInputs = with pkgs.rPackages; [ knitr rmarkdown ];
          };
        in
        {
          packages = {
            inherit ojoregex;
            default = ojoregex;
          };

          devShells.default = pkgs.mkShell {
            name = "ojoregex-dev";
            buildInputs = [ pkgs.R ] ++ rImports ++ rSuggests ++ rDev ++ [ ojodb ];
            packages = with pkgs; [ jarl arf air-formatter ];
            shellHook = ''
              echo "ojoregex dev shell — R -e 'devtools::check()'"
            '';
          };
        };
    };
}
