nix-shell -p "haskellPackages.ghcWithPackages (pkgs: with pkgs; [sbv fgl generic-random QuickCheck brick fgl-arbitrary hspec diagrams palette z3])"