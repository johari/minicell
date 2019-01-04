# tmuxinator

```
windows:
  - shell:
    - jcal
    - cowsay hi
  - backend:
    - cd src
    - 'nix-shell -p "haskellPackages.ghcWithPackages (pkgs: with pkgs; [fgl generic-random QuickCheck brick fgl-arbitrary hspec diagrams palette z3 mysql-simple logict hslogger wai warp aeson wai-websockets wai-extra wai-cors fgl-visualize graphviz dhall wreq stache temporary pureMD5])"'
    - runhaskell SimpleServer.hs
  - frontend:
    - make
    - cd build
    - python -m SimpleHTTPServer
```

# Haskell backend

Right now, [`src/SimpleServer.hs`](src/SimpleServer.hs) requires the following nix command to boot up:

```
nix-shell -p "haskellPackages.ghcWithPackages (pkgs: with pkgs; [fgl generic-random QuickCheck brick fgl-arbitrary hspec diagrams palette z3 mysql-simple logict hslogger wai warp aeson wai-websockets wai-extra wai-cors fgl-visualize graphviz dhall wreq stache])"
```

- [x] Works on macOS
- [ ] Works on Linux

After that,

```
$ cd src
$ runhaskell SimpleServer.hs
```

This will boot up a server on port `3000`.

# Elm frontend

```
TODO: Describe how to install Elm
```

after you've made the `build` directory, run

```
$ make
$ cd build/
$ python -m SimpleHTTPServer
```

Then open your web browser to [](http://localhost:8000/pldi.html) to try your local Minicell installation.

# See if everything works

```
$ curl -X post  http://localhost:3000/minicell/A2/write.json -d 'formula=''=SP(A1)'
```

Then goto [](http://localhost:8000/pldi.html).. You should see `A2` calculating the shortest path of the graph stored in `A1`.

