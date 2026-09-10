# havidrome

A CLI player for a [Navidrome](https://www.navidrome.org) server, written in
Haskell. At this stage the executable only starts and exits.

Nix owns everything: the compiler, the dependencies, the builds and the tests.
Nothing else needs installing.

## Build and run

```sh
nix build            # ./result/bin/havidrome, a release build
nix run              # build and run it in one step
```

## Debug build

```sh
nix build .#havidrome-debug -o result-debug
```

The debug executable is compiled without optimisation and keeps its DWARF
symbols, so a debugger can follow it; the release one is stripped. `file` tells
them apart:

```
result/bin/havidrome:       ... stripped
result-debug/bin/havidrome: ... with debug_info, not stripped
```

## Tests

```sh
nix flake check      # builds the package and runs its test suite
```

## Development

```sh
nix develop          # a shell with GHC and cabal, dependencies already present
cabal build
cabal test
```

With [direnv](https://direnv.net) the shell is entered on `cd` — `direnv allow`
once; `.envrc` is already here.

## Layout

- `src/` — the library, everything the player is made of.
- `app/` — the `havidrome` executable, a thin entry point.
- `test/` — the test suite.
- `nix/havidrome.nix` — the flake's per-system module: packages, checks, shell.

The flake is assembled with [`core-flake`](https://github.com/purplenoodlesoop/core-flake).
