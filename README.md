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

## Configuration

The server URL, username and password are kept in
`$XDG_CONFIG_HOME/havidrome/config` (`~/.config/havidrome/config` when that
variable is unset), one `field=value` line each and the password in the clear —
no keyring, no encryption. The file is the user's own, readable by nobody else;
deleting it discards the credentials.

## Audio

Sound comes from [mpv](https://mpv.io), which the build supplies: the installed
executable carries one on its `PATH`, so nothing has to be installed to play.
It runs with no window, no terminal and none of your own mpv configuration, and
is driven over its JSON IPC.

## Tests

```sh
nix flake check      # builds the package and runs its test suite
```

The tests that drive a real player run it on a null audio output, so they need
no sound device.

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
