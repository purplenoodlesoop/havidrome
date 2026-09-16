# Changelog

## 1.1.0

- A Mac's media keys reach the player from anywhere on the machine: play/pause,
  next and previous are obeyed with no window in front and no session attached.
- The mark on the song playback is on says what playback is doing with it: it
  is loading, its audio is running, or it is held.
- Picking an artist or an album no longer freezes the player while the server
  answers. The row picked carries a loading symbol until its column arrives,
  the keys that would leave that row wait, and everything else keeps working.
- The bottom strip no longer stands anything in for a song's loading time: the
  song list says it instead.
- A server URL typed with neither `http://` nor `https://` in front of it is
  read as `https://` and what was typed.
- Stored credentials are put to the server when a run opens, and a server that
  refuses them puts up the login screen with the refusal on it instead of the
  player stopping.
- Installing the player with Nix no longer needs mpv on the machine: the build
  carries the mpv the player makes its sound with.

Runs on Linux (x86_64, aarch64) and macOS (aarch64).

## 1.0.0

Nothing about the player changes. Releases, their tags and this file's entries
are numbered with semantic versioning from here on, while the package keeps its
Haskell PVP version, 1.0.0.0.

macOS on x86_64 is no longer claimed: nixpkgs has dropped that system, so the
flake builds nothing for it.

Runs on Linux (x86_64, aarch64) and macOS (aarch64).

## 1.0.0.0

First release: a terminal player for a Navidrome server.

- Log in to any Navidrome server, and log out to switch server or account.
- Browse artists, their albums and an album's songs as side-by-side columns.
- Play an album from any of its songs, streamed as the original file, with
  pause, next, previous and seeking.

Runs on Linux (x86_64, aarch64) and macOS (aarch64).
