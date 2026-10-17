A CLI player for a Navidrome server, written entirely in Haskell.

## Login

On first open the player asks for a server URL, a username and a password, and  
uses them to talk to Navidrome (Subsonic API). They are stored in a plain  
config file at `$XDG_CONFIG_HOME/havidrome/config` (password as-is, no  
keyring), so later runs open straight into browsing. A logout action discards  
the stored credentials and returns to the login prompt, where a different  
server or account can be entered. Nothing is hardcoded to a particular server.

A server URL typed with neither `http://` nor `https://` in front of it is  
read as `https://` and what was typed, so a bare host name reaches the server  
the full URL reaches.

If credentials entered at the login screen are rejected, or the server they  
name is unreachable, the error is shown there and the details can be retyped.  
There is no retry loop and no offline mode.

Credentials already stored are checked when the run opens, and a server that  
refuses them puts up the login screen, its three fields empty and the refusal  
on it, instead of the player stopping. Anything else that keeps a run from  
opening the library — a server that cannot be reached, a config file holding  
something other than credentials — leaves the player saying why and stopping.  
Only a logout discards what is stored, so credentials a server refused stay in  
the file until ones it accepts replace them.

## Browsing

A full-screen TUI, built the most idiomatic Haskell way. Every screen, the  
login prompt included, is drawn inside a margin of one blank cell: one column  
at the left and right edges of the terminal, one row at the top and bottom.  
Everything on screen, the bottom strip included, sits inside it. Browsing is  
artist-first:

1. Pick an artist.
2. See all of that artist's albums; pick one.
3. See the album's songs; pick any song.

Every level stays visible as a column of its own, side by side, left to right:  
artists, then the picked artist's albums, then the picked album's songs. The  
artist list opens as a single column on the left. Picking an artist leaves  
that column where it is and puts that artist's albums in a new column to its  
right; picking an album adds the songs in a third column. Going back a level  
removes the rightmost column. The level being browsed is always the rightmost  
column on screen, and it is the one the keys move in. Each column to its left  
shows the row picked in it with the same highlight as the row the keys are on,  
so the whole path reads as highlighted.

Every row of every column keeps one cell for a symbol before its name, with a  
space on either side of it, so the names in a column line up whether or not a  
row carries a symbol. The loading symbol is an animation: one dot going round  
a braille cell — `⠋⠙⠹⠸⠼⠴⠦⠧⠇⠏` — a step every tenth of a second, off the  
player's own clock rather than off the moment a row began waiting, so two rows  
waiting at once are on the same step.

A level takes a moment to arrive, and the row it was asked for says so: from  
Enter on an artist or an album until that level's column appears, the row  
carries the loading symbol. While a column is on its way the keys that move  
within a column, descend a level and go back one are not taken, so nothing  
moves and no second level is asked for; they answer again once it is there.  
Every other key meanwhile reaches what it always reaches: `space`, `n`, `p`  
and the seek keys act on the song that is playing, the MacBook's own media  
keys do the same, `l` logs out and Ctrl+C quits. A level the server will not  
answer for ends that wait exactly as its arrival would: the row stops carrying  
the symbol and the keys answer again. An artist and an album carry no symbol  
other than that one.

Each column's heading (Artists, Albums, Songs) has a horizontal line under it,  
across the column. That line does not join the vertical line between two  
columns where they meet.

Artists are listed alphabetically, albums by year oldest first, songs in album  
order. An album the server gives no year for, and a song it gives no track  
number for, come first in their list — above the oldest album, above the  
first track — ordered among themselves by name. There is no search: artists  
are found by moving through the list.

Keys: arrows or `j`/`k` to move, Enter to select, Esc to go back one level,  
Ctrl+C to quit, `l` to log out. Ctrl+C is the only way out of the player and  
works on every screen, the login prompt included; no letter key quits.

## Playback

Playback starts at the picked song and continues through the remaining songs  
of that album in album order, one after another, without further input. While  
an album is playing, songs from it are only ever played in album order.

Audio is always streamed as the original file as stored on the server (FLAC,  
Opus, whatever it is) — no server-side transcoding; quality is a priority.

While a song is playing, an overlay at the bottom of the screen shows the  
current track name, a bar of how far through the track the audio has come, and  
elapsed/total time. Whenever that bottom strip is on screen, one blank row  
separates it from the columns above. The lists underneath stay live: moving,  
Esc back up to albums or artists, and descending again all leave the audio  
running. Enter on any song starts that song and playback then follows that  
song's album, replacing whatever was playing.

While a song is loading, however it started — picked with Enter, reached by  
`n` or `p`, or the album moving on by itself — the strip shows its elapsed  
time at 0:00, and the song itself says so in the song list. If `space` is  
pressed while the song is loading, it finishes loading paused, and `space`  
again starts the audio from there.

The bottom strip also shows, as a symbol, whether the song is playing or  
paused: one symbol while its audio runs, a different one while it is paused.  
Which symbols they are is the builder's own. While a song is loading the strip  
shows neither.

In the song list the cell before a song's name comes one space after its track  
number. The song playback is on carries a symbol there from the moment  
playback turns to it, whether or not the keys' row is on it: the loading  
symbol while it is loading, one of the builder's own while its audio runs, and  
while it is paused the symbol the strip shows for a paused song. It belongs  
to the song and not to the screen: leaving its album and entering it again  
shows it still carrying its symbol, and when playback moves to another song  
the symbol moves with it. Only the song carries one, never its  
album or its artist in their lists, and with nothing playing no song does.

Controls:

- `space` — pause and resume, from the same position.
- `n` — next song of the album. On the last song, playback ends.
- `p` — previous song of the album. Mid-track it still jumps to the previous  
song, never restarting the current one. On the first song it restarts that  
song.
- left/right — seek 5s; shift+left/right — seek 30s. Seeking clamps to the  
current track: never before its start, never past its end, and never across  
into another song. A seek that lands at the end lets the track finish into the  
next song as normal.
- Ctrl+C and `l` also stop the audio, on the way to quitting and to the login  
prompt respectively.

A MacBook's own play/pause, next and previous keys do what `space`, `n` and  
`p` do, and do it from anywhere on the machine: the player need not be the  
application in front, nor its terminal the window on screen, nor its tmux  
session the attached one. That is macOS's own doing, and nothing of the kind  
is claimed on Linux.

A machine asks the player it believes is playing to play, to pause, to stop  
and to seek of its own accord, and macOS sends one such when a FaceTime call  
starts. None of that reaches the audio; only the three keys do.

When the album's last song finishes (or `n` ends playback), the overlay  
disappears and nothing is playing; the view does not move, so it is still  
whatever list was on screen.

A network failure during playback stops playback and shows the error. A track  
that will not play for a reason other than the network (unsupported or corrupt  
file) is skipped: the error is shown briefly and the next song of the album  
starts. Either error is shown in the bottom strip, in place of the overlay's  
usual contents.

Audio is decoded in Haskell if a good Haskell-native option exists; otherwise  
an external player binary is used, supplied by the Nix build rather than  
installed by the user.

## Journal

The player keeps a journal, and every exception it catches writes a line to  
it. It is a file under `$XDG_STATE_HOME/havidrome`, written on every run.  
What a line says is the builder's own.

## Build &amp; tooling

Haskell only. The project is assembled with `purplenoodlesoop/core-flake`, and  
Nix owns everything around it: dependencies, installation, release builds,  
debug builds and the tests. Nothing needs installing or building outside Nix,  
and nothing the project runs is looked for on the machine: a test that drives  
the player drives the one the build supplies, and fails where it cannot rather  
than passing with nothing run.

## Repository

[https://github.com/purplenoodlesoop/havidrome](https://github.com/purplenoodlesoop/havidrome)

## Release

The repository is public. Its visibility is the operator's to set, on  
GitHub, whenever they choose: it is recorded here so that what is published  
is written down, and never so that anything here may change it back.

Nothing in the repository gives away anything personal about the operator, and nothing  
private: no file, images and their metadata included, and no commit's content,  
message, author or committer, on any branch or tag, now or later. It holds no  
password, no secret, no real account's username or server address, and no  
`Claude-Session:` line. The only personal details it may carry are the ones  
the operator has allowed: the name `purplenoodlesoop` (also the repository's  
owner on GitHub) and the email on the commits, the commits' times with their  
time zone, the copyright line already in LICENSE, and the other traces  
of the tools that build it: `Co-Authored-By` lines, the `loop` genesis commit,  
and the board's card ids and wording in commit messages.

Past commits hold the operator's username on their Navidrome server, its first  
part used as a username, and `Claude-Session:` lines. History is rewritten to  
remove all three. The GitHub repository holding the old history is renamed  
`havidrome-private` and otherwise left as it is. A new repository named  
`havidrome` takes its place at the URL above and carries the rewritten  
history. No commit that held any of the three can be fetched, by a ref or by  
its id, from `havidrome` on GitHub or from the copy of the repository the  
board builds from.

It carries the MIT license, with the copyright line LICENSE already has.

The README opens with a product landing section: a description of the player,  
a list of its features, what it needs (Nix and a Navidrome server), a command  
that installs it with Nix, and screenshots. Everything the README held before  
follows that section, unchanged. The screenshots are a few terminal images,  
each a full screen of the player that shows what it does. The library in them  
is made up: its artists, albums and songs belong to no real collection. Which  
screens, how many, and how they look are the builder's own.

The Haskell packages are versioned under the Haskell package versioning  
policy. Releases, their tags and the entries of `CHANGELOG.md` are numbered  
with semantic versioning instead, a tag being its release's number with a `v`  
before it.

A `CHANGELOG.md` at the repository's root holds an entry per release, newest  
first, each concise and in the builder's own wording. A release is made once  
its work has landed on main: that commit is tagged and a GitHub Release is  
made from the tag. A Release offers no executables to download, and its text  
is concise, the wording the builder's own. A release claims every platform the  
flake builds for when it is made, without the player first being built or run  
on each, and its changelog entry and its Release text each name them.

Three releases exist. 1.0.0.0, under `v1.0.0.0`, came first and stays exactly  
as published, the macOS on x86\_64 it claimed included; nixpkgs has since  
dropped that system. 1.0.0 follows it under `v1.0.0`, claiming Linux on  
x86\_64 and aarch64, and macOS on aarch64, which the landing section names  
too, and saying the x86\_64 macOS claim is gone. 1.1.0 follows that under  
`v1.1.0`, once everything above has landed, claiming the same platforms as  
1.0.0 and naming what has changed since it.

CI, the GitHub page's description and topics, the license, author, maintainer  
and homepage fields of the `.cabal` files, a security policy, a contributing  
guide and issue templates are not part of this release.

## Acceptance

- Nix alone builds the player, provides the dev shell, produces a debug build  
and runs every test; no dependency, build step, test or runtime binary  
requires a tool installed outside Nix, so on a machine with none of them  
installed the player builds, installs — into a home-manager profile as well —  
and plays.
- Every test that drives the player drives the one the build supplies, and a  
build that supplies none fails instead of reporting those tests as passed.
- A first run with no stored credentials asks for server URL, username and  
password; after they are accepted the player lists the server's artists.
- A second run with stored credentials goes straight to the artist list  
without asking again.
- Logging out returns to the credential prompt and leaves no stored  
credentials behind; entering a different server or account then browses that  
account's library.
- Credentials entered at the login screen that are wrong, or that name an  
unreachable server, leave the player on that screen with the error visible and  
the fields retypeable.
- A server URL entered with no scheme in front of it reaches the same server,  
and browses the same library, as that URL entered under `https://`.
- A run whose stored credentials the server refuses opens the login screen,  
its fields empty and the refusal on it, and leaves those credentials stored.
- On the login prompt and at every browsing level, with or without a song  
playing, the terminal's top and bottom rows and its leftmost and rightmost  
columns are blank.
- Artists appear alphabetically; an artist's albums appear by year, oldest  
first; an album's songs appear in album order.
- An album with no year, and a song with no track number, appear first in  
their list, ordered among themselves by name.
- Selecting an artist lists exactly that artist's albums; selecting an album  
lists that album's songs.
- The level being browsed is the rightmost column on screen and every level  
above it is a column to its left: one column at artists, two at albums, three  
at songs. The keys move within the rightmost column only.
- At albums the picked artist, and at songs the picked artist and album, look  
exactly like the row the keys are on in the rightmost column.
- The row under each column's heading is a horizontal line, and on that row  
the vertical line between two columns looks as it does on every other row,  
with no junction piece.
- Going back a level removes the rightmost column and leaves the columns to  
its left exactly as they were, highlighted rows included.
- From Enter on an artist or an album until its column appears, that row  
carries the loading symbol; at no other time does an artist or an album carry  
a symbol.
- A row carrying the loading symbol does not stand still: its cell steps  
through the dot's turns ten times a second for as long as the wait lasts, and  
two rows waiting at the same moment are on the same turn.
- While a column is on its way, the keys that move, descend and go back change  
nothing on screen; once it is there they work as before.
- While a column is on its way, `space`, `n`, `p` and the seek keys act on the  
song that is playing, the MacBook's play/pause, next and previous keys do the  
same, `l` returns to the login prompt and Ctrl+C exits.
- A level the server will not answer for leaves the row it was asked for  
carrying no symbol, and the keys that move, descend and go back answer again.
- Selecting a song plays that song's audio, then plays each subsequent song of  
the same album in album order, with no further input from the user.
- While an album is playing, no song from outside it is ever played, and no  
song of it plays out of album order.
- Each song is fetched as the server's stored original file, with no  
transcoding requested.
- While a song plays, the bottom overlay shows its track name, a progress bar  
whose filled part is how much of the track has played, and the elapsed and  
total time; the time and the bar both advance as it plays.
- With the overlay or an error in the bottom strip, the row between the strip  
and the columns is blank.
- With a song playing, moving in a list, Esc up to albums or artists, and  
descending again all leave the audio running; Enter on another song replaces  
what is playing and continues through that song's album.
- A song that is loading carries the loading symbol in the song list, whether  
it was picked with Enter, reached by `n` or `p`, or started by the album  
moving on, and the strip meanwhile shows its elapsed time at 0:00.
- `space` pressed while a song is loading leaves that song, once loaded,  
paused: it carries the paused symbol, its elapsed time stands at 0:00 and no  
audio is heard, until `space` is pressed again and the song plays from its  
start.
- While a song's audio runs, the bottom strip shows one symbol, and while it  
is paused a different one; `space` switches the strip from one to the other.
- While a song loads the strip shows neither symbol; once loaded it shows the  
playing symbol, or the paused one if `space` was pressed while it loaded.
- The playing song's row in the song list reads its track number, one space,  
the symbol, one space, then the song's name, both when the keys' row is on it  
and when it is not; that symbol is one while its audio runs and another while  
it is paused, the paused one being the character the strip shows while that  
song is paused.
- Every name in a column starts in the same screen column as the other names  
in it, whether or not its row carries a symbol.
- Esc out of the playing song's album and Enter back into it shows that song  
still carrying its symbol; when playback moves to another song, the symbol  
moves to that song.
- No album or artist carries a symbol for the playing song, and with nothing  
playing no song carries one.
- `space` halts audio and freezes the elapsed time and the progress bar;  
pressing it again continues from the same point.
- `n` moves to the next song of the album; on the last song it ends playback.
- `p` moves to the previous song of the album even part-way through a track;  
on the first song it restarts that song.
- Left/right seek the current track by 5s and shift+left/right by 30s, and the  
audio, the elapsed time and the progress bar all move accordingly; a seek past  
either end of the track stops at that end instead of crossing into another  
song.
- Ctrl+C stops the audio and exits; `l` stops the audio and returns to the  
login prompt.
- On a Mac, with a song playing and another application in front of the  
player, the keyboard's play/pause, next and previous keys do what `space`, `n`  
and `p` do.
- A play, a pause, a stop or a seek the machine sends of its own accord — as  
macOS does when a FaceTime call starts — leaves the audio as it was: a held  
song still held and silent, a playing song still playing from where it had  
reached, and the album still able to play on to its end.
- A song that cannot be decoded shows an error briefly and playback continues  
with the next song of the album; a network failure instead stops playback and  
shows the error. Both errors appear in the bottom strip, where the  
overlay's contents otherwise are.
- After the album's last song finishes, the overlay is gone, no audio is  
playing, and the list on screen is unchanged.
- Arrows/`j`/`k` move within a list, Enter descends a level, Esc returns one  
level, `l` logs out.
- Ctrl+C exits the player from every screen, the login prompt included, and no  
letter key exits it from anywhere.
- Every run of the player writes its journal to a file under  
`$XDG_STATE_HOME/havidrome`, and an exception the player catches appears in it  
as a line.
- `havidrome-private`, the repository that held the old history under the  
name `havidrome`, is private.
- The repository carries the MIT license, and its copyright line is the one LICENSE  
already has.
- The README's first section describes the player, lists its features, says  
it needs Nix and a Navidrome server, gives a Nix install command, and shows  
screenshots; below it, the README's earlier contents are unchanged.
- Every screenshot is an image of the terminal showing a whole screen of the  
player, and every artist, album and song in it is made up.
- Every Haskell package in the repository has version 1.1.0.0.
- `CHANGELOG.md` at the repository's root has an entry for every release,  
newest first.
- The landing section, the newest changelog entry and the newest Release's  
text each name the platforms that release claims and no others.
- Each release's tag marks a commit on main that already carries the license,  
the landing section, the changelog and the version, and a GitHub Release for  
that tag exists with no executables attached.
- No file (image metadata included), and no commit's content, message, author  
or committer on any branch or tag, contains a password, a secret, a real  
account's username or server address, a `Claude-Session:` line, or a personal  
detail of the operator other than those Release allows; commits made after  
the release work contain none either.
- No commit that held the operator's Navidrome username, its first part as a  
username, or a `Claude-Session:` line can be fetched from `havidrome` on  
GitHub or from the board's copy of the repository, by any ref or by its id.

