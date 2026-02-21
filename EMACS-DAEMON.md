# Emacs Daemon Setup (macOS)

This documents how the Emacs daemon is configured for instant startup on this machine.

## How it works

A **launchd service** starts Emacs in daemon mode automatically at login. The daemon
loads the full Doom config once in the background. After that, `emacsclient` connects
to the already-running daemon, giving you a fully configured Emacs in under 0.5 seconds.

### The warmup frame trick

Doom Emacs defers a lot of initialization (fonts, theme, package loading) until the
first frame is created and the first key is pressed. In daemon mode this means the
first `emacsclient -nw` connection gets a blank screen and hangs until a keypress
triggers all the deferred init.

The fix (in `config.el`) has four essential pieces:

1. **Clear `server-after-make-frame-hook`** — Doom populates this hook with blocking
   "chainers" that wait for a keypress before completing initialization. Nilling the
   hook prevents the blank-screen-waiting-for-keypress problem.
2. **Re-apply theme/fonts on each new frame** via `after-make-frame-functions` — since
   terminal frames are created fresh by each `emacsclient -nw` connection, the theme
   and fonts must be applied per-frame.
3. **Fundamental-mode fix** via `window-buffer-change-functions` — the first file opened
   in a new session can land in `fundamental-mode` instead of the correct major mode.
   A hook detects this and calls `normal-mode` to re-apply.
4. **Warmup frame** — create and immediately destroy an invisible GUI frame during daemon
   init. This exercises all of Doom's deferred first-frame codepaths (font init, theme
   init, deferred package loading via `doom-first-*-hook`) before any real client connects.

Additionally, `persp-mode` (workspaces) session persistence is disabled to prevent
auto-save/restore of workspace layouts.

## Components

### 1. LaunchAgent plist

**File:** `~/Library/LaunchAgents/homebrew.mxcl.emacs-plus@30.plist`

- Runs `/Applications/Emacs.app/Contents/MacOS/Emacs --fg-daemon` at login
- `KeepAlive: true` -- restarts the daemon if it crashes
- `RunAtLoad: true` -- starts automatically on login (no manual action needed)
- Logs: `/tmp/homebrew.mxcl.emacs-plus.stderr.log`

**Important:** If you reinstall `emacs-plus` via brew, the binary path may change.
Update `ProgramArguments` in the plist to match the new location.

### 2. Shell aliases & functions

**File:** `~/dev/dotfiles/zsh/.zshaliases`

```sh
# Opens dired in current dir (no args) or files (with args)
e() {
  if [ $# -eq 0 ]; then
    emacsclient -nw -a '' .
  else
    emacsclient -nw -a '' "$@"
  fi
}
alias ec="emacsclient -nw -a ''"
alias magit='emacsclient -nw -a "" --eval "(magit-status)"'
alias org='emacsclient -nw -a "" --eval "(+default/find-in-notes)"'
```

The `-a ''` flag is the safety net: if the daemon isn't running, emacsclient will
automatically start it. The first connection in that case takes a few seconds while
the daemon loads; every subsequent one is instant.

### 3. Shell exports

**File:** `~/dev/dotfiles/zsh/.zshexports`

```sh
export EDITOR="emacsclient -nw -a ''"
export GIT_EDITOR="emacsclient -nw -a ''"
export MANPAGER="emacsclient -nw -a '' --eval '(let ((b (man \"-l -\"))) (select-window (get-buffer-window b)))'"
export PATH="$PATH:$HOME/.config/emacs/bin"
```

This makes `e` the default editor for `git commit`, `Ctrl-g` in Claude, `man` pages,
and any other tool that respects `$EDITOR`. The PATH entry makes `doom` CLI commands
available from any terminal.

### 4. Daemon config in config.el

The `(when (daemonp) ...)` block in `config.el` handles:
- Clearing `server-after-make-frame-hook` (removes blocking chainers)
- Re-applying theme/fonts on each new client frame
- Fixing fundamental-mode on first opened buffer
- Warmup frame creation/destruction (exercises all deferred init)

## First boot after login

The launchd service starts the daemon immediately at login (`RunAtLoad: true`).
Doom's config loads in ~1.2 seconds. By the time you open a terminal and type `e`,
the daemon is ready and the warmup frame has already exercised all initialization.

**In practice: `e` is instant from the very first terminal you open.**

## Maintenance

| Task | Command |
|------|---------|
| Check daemon status | `emacsclient -e '(emacs-version)'` |
| Restart daemon | `launchctl kickstart -k gui/$(id -u)/homebrew.mxcl.emacs-plus@30` |
| Stop daemon | `launchctl bootout gui/$(id -u)/homebrew.mxcl.emacs-plus@30` |
| Start daemon | `launchctl bootstrap gui/$(id -u) ~/Library/LaunchAgents/homebrew.mxcl.emacs-plus@30.plist` |
| Reload after config change | `doom sync` then restart daemon (or `SPC h r r` in Emacs) |
| View daemon logs | `cat /tmp/homebrew.mxcl.emacs-plus.stderr.log` |

## Caveats & Good to Know

- **Config changes require a reload.** The daemon loads your Doom config once at startup.
  If you edit `config.el`, `+keybindings.el`, etc., the running daemon won't pick up the
  changes. You have two options:
  - **From inside Emacs:** `SPC h r r` (`doom/reload`) -- reloads most config without
    restarting. Works for variable changes, keybinding tweaks, and theme adjustments.
  - **Full restart:** needed after `doom sync` (new packages, module changes in `init.el`).
    Use `launchctl kickstart -k gui/$(id -u)/homebrew.mxcl.emacs-plus@30` to restart
    the daemon cleanly.

- **`doom sync` alone is not enough.** Running `doom sync` updates packages on disk but
  the daemon is still running the old code in memory. Always restart the daemon after
  `doom sync`.

- **All clients share one Emacs process.** Every `e`, `magit`, `Ctrl-g` editor session,
  and `man` page connects to the same daemon. This means:
  - Buffers are shared -- a file opened in one terminal is visible in another.
  - Killing a buffer in one client affects all clients.
  - Variables and state are global across all connections.
  - This is a feature (shared clipboard, undo history, etc.) but can be surprising.

- **Closing a terminal frame doesn't kill buffers.** When you press `q` or `C-x 5 0` in
  an emacsclient frame, it just closes that frame. The daemon keeps running with all your
  buffers intact. Use `SPC b d` (or `C-x k`) to actually kill a buffer.

- **Environment variables are captured at daemon start.** The daemon inherits the
  environment from launchd, not from your shell. `exec-path-from-shell` (in the config)
  mitigates this by sourcing your shell's PATH at init. But if you add a new env var to
  `.zshexports`, you'll need to restart the daemon to pick it up.

- **Theme may look different in terminal vs GUI.** The daemon can serve both GUI frames
  (`emacsclient -c`) and terminal frames (`emacsclient -nw`). Terminal frames have limited
  color support. If the theme looks off in the terminal, this is expected -- 256-color
  terminals can't reproduce all GUI theme colors exactly.

- **`EDITOR` and `MANPAGER` use the daemon too.** `Ctrl-g` in Claude, `git commit`,
  `man` pages -- these all connect to the same daemon. This is why they're instant, but
  it also means an Emacs crash would affect all of them simultaneously. The `KeepAlive`
  setting in the plist auto-restarts the daemon if this happens.

- **Mouse escape sequence garbage on crash.** If emacsclient disconnects ungracefully,
  you may see raw escape codes (e.g. `35;88;21M35;88;20M...`) dumped to the terminal.
  This is cosmetic -- just press Enter or open a new terminal tab.

## Known Issues

- **First file open requires double-RET.** The very first file you open after connecting
  (e.g. selecting from dired or recentf) may silently fail — nothing happens until you
  repeat the action. This is caused by a Doom hook (`+file-templates-check-h` or similar)
  receiving a nil `buffer-file-name` during the first buffer transition. It's cosmetic and
  only affects the first file open per session.

## Troubleshooting

- **`emacsclient: can't find socket`** -- daemon isn't running. The `-a ''` flag handles
  this automatically, but you can also manually start it with the bootstrap command above.
- **Plist path mismatch** -- if you reinstall emacs-plus via brew, the binary path may
  change. Update the `ProgramArguments` in the plist to match the new location.
- **Config changes not reflected** -- see "Caveats" above. Use `SPC h r r` or restart
  the daemon.
- **Stale environment** -- if a newly installed CLI tool isn't found from within Emacs,
  restart the daemon so `exec-path-from-shell` re-reads your PATH.
- **Native comp errors in logs** -- if you see `emutls_w` or `native-ice` errors in
  the daemon log, the gcc/libgccjit toolchain may need fixing (e.g. `brew reinstall
  libgccjit`). These are non-fatal but may cause brief delays on first use of certain
  functions.
