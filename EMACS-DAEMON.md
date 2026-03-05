# Emacs Daemon Setup (macOS)

This documents how the Emacs daemon is configured for instant startup on this machine.

## How it works

A **launchd service** starts Emacs in daemon mode automatically at login. The daemon
loads the full Doom config once in the background. After that, `emacsclient` connects
to the already-running daemon, giving you a fully configured Emacs in under 0.5 seconds.


## Components

### 1. LaunchAgent plist

**File:** `~/Library/LaunchAgents/org.gnu.emacs.daemon.plist`

- Runs `/Applications/Emacs.app/Contents/MacOS/Emacs --fg-daemon` at login
- `KeepAlive: true` -- restarts the daemon if it crashes
- `RunAtLoad: true` -- starts automatically on login (no manual action needed)
- Logs: `/tmp/homebrew.mxcl.emacs-plus.stderr.log`

These are its contents:
```sh
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE plist PUBLIC "-//Apple//DTD PLIST 1.0//EN"
"http://www.apple.com/DTDs/PropertyList-1.0.dtd">
<plist version="1.0">
<dict>

  <key>Label</key>
  <string>org.gnu.emacs.daemon</string>

  <key>ProgramArguments</key>
  <array>
    <string>/opt/homebrew/bin/emacs</string>
    <string>--fg-daemon</string>
  </array>

  <key>RunAtLoad</key>
  <true/>

  <key>KeepAlive</key>
  <true/>

  <key>StandardOutPath</key>
  <string>/tmp/emacs-daemon.log</string>

  <key>StandardErrorPath</key>
  <string>/tmp/emacs-daemon-error.log</string>

</dict>
</plist>
```

**Important:** If you reinstall `emacs-plus` via brew, the binary path may change.
Update `ProgramArguments` in the plist to match the new location.

## First boot after login

The launchd service starts the daemon immediately at login (`RunAtLoad: true`).
Doom's config loads in ~1.2 seconds. By the time you open a terminal and type `e`,
the daemon is ready and the warmup frame has already exercised all initialization.

**In practice: `e` is instant from the very first terminal you open.**

## Maintenance

| Task                       | Command                                                                        |
|----------------------------|--------------------------------------------------------------------------------|
| Check daemon status        | `emacsclient -e '(emacs-version)'`                                             |
| Restart daemon             | `launchctl kickstart -k gui/$(id -u)/org.gnu.emacs.daemon`                     |
| Stop daemon                | `launchctl bootout gui/$(id -u)/org.gnu.emacs.daemon`                          |
| Start daemon               | `launchctl bootstrap gui/$(id -u)/org.gnu.emacs.daemon` |
| Reload after config change | `doom sync` then restart daemon (or `SPC h r r` in Emacs)                      |
| View daemon logs           | `cat /tmp/homebrew.mxcl.emacs-plus.stderr.log`                                 |

## Caveats & Good to Know

- **Config changes require a reload.** The daemon loads your Doom config once at startup.
  If you edit `config.el`, `+keybindings.el`, etc., the running daemon won't pick up the
  changes. You have two options:
  - **From inside Emacs:** `SPC h r r` (`doom/reload`) -- reloads most config without
    restarting. Works for variable changes, keybinding tweaks, and theme adjustments.
  - **Full restart:** needed after `doom sync` (new packages, module changes in `init.el`).
    Use `launchctl kickstart -k gui/$(id -u)/org.gnu.emacs.daemon` to restart
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
