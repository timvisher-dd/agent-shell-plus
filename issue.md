Feature request: desktop notification when prompt is idle

When an agent turn completes and I'm not looking at the buffer, I have no way to know it's waiting for input. Claude Code now sends idle notifications by default, and I'd like agent-shell to do the same.

## Proposed behavior

- After a turn completes, start a 30s timer.
- If the user interacts with the buffer before the timer fires, cancel it.
- Otherwise, send a desktop notification. Also log to the echo area if the shell buffer is not the active buffer.
  - On GUI Emacs/macOS, use native UNUserNotificationCenter notifications via a JIT-compiled dynamic module. ([`vterm`](https://github.com/akermu/emacs-libvterm/blob/a01a2894a1c1e81a39527835a9169e35b7ec5dec/vterm.el#L136-L142) inspired)
    - **Note:** The Homebrew `emacs-app` cask build currently lacks notification entitlements in its hardened-runtime codesigning, so `UNUserNotificationCenter` returns `UNErrorDomain error 1`. We fall back to `NSAppleScript` `display notification` from within Emacs's process (Emacs-branded, no external dependency). See [caldwell-build-emacs-notification-entitlements.issue.md](caldwell-build-emacs-notification-entitlements.issue.md) for the upstream issue.
  - In terminal Emacs, auto-detect the terminal emulator and send the appropriate OSC escape sequence:
    - OSC 9: iTerm2, Ghostty, WezTerm, foot, mintty, ConEmu
    - OSC 99: kitty
    - OSC 777: urxvt, VTE-based terminals (gnome-terminal, etc.)
  - Inside tmux, wrap in DCS passthrough (requiresb `set -g allow-passthrough on`).
  - Fall back to `osascript` on macOS when the terminal is unknown or tmux passthrough is not enabled.

### Windows and Linux

I don't have regular access to either platform, so when the terminal is unknown the current implementation just falls back to the echo area message (no OS-level notification). Happy to accept input on what the right defaults should be there — e.g. `notify-send` on Linux, `powershell` toast notifications on Windows, etc.

## Checklist

- [x] *I agree to communicate with the author myself* (not AI-generated).
- [x] I've read the README's [Filing issues](https://github.com/xenodium/agent-shell?tab=readme-ov-file#filing-issues) section.
- [x] I'm running the latest versions (fill in below).
  - agent-shell: latest main
  - acp.el: latest main
  - ACP package (e.g. claude-code-acp): latest
  - Agent CLI (e.g. claude, gemini): latest
- [ ] ~For requesting new agent support, I'm including a link to the ACP-capable agent or related ACP package.~ N/A
- [ ] ~For issues, I'm including [ACP traffic](https://github.com/xenodium/agent-shell?tab=readme-ov-file#how-do-i-viewget-agent-client-protocol-traffic) (as per README).~ N/A
