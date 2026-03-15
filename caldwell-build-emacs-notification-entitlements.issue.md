Add notification entitlements to Emacs.app codesigning

## Problem

Emacs packages that use `UNUserNotificationCenter` via dynamic modules
cannot send native macOS notifications from the Homebrew `emacs-app` cask
build. `requestAuthorizationWithOptions:` immediately returns
`UNErrorDomain error 1` (`UNErrorCodeNotificationsNotAllowed`) with no
permission dialog ever shown to the user.

## Root cause

The current build is signed with hardened runtime
(`flags=0x10000(runtime)`, `TeamIdentifier=5BRAQAFB8B`) but the
provisioning profile / codesigning does not include notification-related
entitlements. With hardened runtime enabled, macOS enforces notification
capability checks that reject the authorization request before reaching
the "ask the user" stage.

## Evidence

- Built Emacs from source *without* hardened runtime (adhoc signed,
  `flags=0x2`): `UNUserNotificationCenter` works — permission dialog
  appears, notifications delivered.
- Same code in `/Applications/Emacs.app` (from the cask): `UNErrorDomain
  error 1` every time. No permission dialog. Resetting notification
  permissions in System Settings does not help.
- The `com.apple.security.cs.disable-library-validation` entitlement is
  already present (dynamic modules load fine), but it only affects library
  loading, not notification capabilities.

## Current codesign state

```
Identifier=org.gnu.Emacs
Format=app bundle with Mach-O universal (x86_64 arm64)
CodeDirectory v=20500 size=15065 flags=0x10000(runtime)
TeamIdentifier=5BRAQAFB8B
Sealed Resources version=2 rules=13 files=4514

Entitlements:
  com.apple.security.cs.disable-library-validation = true
```

## Requested change

Add notification entitlements to the codesigning so that
`UNUserNotificationCenter` works from within Emacs (either directly or
via dynamic modules). The specific entitlement may be
`com.apple.developer.usernotifications.filtering` or simply enabling the
User Notifications capability in the provisioning profile.

## Use case

Emacs packages like [agent-shell](https://github.com/xenodium/agent-shell)
want to send desktop notifications when an AI agent turn completes and the
user isn't looking at the buffer. The current workaround is
`NSAppleScript` running `display notification` from within Emacs's
process, which works but is the deprecated notification path.

## Target

caldwell/build-emacs: https://github.com/caldwell/build-emacs
