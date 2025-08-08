# CLAUDE.md

This file provides guidance to Claude Code (claude.ai/code) when working with code in this repository.

## Commands

### Doom Emacs Management
- `doom sync` - Synchronize package changes after modifying `init.el` or `packages.el`
- `doom upgrade` - Update Doom Emacs to the latest version
- `doom doctor` - Check for configuration issues and missing dependencies
- `doom reload` - Reload configuration without restarting Emacs

### Package Management
- Add packages to `packages.el` and run `doom sync`
- Package declarations use the format: `(package! package-name)`
- Custom package recipes can be specified with `:recipe` parameter

## Architecture

### Configuration Structure
This is a modular Doom Emacs configuration split across several files:

- **`init.el`** - Controls which Doom modules are enabled and their configuration flags
- **`config.el`** - Main configuration file that loads other configuration modules
- **`packages.el`** - Package declarations and custom packages
- **`+functions.el`** - Custom utility functions, particularly for git operations and Obsidian integration
- **`+keybindings.el`** - Custom key mappings organized by prefixes (leader keys)
- **`+org-minimal.el`** - Minimal org-mode configuration with agenda and capture templates
- **`+theme.el`** - Gruvbox theme customizations for org-mode and doom-modeline

### Key Features
- Uses Evil mode (Vim keybindings) with `+everywhere` flag
- Vertico completion framework instead of Ivy/Helm
- LSP support enabled for Go, Java, and JavaScript
- Magit for Git integration with delta for better diffs
- Org-mode with roam2 for note-taking
- Custom Obsidian integration functions for cross-application workflow
- Winum for window numbering and navigation

### Module Configuration
The configuration enables specific Doom modules:
- **Completion:** company, vertico
- **UI:** doom, doom-dashboard, modeline, popup, vc-gutter, workspaces, zen
- **Editor:** evil, file-templates, fold, format (+onsave), multiple-cursors, snippets
- **Tools:** eval, lookup, lsp, magit, tree-sitter
- **Languages:** emacs-lisp, go (+lsp), java (+lsp), javascript (+lsp +tree-sitter), markdown, nix, org (+pretty +dragndrop +roam2), sh

### Custom Functions
Located in `+functions.el`:
- Git utilities: `git-root()`, `current-file-relative-path()`, `current-line-number()`
- Obsidian integration: `create-obsidian-uri()`, `open-in-obsidian()` for cross-app navigation

### Keybinding Organization
Custom keybindings are organized under leader key prefixes in `+keybindings.el`:
- `SPC a` - Applications (ag, dired, eshell, terminal)
- `SPC f` - File operations  
- `SPC g` - Git operations
- `SPC o` - Open/launch actions
- `SPC p` - Project operations
- `SPC w` - Window management
- `SPC 1-5` - Window navigation via winum

### Theme Customization
The Gruvbox theme is extensively customized in `+theme.el` with specific face definitions for:
- Org-mode elements (blocks, headers, links, etc.)
- Doom modeline states
- Mail client (mu4e) faces

### Development Workflow
- C++ files are configured with custom compile commands and templates
- Tree-sitter is configured for enhanced syntax highlighting
- Exec-path-from-shell ensures proper PATH handling on macOS
- Claude Code IDE integration is enabled for AI-assisted development