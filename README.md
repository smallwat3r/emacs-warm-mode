# warm-mode.el

A global minor mode that warms Emacs colors for nighttime coding. Reduces blue
light and slightly dims colors across all faces, working with any theme.

An alternative is to switch to a different theme at night, but warm-mode
preserves your exact color scheme, just making it warmer. This makes it easier
to adapt without losing familiarity with your theme.

## Screenshots

Examples using `warm-mode-warmth` 0.25 and `warm-mode-dim` 0.9.

**modus-vivendi**
| Disabled | Enabled |
|----------|---------|
| ![modus-vivendi disabled](images/modus-vivendi-disabled.png) | ![modus-vivendi enabled](images/modus-vivendi-enabled.png) |

**modus-operandi**
| Disabled | Enabled |
|----------|---------|
| ![modus-operandi disabled](images/modus-operandi-disabled.png) | ![modus-operandi enabled](images/modus-operandi-enabled.png) |

**creamy**
| Disabled | Enabled |
|----------|---------|
| ![creamy disabled](images/creamy-disabled.png) | ![creamy enabled](images/creamy-enabled.png) |

## Installation

### Manual

Clone this repository and add to your `load-path`:

```elisp
(add-to-list 'load-path "/path/to/emacs-warm-mode")
(require 'warm-mode)
```

## Usage

Toggle warm mode:

```
M-x warm-mode
```

## Customization

| Variable           | Default | Description                                      |
|--------------------|---------|--------------------------------------------------|
| `warm-mode-warmth` | 0.2     | Intensity of warm shift (0.0 to 0.5)             |
| `warm-mode-dim`    | 0.9     | Brightness multiplier (0.5 to 1.0, 1.0 = no dim) |

Example configuration:

```elisp
(use-package warm-mode
  :ensure (:host github :repo "smallwat3r/emacs-warm-mode")
  :custom
  (warm-mode-warmth 0.25)
  (warm-mode-dim 0.9))
```
