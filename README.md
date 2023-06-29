# Tabnine.el

Tabnine is a minor mode for the tabnine completion engine.

**Warning:** This Tabnine minor mode is unofficial, to fully exploit this minor mode you need a Tabnine pro account as the main reason for creating this minor mode is to make multi-line completions available in EMACS.

**Note:** Currently this is only supported on Linux and all configurations below are for Doom, this has only been tested with doom.

## Installation

Ensure that you are using Emacs 28 with Doom

Add to packages.el

```elisp
(package! tabnine 
    :recipe (:host github :repo "st1971/tabnine-mode"))
```

Configure doom

Configure copilot in `~/.doom.d/config.el`:

```elisp
;; Remove tab and shift tab from company mapping
(map! :after company
      :map company-active-map
      "TAB" nil
      "<tab>" nil
      "S-TAB" nil
      "S-<tab>" nil)

;; accept completion from copilot and fallback to company
(map! :after tabnine
      :map tabnine-active-map
      "C-g" #'tabnine-abort
      "<tab>" #'tabnine-next-completion
      "C-<tab>" #'tabnine-accept-completion
      )
      
;; add hook to automatically enable in modes
(add-hook! 'java-mode-hook (tabnine-mode 1))
```

Strongly recommend to enable `childframe` option in `company` module (`(company +childframe)`) to prevent overlay conflict.

These projects helped me a lot, without them this would have been impossible for me to create:

+ https://github.com/TommyX12/company-tabnine/
+ https://github.com/zerolfx/copilot.el
