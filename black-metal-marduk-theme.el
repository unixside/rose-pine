;; black-metal-marduk-theme.el --- -*- lexical-binding: t -*-
(require 'modus-themes)

(defvar black-metal-marduk-palette
  (modus-themes-generate-palette
   '(
     ;; Base colors
     (bg-main          "#000000")
     (fg-main          "#c1c1c1")
     (fg-dim           "#999999")
     (fg-alt           "#666666")
     (bg-active        "#121212")
     (bg-inactive      "#1a1a1a")
     (border           "#333333")

     ;; Accent foregrounds - Black Metal colors
     (red              "#5f8787")
     (red-warmer       "#6a9090")
     (red-cooler       "#547878")
     (red-faint        "#5a7a7a")
     (red-intense      "#709898")

     (green            "#a5aaa7")
     (green-warmer     "#b5bab7")
     (green-cooler     "#959a97")
     (green-faint      "#959a97")
     (green-intense    "#b5bab7")

     (yellow           "#626b67")
     (yellow-warmer    "#727b77")
     (yellow-cooler    "#525b57")
     (yellow-faint     "#525b57")
     (yellow-intense   "#727b77")

     (blue             "#888888")
     (blue-warmer      "#989898")
     (blue-cooler      "#787878")
     (blue-faint       "#787878")
     (blue-intense     "#989898")

     (magenta           "#999999")
     (magenta-warmer    "#a8a8a8")
     (magenta-cooler    "#888888")
     (magenta-faint    "#888888")
     (magenta-intense  "#a8a8a8")

     (cyan             "#aaaaaa")
     (cyan-warmer       "#b8b8b8")
     (cyan-cooler       "#989898")
     (cyan-faint       "#989898")
     (cyan-intense     "#b8b8b8")

     (comment          "#555555")

     ;; Accent backgrounds
     (bg-red-subtle      "#1a3030")
     (bg-red-faint       "#102020")
     (bg-red-nuanced     "#102020")
     (bg-red-intense     "#2a5050")

     (bg-green-subtle    "#151615")
     (bg-green-faint      "#0d0e0d")
     (bg-green-nuanced    "#0d0e0d")
     (bg-green-intense    "#272827")

     (bg-yellow-subtle   "#151616")
     (bg-yellow-faint     "#0d0e0e")
     (bg-yellow-nuanced   "#0d0e0e")
     (bg-yellow-intense   "#262928")

     (bg-blue-subtle      "#222228")
     (bg-blue-faint        "#181818")
     (bg-blue-nuanced      "#181818")
     (bg-blue-intense      "#383838")

     (bg-magenta-subtle   "#282828")
     (bg-magenta-faint    "#181818")
     (bg-magenta-nuanced  "#181818")
     (bg-magenta-intense  "#404040")

     (bg-cyan-subtle      "#2a2a2a")
     (bg-cyan-faint       "#181818")
     (bg-cyan-nuanced     "#181818")
     (bg-cyan-intense     "#484848")

     ;; Special purpose backgrounds
     (bg-dim           "#121212")
     (bg-completion    "#1a1a1a")
     (bg-popup         "#080808")
     (bg-hover         "#1a1a1a")
     (bg-hover-secondary "#181b1a")
     (bg-hl-line       "#121212")
     (bg-region        "#333333")

     (bg-mode-line-active    "#333333")
     (border-mode-line-active "#555555")
     (bg-mode-line-inactive  "#121212")
     (fg-mode-line-inactive  "#666666")
     (border-mode-line-inactive "#333333")

     (modeline-err     "#5f8787")
     (modeline-warning "#626b67")
     (modeline-info    "#a5aaa7")

     (bg-tab-bar      "#0a0a0a")
     (bg-tab-current  "#000000")
     (bg-tab-other     "#1a1a1a")

     ;; Diffs
     (bg-added           "#111211")
     (bg-added-faint     "#0a0b0a")
     (bg-added-refine    "#171817")
     (bg-added-fringe    "#252625")
     (fg-added           "#a5aaa7")
     (fg-added-intense   "#b5bab7")

     (bg-changed         "#111212")
     (bg-changed-faint   "#0a0b0b")
     (bg-changed-refine  "#161918")
     (bg-changed-fringe  "#242726")
     (fg-changed         "#626b67")
     (fg-changed-intense "#727b77")

     (bg-removed         "#1a2828")
     (bg-removed-faint   "#101a1a")
     (bg-removed-refine  "#203838")
     (bg-removed-fringe  "#305050")
     (fg-removed         "#5f8787")
     (fg-removed-intense  "#709898")

     (bg-diff-context    "#080808")

     ;; Paren match
     (bg-paren-match    "#555555")
     (bg-paren-expression "#1a1a1a")

     ;; Uncommon pairs
     (bg-clay    "#2a1a1a")
     (fg-clay    "#c08080")

     (bg-ochre   "#2a2a1a")
     (fg-ochre   "#c0c080")

     (bg-lavender "#1a1a2a")
     (fg-lavender "#8080c0")

     (bg-sage    "#1a2a1a")
     (fg-sage    "#80c080")
     )
   'modus-themes-vivendi-tinted-palette
   nil
   '(
     (cursor         fg-main)
     (keybind        blue)
     (name           magenta)
     (identifier     fg-alt)

     (err            red)
     (warning        yellow)
     (info           green)
     (underline-err      red-intense)
     (underline-warning yellow-intense)
     (underline-note      green-faint)

     (bg-prominent-err bg-red-intense)
     (fg-prominent-err fg-main)
     (bg-prominent-warning bg-yellow-intense)
     (fg-prominent-warning fg-main)
     (bg-prominent-note bg-cyan-intense)
     (fg-prominent-note fg-main)

     (bg-active-argument bg-yellow-nuanced)
     (fg-active-argument yellow-cooler)
     (bg-active-value bg-cyan-nuanced)
     (fg-active-value cyan-cooler)

     (builtin        red-faint)
     (comment        comment)
     (constant       blue)
     (docstring      yellow-faint)
     (fnname         cyan)
     (fnname-call    cyan)
     (keyword        green)
     (preprocessor   red-cooler)
     (property       cyan)
     (rx-backslash   magenta-faint)
     (rx-construct   green-faint)
     (string         yellow)
     (type           blue)
     (variable       fg-main)
     (variable-use   cyan-faint)

     (bracket        fg-main)
     (delimiter      fg-main)
     (docmarkup      magenta-faint)
     (number         fg-main)
     (operator       fg-alt)
     (punctuation    fg-alt)

     (fg-link           magenta)
     (underline-link    magenta)
     (fg-link-symbolic  cyan)
     (fg-link-visited   magenta-faint)
     (underline-link-symbolic cyan)
     (underline-link-visited magenta-faint)

     (fg-search-current fg-main)
     (fg-search-lazy    fg-main)
     (fg-search-static  fg-main)
     (fg-search-replace fg-main)
     (bg-search-current bg-yellow-intense)
     (bg-search-lazy    bg-cyan-intense)
     (bg-search-static  bg-magenta-subtle)
     (bg-search-replace bg-red-intense)

     (fg-search-rx-group-0 fg-main)
     (fg-search-rx-group-1 fg-main)
     (fg-search-rx-group-2 fg-main)
     (fg-search-rx-group-3 fg-main)
     (bg-search-rx-group-0 bg-blue-intense)
     (bg-search-rx-group-1 bg-green-intense)
     (bg-search-rx-group-2 bg-red-subtle)
     (bg-search-rx-group-3 bg-magenta-subtle)

     (fg-added          green)
     (fg-added-intense  green-intense)
     (fg-removed        red)
     (fg-removed-intense red-intense)
     (fg-changed        yellow)
     (fg-changed-intense yellow-intense)
     (fg-diff-context   fg-dim)

     (fg-prose-code     cyan)
     (fg-prose-macro    magenta)
     (fg-prose-verbatim yellow)
     (prose-done        green)
     (prose-todo        red)
     (prose-metadata    fg-dim)
     (prose-metadata-value fg-alt)
     (prose-table       fg-alt)
     (prose-table-formula magenta)
     (prose-tag         magenta-faint)
     (bg-prose-block-delimiter bg-dim)
     (fg-prose-block-delimiter fg-dim)
     (bg-prose-block-contents  bg-dim)

     (bg-completion-match-0 unspecified)
     (bg-completion-match-1 unspecified)
     (bg-completion-match-2 unspecified)
     (bg-completion-match-3 unspecified)
     (fg-completion-match-0 blue)
     (fg-completion-match-1 magenta)
     (fg-completion-match-2 cyan)
     (fg-completion-match-3 yellow)

     (date-common        cyan)
     (date-deadline      red-cooler)
     (date-deadline-subtle red-faint)
     (date-event        fg-alt)
     (date-holiday      magenta-warmer)
     (date-holiday-other blue)
     (date-range        fg-alt)
     (date-scheduled    yellow-cooler)
     (date-scheduled-subtle yellow-faint)
     (date-weekday      cyan)
     (date-weekend      magenta)
     (date-now          fg-main)

     (mail-cite-0    blue-faint)
     (mail-cite-1    yellow-cooler)
     (mail-cite-2    cyan-cooler)
     (mail-cite-3    red-cooler)
     (mail-part      blue)
     (mail-recipient magenta-cooler)
     (mail-subject   magenta-warmer)
     (mail-other     magenta)

     (bg-mark-delete   bg-red-subtle)
     (fg-mark-delete   red)
     (bg-mark-select   bg-cyan-subtle)
     (fg-mark-select   cyan)
     (bg-mark-other    bg-yellow-subtle)
     (fg-mark-other    yellow)

     (fg-prompt cyan)

     (fg-heading-0 cyan)
     (fg-heading-1 fg-main)
     (fg-heading-2 magenta)
     (fg-heading-3 green)
     (fg-heading-4 blue)
     (fg-heading-5 yellow)
     (fg-heading-6 red)
     (fg-heading-7 magenta-faint)
     (fg-heading-8 fg-dim)

     (accent-0 magenta-cooler)
     (accent-1 cyan)
     (accent-2 magenta-warmer)
     (accent-3 yellow-warmer)

     (fringe bg-main)

     (fg-paren-match fg-main)
     (underline-paren-match unspecified)

     (fg-button-active   fg-main)
     (fg-button-inactive fg-dim)
     (bg-button-active   bg-active)
     (bg-button-inactive bg-dim)

     (fg-region fg-main)

     (fg-mode-line-active fg-main)

     (bg-space unspecified)
     (fg-space border)
     (bg-space-err bg-red-intense)

     (bg-link unspecified)
     (bg-link-symbolic unspecified)
     (bg-link-visited unspecified)
     (bg-prompt unspecified)

     (bg-prose-code unspecified)
     (bg-prose-macro unspecified)
     (bg-prose-verbatim unspecified)

     (rainbow-0 fg-main)
     (rainbow-1 magenta-intense)
     (rainbow-2 cyan-intense)
     (rainbow-3 red-warmer)
     (rainbow-4 yellow-intense)
     (rainbow-5 magenta-cooler)
     (rainbow-6 green-intense)
     (rainbow-7 blue-warmer)
     (rainbow-8 magenta-warmer)

     (fg-line-number-inactive fg-dim)
     (fg-line-number-active   fg-main)
     (bg-line-number-inactive bg-dim)
     (bg-line-number-active   bg-active)
     ))
  "Color palette of the marduk variant of the Black Metal theme.")

(modus-themes-theme
 'black-metal-marduk
 'black-metal-themes
 "Black Metal (Marduk) variant generated with modus-themes-tool."
 'dark
 'black-metal-marduk-palette
 nil
 nil)

(provide-theme 'black-metal-marduk)
