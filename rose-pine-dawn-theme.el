;; rose-pine-dawn-theme.el --- -*- lexical-binding: t -*-
(require 'modus-themes)

(defvar rose-pine-dawn-palette
  (modus-themes-generate-palette
   '(
     ;; Base colors
     (bg-main          "#faf4ed")
     (fg-main          "#575279")
     (fg-dim           "#797593")
     (fg-alt           "#797593")
     (bg-active        "#f2e9e1")
     (bg-inactive      "#f4ede8")
     (border           "#d8d0e5")

     ;; Accent foregrounds - Rose Pine Dawn colors
     (red              "#b4637a")
     (red-warmer       "#c07085")
     (red-cooler       "#a05870")
     (red-faint        "#a05870")
     (red-intense      "#c07085")

     (green            "#286983")
     (green-warmer     "#3878a0")
     (green-cooler     "#206078")
     (green-faint      "#206078")
     (green-intense    "#3878a0")

     (yellow           "#ea9d34")
     (yellow-warmer    "#f0a840")
     (yellow-cooler    "#d89030")
     (yellow-faint     "#d89030")
     (yellow-intense   "#f0a840")

     (blue             "#56949f")
     (blue-warmer      "#68a5b0")
     (blue-cooler      "#4888a0")
     (blue-faint       "#4888a0")
     (blue-intense     "#68a5b0")

     (magenta           "#907aa9")
     (magenta-warmer    "#a088b8")
     (magenta-cooler    "#807098")
     (magenta-faint    "#807098")
     (magenta-intense  "#a088b8")

     (cyan             "#d7827e")
     (cyan-warmer       "#e09088")
     (cyan-cooler       "#c87875")
     (cyan-faint       "#c87875")
     (cyan-intense     "#e09088")

     (comment          "#9893a5")

     ;; Accent backgrounds
     (bg-red-subtle      "#f5d5de")
     (bg-red-faint       "#f5d5de")
     (bg-red-nuanced     "#f8e0e5")
     (bg-red-intense     "#f0b0c0")

     (bg-green-subtle    "#d0e8f0")
     (bg-green-faint      "#d0e8f0")
     (bg-green-nuanced    "#e0f0f5")
     (bg-green-intense    "#a0d8e0")

     (bg-yellow-subtle   "#f5ead5")
     (bg-yellow-faint     "#f5ead5")
     (bg-yellow-nuanced   "#f8efe0")
     (bg-yellow-intense   "#f0d890")

     (bg-blue-subtle      "#d5e8ec")
     (bg-blue-faint        "#d5e8ec")
     (bg-blue-nuanced      "#e0f0f5")
     (bg-blue-intense      "#a0d0d8")

     (bg-magenta-subtle   "#e5d8f0")
     (bg-magenta-faint    "#e5d8f0")
     (bg-magenta-nuanced  "#ece0f5")
     (bg-magenta-intense  "#c8b0d8")

     (bg-cyan-subtle      "#ead5d5")
     (bg-cyan-faint       "#ead5d5")
     (bg-cyan-nuanced     "#f0e0e0")
     (bg-cyan-intense     "#d8b0b0")

     ;; Special purpose backgrounds
     (bg-dim           "#f4ede8")
     (bg-completion    "#e5d8f0")
     (bg-popup         "#faf4ed")
     (bg-hover         "#f0e8e0")
     (bg-hover-secondary "#f5f0e0")
     (bg-hl-line       "#f0e8e0")
     (bg-region        "#e0d8e8")

     (bg-mode-line-active    "#e0d5e5")
     (border-mode-line-active "#a898b8")
     (bg-mode-line-inactive  "#f0e8e5")
     (fg-mode-line-inactive  "#9893a5")
     (border-mode-line-inactive "#d0c8d8")

     (modeline-err     "#b4637a")
     (modeline-warning "#ea9d34")
     (modeline-info    "#286983")

     (bg-tab-bar      "#f0e8e0")
     (bg-tab-current  "#faf4ed")
     (bg-tab-other     "#e0d8d0")

     ;; Diffs
     (bg-added           "#d8f0e0")
     (bg-added-faint     "#e0f5e8")
     (bg-added-refine    "#c8e8d5")
     (bg-added-fringe    "#a0d0b8")
     (fg-added           "#286983")
     (fg-added-intense   "#3878a0")

     (bg-changed         "#f5e8d0")
     (bg-changed-faint   "#f8f0e0")
     (bg-changed-refine  "#f0d8b0")
     (bg-changed-fringe  "#e0c090")
     (fg-changed         "#906028")
     (fg-changed-intense "#a07038")

     (bg-removed         "#f5d8e0")
     (bg-removed-faint   "#f8e0e8")
     (bg-removed-refine  "#f0c0d0")
     (bg-removed-fringe  "#e0a0b0")
     (fg-removed         "#b4637a")
     (fg-removed-intense  "#c07088")

     (bg-diff-context    "#f0ece8")

     ;; Paren match
     (bg-paren-match    "#90d0d8")
     (bg-paren-expression "#e8d5e0")

     ;; Uncommon pairs
     (bg-clay    "#ead8d0")
     (fg-clay    "#704040")

     (bg-ochre   "#f0e8d0")
     (fg-ochre   "#605030")

     (bg-lavender "#e0d0f0")
     (fg-lavender "#504070")

     (bg-sage    "#d8f0e0")
     (fg-sage    "#285050")
     )
   'modus-themes-operandi-tinted-palette
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
  "Color palette of the light variant of the theme.")

(modus-themes-theme
 'rose-pine-dawn
 'rose-pine-themes
 "Rose-Pine theme dawn variant generate with modus-themes-tool."
 'light
 'rose-pine-dawn-palette
 nil
 nil)

(provide-theme 'rose-pine-dawn)