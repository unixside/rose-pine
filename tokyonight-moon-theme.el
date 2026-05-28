;; tokyonight-moon-theme.el --- -*- lexical-binding: t -*-
(require 'modus-themes)

(defvar tokyonight-moon-palette
  (modus-themes-generate-palette
   '(
     ;; Base colors
     (bg-main          "#222436")
     (fg-main          "#c8d3f5")
     (fg-dim           "#828bb8")
     (fg-alt           "#7a88cf")
     (bg-active        "#2f334d")
     (bg-inactive      "#1b1d2b")
     (border           "#363a54")

     ;; Accent foregrounds - TokyoNight Moon colors
     (red              "#ff757f")
     (red-warmer       "#ff8d94")
     (red-cooler       "#e86a75")
     (red-faint        "#d8606a")
     (red-intense      "#ff9ea8")

     (green            "#c3e88d")
     (green-warmer     "#c7fb6d")
     (green-cooler     "#a8cf7a")
     (green-faint      "#95b86a")
     (green-intense    "#d0ff90")

     (yellow           "#ffc777")
     (yellow-warmer    "#ffd8ab")
     (yellow-cooler    "#d8a860")
     (yellow-faint     "#c09a55")
     (yellow-intense   "#ffd080")

     (blue             "#82aaff")
     (blue-warmer      "#9ab8ff")
     (blue-cooler      "#6a8ee0")
     (blue-faint       "#5a7ec8")
     (blue-intense     "#90b8ff")

     (magenta           "#c099ff")
     (magenta-warmer    "#caabff")
     (magenta-cooler    "#a888e0")
     (magenta-faint    "#9878c8")
     (magenta-intense  "#d0b8ff")

     (cyan             "#86e1fc")
     (cyan-warmer       "#b2ebff")
     (cyan-cooler       "#6ac0e0")
     (cyan-faint       "#5aa8c8")
     (cyan-intense     "#90e8ff")

     (comment          "#636da6")

     ;; Accent backgrounds
     (bg-red-subtle      "#3d202a")
     (bg-red-faint       "#2a1518")
     (bg-red-nuanced     "#2a1518")
     (bg-red-intense     "#603040")

     (bg-green-subtle    "#203025")
     (bg-green-faint      "#152020")
     (bg-green-nuanced    "#152020")
     (bg-green-intense    "#304040")

     (bg-yellow-subtle   "#3d3020")
     (bg-yellow-faint     "#2a2015")
     (bg-yellow-nuanced   "#2a2015")
     (bg-yellow-intense   "#5f5040")

     (bg-blue-subtle      "#202a38")
     (bg-blue-faint        "#152030")
     (bg-blue-nuanced      "#152030")
     (bg-blue-intense      "#304050")

     (bg-magenta-subtle   "#2d2535")
     (bg-magenta-faint    "#201820")
     (bg-magenta-nuanced  "#201820")
     (bg-magenta-intense  "#403050")

     (bg-cyan-subtle      "#1d2830")
     (bg-cyan-faint       "#152025")
     (bg-cyan-nuanced     "#152025")
     (bg-cyan-intense     "#303840")

     ;; Special purpose backgrounds
     (bg-dim           "#2f334d")
     (bg-completion    "#2d3048")
     (bg-popup         "#1b1d2b")
     (bg-hover         "#252840")
     (bg-hover-secondary "#2d3038")
     (bg-hl-line       "#2f334d")
     (bg-region        "#2d3f76")

     (bg-mode-line-active    "#363a54")
     (border-mode-line-active "#545c7e")
     (bg-mode-line-inactive  "#1b1d2b")
     (fg-mode-line-inactive  "#545c7e")
     (border-mode-line-inactive "#363a54")

     (modeline-err     "#ff757f")
     (modeline-warning "#ffc777")
     (modeline-info    "#c3e88d")

     (bg-tab-bar      "#222436")
     (bg-tab-current  "#222436")
     (bg-tab-other     "#2f334d")

     ;; Diffs
     (bg-added           "#20302a")
     (bg-added-faint     "#1a2825")
     (bg-added-refine    "#253835")
     (bg-added-fringe    "#304050")
     (fg-added           "#c3e88d")
     (fg-added-intense   "#d0ff90")

     (bg-changed         "#3d3020")
     (bg-changed-faint   "#2a2015")
     (bg-changed-refine  "#4f4030")
     (bg-changed-fringe  "#6f5040")
     (fg-changed         "#ffc777")
     (fg-changed-intense "#ffd080")

     (bg-removed         "#3d202a")
     (bg-removed-faint   "#2a1518")
     (bg-removed-refine  "#4f2535")
     (bg-removed-fringe  "#6f3050")
     (fg-removed         "#ff757f")
     (fg-removed-intense "#ff9ea8")

     (bg-diff-context    "#222436")

     ;; Paren match
     (bg-paren-match    "#3d5070")
     (bg-paren-expression "#252840")

     ;; Uncommon pairs
     (bg-clay    "#3d2d28")
     (fg-clay    "#d0a090")

     (bg-ochre   "#3d3520")
     (fg-ochre   "#e0d090")

     (bg-lavender "#302840")
     (fg-lavender "#d0b0e8")

     (bg-sage    "#1f3028")
     (fg-sage    "#a0e0c0")
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

     (builtin        blue)
     (comment        comment)
     (constant       yellow)
     (docstring      green-faint)
     (fnname         blue)
     (fnname-call    blue)
     (keyword        magenta)
     (preprocessor   green)
     (property       fg-main)
     (rx-backslash   magenta)
     (rx-construct   green-faint)
     (string         green)
     (type           cyan)
     (variable       fg-main)
     (variable-use   fg-main)

     (bracket        cyan)
     (delimiter      cyan)
     (docmarkup      comment)
     (number         yellow)
     (operator       cyan)
     (punctuation    cyan)

     (fg-link           green)
     (underline-link    green)
     (fg-link-symbolic  cyan)
     (fg-link-visited   green-faint)
     (underline-link-symbolic cyan)
     (underline-link-visited green-faint)

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

     (fg-prose-code     green)
     (fg-prose-macro    magenta)
     (fg-prose-verbatim green)
     (prose-done        green)
     (prose-todo        yellow)
     (prose-metadata    fg-dim)
     (prose-metadata-value fg-alt)
     (prose-table       fg-alt)
     (prose-table-formula magenta)
     (prose-tag         red)
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
  "Color palette of the lighter dark variant of the theme.")

(modus-themes-theme
 'tokyonight-moon
 'tokyonight-themes
 "TokyoNight theme moon variant generated with modus-themes-tool."
 'dark
 'tokyonight-moon-palette
 nil
 nil)

(provide-theme 'tokyonight-moon)
