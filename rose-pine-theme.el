;; rose-pine-theme.el --- -*- lexical-binding: t -*-
(require 'modus-themes)

(defvar rose-pine-main-palette
  (modus-themes-generate-palette
   '(
     ;; Base colors
     (bg-main          "#191724")
     (fg-main          "#e0def4")
     (fg-dim           "#9893a5")
     (fg-alt           "#908caa")
     (bg-active        "#26233a")
     (bg-inactive      "#21202e")
     (border           "#3b385c")

     ;; Accent foregrounds - Rose Pine colors
     (red              "#eb6f92")
     (red-warmer       "#ec6082")
     (red-cooler       "#d08090")
     (red-faint        "#d0677d")
     (red-intense      "#f080a0")

     (green            "#31748f")
     (green-warmer     "#4080a0")
     (green-cooler     "#286080")
     (green-faint      "#2a6480")
     (green-intense    "#4080a0")

     (yellow           "#f6c177")
     (yellow-warmer    "#f8c888")
     (yellow-cooler    "#e0b066")
     (yellow-faint     "#e0b066")
     (yellow-intense   "#f8c888")

     (blue             "#9ccfd8")
     (blue-warmer      "#a8d8e0")
     (blue-cooler      "#8ac0d0")
     (blue-faint       "#8ac0d0")
     (blue-intense     "#a8d8e0")

     (magenta           "#c4a7e7")
     (magenta-warmer    "#d0b0e8")
     (magenta-cooler    "#b496d8")
     (magenta-faint    "#b496d8")
     (magenta-intense  "#d0b0e8")

     (cyan             "#ebbcba")
     (cyan-warmer       "#f0c8c5")
     (cyan-cooler       "#d0a9a7")
     (cyan-faint       "#d0a9a7")
     (cyan-intense     "#f0c8c5")

     (comment          "#6e6a86")

     ;; Accent backgrounds
     (bg-red-subtle      "#3d1f2a")
     (bg-red-faint       "#2a1518")
     (bg-red-nuanced     "#2a1518")
     (bg-red-intense     "#5f3040")

     (bg-green-subtle    "#1f2d35")
     (bg-green-faint      "#151f25")
     (bg-green-nuanced    "#151f25")
     (bg-green-intense    "#2f4050")

     (bg-yellow-subtle   "#3d3520")
     (bg-yellow-faint     "#2a2315")
     (bg-yellow-nuanced   "#2a2315")
     (bg-yellow-intense   "#5f5040")

     (bg-blue-subtle      "#1f2d35")
     (bg-blue-faint        "#152030")
     (bg-blue-nuanced      "#152030")
     (bg-blue-intense      "#304050")

     (bg-magenta-subtle   "#2d2535")
     (bg-magenta-faint    "#201820")
     (bg-magenta-nuanced  "#201820")
     (bg-magenta-intense  "#403050")

     (bg-cyan-subtle      "#2d2828")
     (bg-cyan-faint       "#201820")
     (bg-cyan-nuanced     "#201820")
     (bg-cyan-intense     "#403838")

     ;; Special purpose backgrounds
     (bg-dim           "#26233a")
     (bg-completion    "#2d2535")
     (bg-popup         "#1f1d2e")
     (bg-hover         "#2a2538")
     (bg-hover-secondary "#2f3025")
     (bg-hl-line       "#26233a")
     (bg-region        "#3b385c")

     (bg-mode-line-active    "#3b385c")
     (border-mode-line-active "#5a5880")
     (bg-mode-line-inactive  "#26233a")
     (fg-mode-line-inactive  "#6e6a86")
     (border-mode-line-inactive "#3b385c")

     (modeline-err     "#eb6f92")
     (modeline-warning "#f6c177")
     (modeline-info    "#31748f")

     (bg-tab-bar      "#1f1d2e")
     (bg-tab-current  "#191724")
     (bg-tab-other     "#2a2838")

     ;; Diffs
     (bg-added           "#1f3530")
     (bg-added-faint     "#1a2a25")
     (bg-added-refine    "#254535")
     (bg-added-fringe    "#2f6050")
     (fg-added           "#9ccfd8")
     (fg-added-intense   "#a8d8e0")

     (bg-changed         "#3d3520")
     (bg-changed-faint   "#2a2315")
     (bg-changed-refine  "#4f4530")
     (bg-changed-fringe  "#6f5f40")
     (fg-changed         "#f6c177")
     (fg-changed-intense "#f8c888")

     (bg-removed         "#3d1f2a")
     (bg-removed-faint   "#2a1518")
     (bg-removed-refine  "#4f2535")
     (bg-removed-fringe  "#6f3050")
     (fg-removed         "#eb6f92")
     (fg-removed-intense  "#f080a0")

     (bg-diff-context    "#1f1d2e")

     ;; Paren match
     (bg-paren-match    "#4f6070")
     (bg-paren-expression "#352535")

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
  "Color palette of the dark variant of the theme.")

(modus-themes-theme
 'rose-pine
 'rose-pine-themes
 "Rose-Pine theme main variant generate with modus-themes-tool."
 'dark
 'rose-pine-main-palette
 nil
 nil)

(provide-theme 'rose-pine)