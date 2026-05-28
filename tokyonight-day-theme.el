;; tokyonight-day-theme.el --- -*- lexical-binding: t -*-
(require 'modus-themes)

(defvar tokyonight-day-palette
  (modus-themes-generate-palette
   '(
     ;; Base colors
     (bg-main          "#d5d6db")
     (fg-main          "#343b58")
     (fg-dim           "#4c505e")
     (fg-alt           "#828594")
     (bg-active        "#cbccd1")
     (bg-inactive      "#d5d6db")
     (border           "#c1c2c7")

     ;; Accent foregrounds - TokyoNight Day colors
     (red              "#8c4351")
     (red-warmer       "#9a4e5e")
     (red-cooler       "#7a3848")
     (red-faint        "#6e3240")
     (red-intense      "#a85060")

     (green            "#485e30")
     (green-warmer     "#527030")
     (green-cooler     "#3c5028")
     (green-faint      "#354522")
     (green-intense    "#587038")

     (yellow           "#8f5e15")
     (yellow-warmer    "#a07018")
     (yellow-cooler    "#785010")
     (yellow-faint     "#684510")
     (yellow-intense   "#a06820")

     (blue             "#34548a")
     (blue-warmer      "#4060a0")
     (blue-cooler      "#284878")
     (blue-faint       "#224068")
     (blue-intense     "#4068a8")

     (magenta           "#5a4a78")
     (magenta-warmer    "#685488")
     (magenta-cooler    "#4c3c68")
     (magenta-faint    "#40345a")
     (magenta-intense  "#685898")

     (cyan             "#166775")
     (cyan-warmer       "#1e7888")
     (cyan-cooler       "#105868")
     (cyan-faint       "#0e4c58")
     (cyan-intense     "#207888")

     (comment          "#9699a3")

     ;; Accent backgrounds
     (bg-red-subtle      "#f0e0e4")
     (bg-red-faint       "#f0e0e4")
     (bg-red-nuanced     "#f2e8ea")
     (bg-red-intense     "#e0b0bc")

     (bg-green-subtle    "#e4ece0")
     (bg-green-faint      "#e4ece0")
     (bg-green-nuanced    "#e8f0e4")
     (bg-green-intense    "#c0d8b0")

     (bg-yellow-subtle   "#f0ece0")
     (bg-yellow-faint     "#f0ece0")
     (bg-yellow-nuanced   "#f2eee4")
     (bg-yellow-intense   "#e0d0a0")

     (bg-blue-subtle      "#e0e4ec")
     (bg-blue-faint        "#e0e4ec")
     (bg-blue-nuanced      "#e4e8ee")
     (bg-blue-intense      "#b0c0e0")

     (bg-magenta-subtle   "#e8e0f0")
     (bg-magenta-faint    "#e8e0f0")
     (bg-magenta-nuanced  "#ece4f2")
     (bg-magenta-intense  "#c8b0e0")

     (bg-cyan-subtle      "#e0e8ec")
     (bg-cyan-faint       "#e0e8ec")
     (bg-cyan-nuanced     "#e4eaee")
     (bg-cyan-intense     "#b0c8d8")

     ;; Special purpose backgrounds
     (bg-dim           "#cbccd1")
     (bg-completion    "#dde0e6")
     (bg-popup         "#d5d6db")
     (bg-hover         "#c4c5ca")
     (bg-hover-secondary "#e0e8e0")
     (bg-hl-line       "#c4c5ca")
     (bg-region        "#b4bcd0")

     (bg-mode-line-active    "#c0c0c8")
     (border-mode-line-active "#828594")
     (bg-mode-line-inactive  "#d0d0d8")
     (fg-mode-line-inactive  "#828594")
     (border-mode-line-inactive "#c0c0c8")

     (modeline-err     "#8c4351")
     (modeline-warning "#8f5e15")
     (modeline-info    "#485e30")

     (bg-tab-bar      "#cbccd1")
     (bg-tab-current  "#d5d6db")
     (bg-tab-other     "#c0c0c8")

     ;; Diffs
     (bg-added           "#d8e8dc")
     (bg-added-faint     "#dcece0")
     (bg-added-refine    "#c8dcc8")
     (bg-added-fringe    "#a0c8a8")
     (fg-added           "#485e30")
     (fg-added-intense   "#587038")

     (bg-changed         "#ece4d0")
     (bg-changed-faint   "#eee8d8")
     (bg-changed-refine  "#d8d0b0")
     (bg-changed-fringe  "#c8b890")
     (fg-changed         "#8f5e15")
     (fg-changed-intense "#a06820")

     (bg-removed         "#f0e0e4")
     (bg-removed-faint   "#f2e4e8")
     (bg-removed-refine  "#e0c8d0")
     (bg-removed-fringe  "#d0a8b0")
     (fg-removed         "#8c4351")
     (fg-removed-intense "#a85060")

     (bg-diff-context    "#d5d6db")

     ;; Paren match
     (bg-paren-match    "#b8c8e0")
     (bg-paren-expression "#e0d8e8")

     ;; Uncommon pairs
     (bg-clay    "#e8d8d0")
     (fg-clay    "#603838")

     (bg-ochre   "#e8e0c8")
     (fg-ochre   "#504828")

     (bg-lavender "#ddd0f0")
     (fg-lavender "#483868")

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

     (bracket        fg-dim)
     (delimiter      fg-dim)
     (docmarkup      comment)
     (number         yellow)
     (operator       fg-dim)
     (punctuation    fg-dim)

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
     (prose-done        green-faint)
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

     (fg-line-number-inactive fg-alt)
     (fg-line-number-active   fg-main)
     (bg-line-number-inactive bg-dim)
     (bg-line-number-active   bg-active)
     ))
  "Color palette of the light variant of the theme.")

(modus-themes-theme
 'tokyonight-day
 'tokyonight-themes
 "TokyoNight theme day variant generated with modus-themes-tool."
 'light
 'tokyonight-day-palette
 nil
 nil)

(provide-theme 'tokyonight-day)
