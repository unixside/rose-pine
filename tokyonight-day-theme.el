;; tokyonight-day-theme.el --- -*- lexical-binding: t -*-
(require 'modus-themes)

(defvar tokyonight-day-palette
  (modus-themes-generate-palette
   '(
      ;; Base colors
      (bg-main          "#e1e2e7")
      (fg-main          "#3760bf")
      (fg-dim           "#6172b0")
      (fg-alt           "#a8aecb")
      (bg-active        "#c4c8da")
      (bg-inactive      "#d0d5e3")
      (border           "#b4b5b9")

      ;; Accent foregrounds - TokyoNight Day colors
      (red              "#f52a65")
      (red-warmer       "#f83a72")
      (red-cooler       "#e82058")
      (red-faint        "#d06075")
      (red-intense      "#ff3a70")

      (green            "#587539")
      (green-warmer     "#608040")
      (green-cooler     "#506e32")
      (green-faint      "#708858")
      (green-intense    "#608838")

      (yellow           "#8c6c3e")
      (yellow-warmer    "#987644")
      (yellow-cooler    "#806236")
      (yellow-faint     "#a08058")
      (yellow-intense   "#987840")

      (blue             "#2e7de9")
      (blue-warmer      "#3888f0")
      (blue-cooler      "#2872d8")
      (blue-faint       "#5890c0")
      (blue-intense     "#3888f0")

      (magenta           "#9854f1")
      (magenta-warmer    "#a060f8")
      (magenta-cooler    "#8848e0")
      (magenta-faint    "#a878c8")
      (magenta-intense  "#a868ff")

      (cyan             "#007197")
      (cyan-warmer       "#0878a0")
      (cyan-cooler       "#006888")
      (cyan-faint       "#2080a0")
      (cyan-intense     "#0880a8")

      (comment          "#848cb5")

      ;; Accent backgrounds
      (bg-red-subtle      "#fce8ec")
      (bg-red-faint       "#fce8ec")
      (bg-red-nuanced     "#fceeef")
      (bg-red-intense     "#f8c0cc")

      (bg-green-subtle    "#e8f2e4")
      (bg-green-faint      "#e8f2e4")
      (bg-green-nuanced    "#ecf4e8")
      (bg-green-intense    "#c8dab0")

      (bg-yellow-subtle   "#f0ece0")
      (bg-yellow-faint     "#f0ece0")
      (bg-yellow-nuanced   "#f2eee4")
      (bg-yellow-intense   "#e0d0a0")

      (bg-blue-subtle      "#e0e8f4")
      (bg-blue-faint        "#e0e8f4")
      (bg-blue-nuanced      "#e4eaf6")
      (bg-blue-intense      "#b0c4e8")

      (bg-magenta-subtle   "#eee4f6")
      (bg-magenta-faint    "#eee4f6")
      (bg-magenta-nuanced  "#f0e8f8")
      (bg-magenta-intense  "#d0b8e8")

      (bg-cyan-subtle      "#e0e8ee")
      (bg-cyan-faint       "#e0e8ee")
      (bg-cyan-nuanced     "#e4eaee")
      (bg-cyan-intense     "#b0c8d8")

      ;; Special purpose backgrounds
      (bg-dim           "#c4c8da")
      (bg-completion    "#d8dbe8")
      (bg-popup         "#d0d5e3")
      (bg-hover         "#bcc0d0")
      (bg-hover-secondary "#e0e8e0")
      (bg-hl-line       "#c4c8da")
      (bg-region        "#b7c1e3")

      (bg-mode-line-active    "#c0c4d4")
      (border-mode-line-active "#6172b0")
      (bg-mode-line-inactive  "#d0d5e3")
      (fg-mode-line-inactive  "#6172b0")
      (border-mode-line-inactive "#c0c4d4")

      (modeline-err     "#f52a65")
      (modeline-warning "#8c6c3e")
      (modeline-info    "#587539")

      (bg-tab-bar      "#c4c8da")
      (bg-tab-current  "#e1e2e7")
      (bg-tab-other     "#c0c4d4")

      ;; Diffs
      (bg-added           "#d8ece0")
      (bg-added-faint     "#dceee4")
      (bg-added-refine    "#c8dec8")
      (bg-added-fringe    "#a0c8a8")
      (fg-added           "#587539")
      (fg-added-intense   "#608838")

      (bg-changed         "#ece4d0")
      (bg-changed-faint   "#eee8d8")
      (bg-changed-refine  "#d8d0b0")
      (bg-changed-fringe  "#c8b890")
      (fg-changed         "#8c6c3e")
      (fg-changed-intense "#987840")

      (bg-removed         "#fce8ec")
      (bg-removed-faint   "#fceeef")
      (bg-removed-refine  "#e8c8d0")
      (bg-removed-fringe  "#d0a8b0")
      (fg-removed         "#f52a65")
      (fg-removed-intense "#ff3a70")

      (bg-diff-context    "#e1e2e7")

      ;; Paren match
      (bg-paren-match    "#b7c1e3")
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
      (delimiter      cyan)
      (docmarkup      comment)
      (number         yellow)
      (operator       cyan)
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
