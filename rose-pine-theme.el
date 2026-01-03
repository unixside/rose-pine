;; rose-pine-theme.el --- -*- lexical-binding: t -*-
(require 'cl-lib)

(defgroup rose-pine-themes nil
  "All natural pine, faux fur and a bit of soho vibes for the classy minimalist."
  :group 'convenience)

;;;###autoload
(deftheme rose-pine
  "Pastel color scheme with three variants main, moon and dawn"
  :background-mode 'both
  :kind 'color-scheme
  :group 'rose-pine-themes)

(defcustom rose-pine-variant 'dark
  "Default variant to load."
  :type '(choice (const :tag "Dark"  dark)
		 (const :tag "Light" light))
  :group 'rose-pine-themes)

(defcustom rose-pine-dark 'main
  "Dark variant of theme."
  :type '(choice (const :tag "Main" main)
		 (const :tag "Moon" moon))
  :group 'rose-pine-themes)

(defcustom rose-pine-light 'dawn
  "Light variant of theme."
  :type '(choice (const :tag "Dawn" dawn))
  :group 'rose-pine-themes)

(defconst rose-pine-main-palette
  (list :base    "#191724"
	:surface "#1f1d2e"
	:overlay "#26233a"
	:muted   "#6e6a86"
	:subtle  "#908caa"
	:text    "#e0def4"
	:red     "#eb6f92"
	:yellow  "#f6c177"
	:cyan    "#ebbcba"
	:green   "#31748f"
	:blue    "#9ccfd8"
	:magenta "#c4a7e7"
	:hl-low  "#21202e"
	:hl-med  "#403d52"
	:hl-high "#524f67")
  "Color palette of the dark variant of the theme.")

(defconst rose-pine-moon-palette
  (list :base    "#232136"
	:surface "#2a273f"
	:overlay "#393552"
	:muted   "#6e6a86"
	:subtle  "#908caa"
	:text    "#e0def4"
	:red     "#eb6f92"
	:yellow  "#f6c177"
	:cyan    "#ea9a97"
	:green   "#3e8fb0"
	:blue    "#9ccfd8"
	:magenta "#c4a7e7"
	:hl-low  "#2a283e"
	:hl-med  "#44415a"
	:hl-high "#56526e")
  "Color palette of the alternative dark variant of the theme.")

(defconst rose-pine-dawn-palette
  (list :base    "#faf4ed"
	:surface "#fffaf3"
	:overlay "#f2e9e1"
	:muted   "#9893a5"
	:subtle  "#797593"
	:text    "#575279"
	:red     "#b4637a"
	:yellow  "#ea9d34"
	:cyan    "#d7827e"
	:green   "#286983"
	:blue    "#56949f"
	:magenta "#907aa9"
	:hl-low  "#f4ede8"
	:hl-med  "#dfdad9"
	:hl-high "#cecacd")
  "Color palette of light variant of the theme.")

(defun rose-pine--get-variant-palette (variant)
  "Get the color palette for VARIANT (dark or light)."
  (let ((palette-name (if (eq variant 'dark)
                          rose-pine-dark
                        rose-pine-light)))
    (intern (format "rose-pine-%s-palette" palette-name))))

(defun rose-pine-theme-color (name &optional variant)
  "Get color value of `NAME' with `VARIANT' if is present."
  (interactive)
  (let ((variant (or variant rose-pine-variant)))
   (plist-get (symbol-value (rose-pine--get-variant-palette variant)) name)))

(defun rose-pine--plist-to-list (palette)
  "Convert `PALETTE' plist to list ((name color) (name color))."
  (cl-loop for (name color) on palette by #'cddr
	   for symbol = (if (keywordp name)
			    (intern (substring (symbol-name name) 1))
			  name)
	   collect (list symbol color)))

(defmacro rose-pine-with-colors (variant &rest body)
  "Execute BODY with color variables bound from VARIANT palette."
  (declare (indent 1))
  `(let* ((palette (symbol-value (rose-pine--get-variant-palette ,variant)))
          ,@(mapcar (lambda (pair) (list (car pair) `(plist-get palette ,(cadr pair))))
                    '((base :base) (surface :surface) (overlay :overlay)
                      (muted :muted) (subtle :subtle) (text :text)
                      (red :red) (yellow :yellow) (cyan :cyan)
                      (green :green) (blue :blue) (magenta :magenta)
                      (hl-low :hl-low) (hl-med :hl-med) (hl-high :hl-high))))
     ,@body))

(defun rose-pine--generate-face-specs (class palette)
  "Generate face specifications for CLASS using PALETTE."
  (let ((colors (rose-pine--plist-to-list palette)))
    (cl-flet ((color (name) (cadr (assoc name colors))))
      (let ((base    (color 'base))
            (overlay (color 'overlay))
            (muted   (color 'muted))
            (subtle  (color 'subtle))
            (text    (color 'text))
            (red     (color 'red))
            (yellow  (color 'yellow))
            (cyan    (color 'cyan))
            (green   (color 'green))
            (blue    (color 'blue))
            (magenta (color 'magenta))
            (hl-low  (color 'hl-low))
            (hl-med  (color 'hl-med))
            (hl-high (color 'hl-high)))
        `((default                          ((,class (:foreground ,text :background ,base))))
          (cursor                           ((,class (:background ,magenta))))
          (hl-line                          ((,class (:inherit t :background ,hl-low))))
          (fringe                           ((,class (:background ,base))))
          (highlight                        ((,class (:background ,hl-low))))
          (error                            ((,class (:foreground ,red))))
          (warning                          ((,class (:foreground ,yellow))))
          (success                          ((,class (:foreground ,blue))))

          (minibuffer-prompt                ((,class (:foreground ,blue))))
          (link                             ((,class (:foreground ,blue :underline t))))
          (link-visited                     ((,class (:foreground ,magenta :underline t))))

          (region                           ((,class (:background ,hl-med))))
          (show-paren-match                 ((,class (:background ,hl-high))))
          (show-paren-match-expression      ((,class (:background ,hl-med))))
          (show-paren-mismatch              ((,class (:background ,red))))
          
          ;; Line numbers
          (line-number                      ((,class (:foreground ,muted
							         :background ,hl-low))))
          (line-number-current-line         ((,class (:foreground ,magenta
							         :background ,hl-low))))

          ;; Windows dividers
          (window-divider                   ((,class (:foreground ,base
							         :background ,base))))
          (window-divider-first-pixel       ((,class (:foreground ,base
							         :background ,base))))
          (window-divider-last-pixel        ((,class (:foreground ,base
							         :background ,base))))

          ;; Header line
          (header-line                      ((,class (:foreground ,text :background ,overlay))))
          
          ;; My own header-line mode
          (mono-modeline-name-face          ((,class (:inherit (bold italic) :foreground ,text :background ,overlay))))
          (mono-modeline-name-i-face        ((,class (:foreground ,muted :background ,overlay))))
          (mono-modeline-primary-face       ((,class (:foreground ,subtle :background ,overlay))))
          (mono-modeline-secondary-face     ((,class (:inherit (bold italic) :foreground ,muted :background ,overlay))))
          (mono-modeline-text-i-face        ((,class (:foreground ,muted :background ,overlay))))

          ;; Optional appearance for mode-line
          (mono-modeline-thin-face          ((,class (:foreground ,text :background ,text))))
          (mono-modeline-thin-i-face        ((,class (:foreground ,overlay :background ,overlay))))

          ;; PGNA Mode
          (pgna-mode-blunder-face           ((,class (:inherit (bold italic):foreground ,red))))
          (pgna-mode-mistake-face           ((,class (:inherit (bold italic):foreground ,cyan))))
          (pgna-mode-inaccurancy-face       ((,class (:inherit (bold italic):foreground ,yellow))))
          (pgna-mode-excellent-face         ((,class (:inherit (bold italic):foreground ,magenta))))
          (pgna-mode-brilliant-face         ((,class (:inherit (bold italic):foreground ,blue))))
          
          ;; Font locks
          (font-lock-warning-face           ((,class (:foreground ,red))))
          (font-lock-function-name-face     ((,class (:foreground ,cyan))))
          (font-lock-function-call-face     ((,class (:foreground ,blue))))
          (font-lock-variable-name-face     ((,class (:foreground ,text))))
          (font-lock-variable-use-face      ((,class (:foreground ,text))))
          (font-lock-keyword-face           ((,class (:foreground ,green))))
          (font-lock-comment-face           ((,class (:foreground ,muted))))
          (font-lock-comment-delimiter-face ((,class (:foreground ,muted))))
          (font-lock-type-face              ((,class (:foreground ,blue))))
          (font-lock-constant-face          ((,class (:foreground ,blue))))
          (font-lock-builtin-face           ((,class (:foreground ,red))))
          (font-lock-preprocessor-face      ((,class (:foreground ,red))))
          (font-lock-string-face            ((,class (:foreground ,yellow))))
          (font-lock-doc-face               ((,class (:foreground ,magenta))))
          (font-lock-doc-markup-face        ((,class (:foreground ,blue))))
          (font-lock-negation-char-face     ((,class (:foreground ,red))))
          (font-lock-scape-face             ((,class (:foreground ,red))))
          (font-lock-number-face            ((,class (:foreground ,cyan))))
          (font-lock-operator-face          ((,class (:foreground ,subtle))))
          (font-lock-punctuation-face       ((,class (:foreground ,subtle))))
          (font-lock-delimiter-face         ((,class (:foreground ,cyan))))
          (font-lock-property-name-face     ((,class (:foreground ,blue))))
          (font-lock-property-use-face      ((,class (:foreground ,blue))))
          (font-lock-misc-punctuation-face  ((,class (:foreground ,magenta))))

          (sh-quoted-exec                   ((,class (:foreground ,red))))
          
          ;; VTerm mode
          (vterm-color-black          ((,class (:foreground ,muted))))
          (vterm-color-red            ((,class (:foreground ,red))))
          (vterm-color-green          ((,class (:foreground ,green))))
          (vterm-color-yellow         ((,class (:foreground ,yellow))))
          (vterm-color-blue           ((,class (:foreground ,blue))))
          (vterm-color-magenta        ((,class (:foreground ,magenta))))
          (vterm-color-cyan           ((,class (:foreground ,cyan))))
          (vterm-color-white          ((,class (:foreground ,text))))
          (vterm-color-bright-black   ((,class (:foreground ,muted))))
          (vterm-color-bright-red     ((,class (:foreground ,red))))
          (vterm-color-bright-green   ((,class (:foreground ,green))))
          (vterm-color-bright-yellow  ((,class (:foreground ,yellow))))
          (vterm-color-bright-blue    ((,class (:foreground ,blue))))
          (vterm-color-bright-magenta ((,class (:foreground ,magenta))))
          (vterm-color-bright-cyan    ((,class (:foreground ,cyan))))
          (vterm-color-bright-white   ((,class (:foreground ,text))))

          ;; Rainbow delimiters mode
          (rainbow-delimiters-depth-1-face    ((,class (:foreground ,green))))
          (rainbow-delimiters-depth-2-face    ((,class (:foreground ,red))))
          (rainbow-delimiters-depth-3-face    ((,class (:foreground ,magenta))))
          (rainbow-delimiters-depth-4-face    ((,class (:foreground ,yellow))))
          (rainbow-delimiters-depth-5-face    ((,class (:foreground ,blue))))
          (rainbow-delimiters-depth-6-face    ((,class (:foreground ,green))))
          (rainbow-delimiters-depth-7-face    ((,class (:foreground ,red))))
          (rainbow-delimiters-depth-8-face    ((,class (:foreground ,magenta))))
          (rainbow-delimiters-depth-9-face    ((,class (:foreground ,blue))))
          (rainbow-delimiters-unmatched-face  ((,class (:foreground ,muted))))
          (rainbow-delimiters-mismatched-face ((,class (:foreground ,red))))

          ;; Dired full color
          (diredfl-dir-priv    ((,class (:background ,base :foreground ,green))))
          (diredfl-read-priv   ((,class (:background ,base :foreground ,yellow))))
          (diredfl-write-priv  ((,class (:background ,base :foreground ,red))))
          (diredfl-exec-priv   ((,class (:background ,base :foreground ,green))))
          (diredfl-no-priv     ((,class (:background ,base :foreground ,muted))))
          (diredfl-date-time   ((,class (:background ,base :foreground ,blue))))
          (diredfl-dir-heading ((,class (:background ,base :foreground ,text))))
          (diredfl-dir-name    ((,class (:background ,base :foreground ,green))))
          (diredfl-file-name   ((,class (:background ,base :foreground ,text))))
          (diredfl-file-suffix ((,class (:background ,base :foreground ,text))))
          (diredfl-number      ((,class (:background ,base :foreground ,blue))))

          ;; Multiple cursor
          (mc/cursor-face           ((,class (:inherit cursor))))
          (mc/cursor-bar-face       ((,class (:inherit cursor))))
          (mc/region-face           ((,class (:inherit cursor))))
          
          ;; Org Mode Faces
          (variable-pitch ((,class (:foreground ,text))))
          (fixed-pitch    ((,class (:foreground ,text))))

          (org-level-8 ((,class (:foreground ,cyan))))
          (org-level-7 ((,class (:foreground ,yellow))))
          (org-level-6 ((,class (:foreground ,yellow))))
          (org-level-5 ((,class (:foreground ,text))))
          (org-level-4 ((,class (:foreground ,text))))
          (org-level-3 ((,class (:foreground ,cyan))))
          (org-level-2 ((,class (:foreground ,magenta))))
          (org-level-1 ((,class (:foreground ,red))))
          
          (org-document-title            ((,class (:foreground ,text))))
          (org-block                     ((,class (:inherit fixed-pitch :background ,hl-low))))
          (org-code                      ((,class (:inherit default :foreground ,blue))))
          (org-document-info             ((,class (:foreground ,muted ))))
          (org-document-info-keyword     ((,class (:inherit (shadow fixed-pitch)))))
          (org-indent                    ((,class (:inherit (org-hide fixed-pitch)))))
          (org-link                      ((,class (:foreground ,magenta))))
          (org-meta-line                 ((,class (:inherit (font-lock-comment-face fixed-pitch)))))
          (org-property-value            ((,class (:inherit fixed-pitch))) t)
          (org-special-keyword           ((,class (:inherit (font-lock-comment-face fixed-pitch)))))
          (org-table                     ((,class (:inherit fixed-pitch :foreground ,subtle)))) 
          (org-verbatim                  ((,class (:inherit (shadow fixed-pitch) :foreground ,subtle))))
          (org-drawer                    ((,class (:inherit fixed-pitch :foreground ,muted))))
          (org-hide                      ((,class (:inherit fixed-pitch :foreground ,base))))
          (org-tag                       ((,class (:foreground ,muted))))
          (org-todo                      ((,class (:foreground ,red))))
          (org-done                      ((,class (:foreground ,muted))))
          (org-date                      ((,class (:foreground ,muted))))
          (org-scheduled                 ((,class (:foreground ,text))))
          (org-scheduled-previously      ((,class (:foreground ,magenta))))
          (org-scheduled-today           ((,class (:foreground ,text))))
          (org-imminent-deadline         ((,class (:foreground ,red))))
          (org-upcoming-deadline         ((,class (:foreground ,red))))
          (org-upcoming-distant-deadline ((,class (:foreground ,cyan))))
          (org-time-grid                 ((,class (:foreground ,yellow))))
          (org-priority                  ((,class (:foreground ,green))))

          ;; Org Agenda faces
          (org-agenda-structure          ((,class (:foreground ,text))))
          (org-agenda-date               ((,class (:foreground ,subtle))))
          (org-agenda-date-weekend       ((,class (:foreground ,green))))
          (org-agenda-date-weekend-today ((,class (:foreground ,red))))
          (org-agenda-current-time       ((,class (:foreground ,text))))
          (org-agenda-done               ((,class (:foreground ,muted))))
          (org-agenda-calendar-daterange ((,class (:foreground ,text))))
          (org-agenda-calendar-event     ((,class (:foreground ,text))))
          (org-agenda-calendar-sexp      ((,class (:foreground ,text))))
          (org-agenda-clocking           ((,class (:foreground ,text))))
          (org-agenda-column-dateline    ((,class (:foreground ,text)))) 
          (org-agenda-date-today         ((,class (:foreground ,green))))
          (org-agenda-diary              ((,class (:foreground ,text))))
          (org-agenda-dimmed-todo-face   ((,class (:foreground ,text))))
          (org-agenda-filter-category    ((,class (:foreground ,text))))
          (org-agenda-filter-effort      ((,class (:foreground ,text))))
          (org-agenda-filter-regexp      ((,class (:foreground ,text))))
          (org-agenda-filter-tags        ((,class (:foreground ,text))))
          (org-agenda-restriction-lock   ((,class (:foreground ,text))))
          (org-agenda-structure-filter   ((,class (:foreground ,red))))
          (org-agenda-structure-secondar ((,class (:foreground ,text)))))))))

(defun rose-pine--generate-variable-specs (palette)
  "Generate variable specifications using PALETTE."
  (let ((colors (rose-pine--plist-to-list palette)))
    (cl-flet ((color (name) (cadr (assoc name colors))))
      (let ((text    (color 'text))
            (surface (color 'surface))
            (overlay (color 'overlay))
            (muted   (color 'muted))
            (red     (color 'red))
            (blue    (color 'blue)))
        `((mono-modeline-icon-RW-color    ,text)
          (mono-modeline-icon-**-color    ,red)
          (mono-modeline-icon-RO-color    ,blue)
          (mono-modeline-background-color ,overlay)
          (mono-modeline-i-color          ,muted)
          (mono-modeline-thin-color       ,text)
          (mono-modeline-thin-i-color     ,surface)
          (window-divider-mode t))))))

(defun rose-pine--apply-theme ()
  "Apply the theme with the current variant."
  (let* ((class '((class color) (min-colors 256)))
         (palette (symbol-value (rose-pine--get-variant-palette rose-pine-variant)))
         (face-specs (rose-pine--generate-face-specs class palette))
         (var-specs (rose-pine--generate-variable-specs palette)))
    (apply #'custom-theme-set-faces 'rose-pine face-specs)
    (apply #'custom-theme-set-variables 'rose-pine var-specs)))

(defun rose-pine-theme-reload ()
  "Load or reload theme."
  (interactive)
  (mapc #'disable-theme custom-enabled-themes)
  (load-theme 'rose-pine t))

(defun rose-pine-theme-toggle ()
  "Toggle between dark and light variant of rose-pine-theme."
  (interactive)
  (setq rose-pine-variant (if (eq rose-pine-variant 'dark) 'light 'dark))
  (rose-pine-theme-reload)
  (message "Rose Pine: Switched to %s variant (%s)" 
           rose-pine-variant
           (symbol-value (intern (format "rose-pine-%s" rose-pine-variant)))))

;; Apply the theme with the current variant
(rose-pine--apply-theme)

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
	       (file-name-as-directory (file-name-directory load-file-name))))

(provide-theme 'rose-pine)
