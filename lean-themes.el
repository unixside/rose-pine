;;; lean-themes.el --- -*- lexical-binding: t; -*-
;;; Package-Requires: ((modus-themes "5.2.0") (emacs "27.1"))
;;; Commentary:
;;; Code:
;;;###autoload
(when load-file-name
  (let ((dir (file-name-directory load-file-name)))
    (add-to-list 'custom-theme-load-path dir)))

;;;###autoload
(defcustom lean-themes-to-toggle '(tokyonight-night tokyonight-day)
  "List of two theme symbols to toggle between with `lean-themes-toggle'.
The first element is typically a dark theme, the second a light one."
  :type '(list (symbol :tag "Theme A")
               (symbol :tag "Theme B"))
  :group 'lean-themes)

;;;###autoload
(defun lean-themes-toggle ()
  "Toggle between the two themes in `lean-themes-to-toggle'."
  (interactive)
  (let* ((themes lean-themes-to-toggle)
         (a (car themes))
         (b (cadr themes))
         (current (car custom-enabled-themes)))
    (mapc #'disable-theme custom-enabled-themes)
    (load-theme (if (eq current a) b a) :no-confirm)))

(provide 'lean-themes)
;;; lean-themes.el ends here
