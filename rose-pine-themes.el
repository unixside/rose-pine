;;; rose-pine-themes.el --- -*- lexical-binding: t; -*-
;; Package-Requires: ((modus-themes "5.2.0") (emacs "27.1"))
;;;###autoload
(when load-file-name
  (let ((dir (file-name-directory load-file-name)))
    (add-to-list 'custom-theme-load-path dir)))

(provide 'rose-pine-themes)
