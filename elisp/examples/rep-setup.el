;;; rep-setup.el ---

;; Copyright 2010 Joseph Brenner
;;
;; Author: doom@kzsu.stanford.edu
;; Version: $Id: rep-setup.el,v 0.0 2010/05/29 01:17:56 doom Exp $
;; Keywords:
;; X-URL: not distributed yet

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program; if not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.

;;; Commentary:

;;

;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'rep-setup)

;;; Code:

(provide 'rep-setup)
(eval-when-compile
  (require 'cl))




(require 'rep)

;; Add definitions of default locations for things like
;;  *.rep files.

;; the backup files, I think I'm going to force into local .rep subdirs
;; for now.

;; defines binding: Control-c . S
(rep-define-global-key-binding "C-c.")

;; Any other magic to make that binding available everywhere?

;; associate the *.rep extension with my substitutions-mode




;;; rep-setup.el ends here
