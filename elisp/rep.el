;;; rep.el --- find and replace using perl5

;; Copyright 2010 Joseph Brenner
;;
;; Author: doom@kzsu.stanford.edu
;; Version: $Id: rep.el,v 0.0 2010/05/14 01:49:29 doom Exp $
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
;;   (require 'rep)

;;; Code:

(provide 'rep)
(eval-when-compile
  (require 'cl))



;;;;##########################################################################
;;;;  User Options, Variables
;;;;##########################################################################

;; TODO research how flyspell mode works.
;;      research overlays
;;      review macros again (sigh)

;; TODO DELETE eventually
;; Basic face definition used during development.
(defface rep-changed-face
  '((((class color)
      (background light))
     (:foreground "DarkGoldenrod4"))
    (((class color)
      (background dark))
     (:foreground "DarkGoldenrod2")))
  "Face used for indicating a change made by rep.el"
  :group 'desktop-recover-faces)


;; (set-face-underline-p 'rep-changed-face t)
(set-face-underline-p 'rep-changed-face "green")
;;  Docs: If UNDERLINE is a string, underline with the color named UNDERLINE.


;; TODO get :underline to work at this stage?
;; (defmacro rep-make-face (name number color1 color2)
;;   `(defface ,name
;;   '((((class color)
;;       (background light))
;;      (:foreground ,color1)
;;       (:underline  ,color1)
;;      )
;;     (((class color)
;;       (background dark)
;;       (:foreground ,color2)
;;       (:underline  ,color2)
;;       )
;;      ))
;;   ,(format "Face used for to indicate changes from substitution number: %s." number)
;;   :group 'rep-faces
;;   ))


(defmacro rep-make-face (name number color1 color2)
  `(defface ,name
  '((((class color)
      (background light))
     (:foreground ,color1))
    (((class color)
      (background dark))
     (:foreground ,color2)))
  ,(format "Face used for changes from substitution number: %s." number)
  :group 'desktop-recover-faces
  ))


;; TODO apply underline to these, too?

(rep-make-face rep-00-face 00 "PaleVioletRed4" "PaleVioletRed1")
(rep-make-face rep-01-face 01 "DarkGoldenrod4" "DarkGoldenrod2")
(rep-make-face rep-02-face 02 "MediumPurple4" "MediumPurple1")
(rep-make-face rep-03-face 03 "forest green" "light green")
(rep-make-face rep-04-face 04 "gold4" "gold1")
(rep-make-face rep-05-face 05 "SeaGreen4" "SeaGreen1")
(rep-make-face rep-06-face 06 "green4" "green1")
(rep-make-face rep-07-face 07 "tan4" "tan1")
(rep-make-face rep-08-face 08 "DarkOrchid4" "DarkOrchid1")
(rep-make-face rep-09-face 09 "SpringGreen4" "SpringGreen1")
(rep-make-face rep-10-face 10 "DarkOrange4" "DarkOrange1")
(rep-make-face rep-11-face 11 "salmon4" "salmon1")
(rep-make-face rep-12-face 12 "OliveDrab4" "OliveDrab1")
(rep-make-face rep-13-face 13 "orange4" "orange1")
(rep-make-face rep-14-face 14 "RoyalBlue1" "RoyalBlue1")
(rep-make-face rep-15-face 15 "DarkOliveGreen4" "DarkOliveGreen1")
(rep-make-face rep-16-face 16 "chocolate4" "chocolate1")
(rep-make-face rep-17-face 17 "maroon4" "maroon1")
(rep-make-face rep-18-face 18 "khaki1" "khaki4")
(rep-make-face rep-19-face 19 "CadetBlue4" "CadetBlue1")
(rep-make-face rep-20-face 20 "DarkSeaGreen4" "DarkSeaGreen1")
(rep-make-face rep-21-face 21 "LightSalmon4" "LightSalmon1")
(rep-make-face rep-22-face 22 "DeepSkyBlue4" "DeepSkyBlue1")
(rep-make-face rep-23-face 23 "chartreuse4" "chartreuse1")
(rep-make-face rep-24-face 24 "cyan4" "cyan1")
(rep-make-face rep-25-face 25 "magenta4" "magenta1")
(rep-make-face rep-26-face 26 "blue4" "blue1")
(rep-make-face rep-27-face 27 "DeepPink4" "DeepPink1")
(rep-make-face rep-28-face 28 "aquamarine4" "aquamarine1")
(rep-make-face rep-29-face 29 "coral4" "coral1")
(rep-make-face rep-30-face 30 "PaleGreen4" "PaleGreen1")
(rep-make-face rep-31-face 31 "PeachPuff4" "PeachPuff1")
(rep-make-face rep-32-face 32 "firebrick4" "firebrick1")
(rep-make-face rep-33-face 33 "PeachPuff4" "PeachPuff1")

(defvar rep-face-alist ()
 "Faces keyed by number (an integer to font association).")
;; hardcoded generation of look-up table (stupid, I know, but simple)
(setq rep-face-alist
      '(
        (00 . rep-00-face)
        (01 . rep-01-face)
        (02 . rep-02-face)
        (03 . rep-03-face)
        (04 . rep-04-face)
        (05 . rep-05-face)
        (06 . rep-06-face)
        (07 . rep-07-face)
        (08 . rep-08-face)
        (09 . rep-09-face)
        (10 . rep-10-face)
        (11 . rep-11-face)
        (12 . rep-12-face)
        (13 . rep-13-face)
        (14 . rep-14-face)
        (15 . rep-15-face)
        (16 . rep-16-face)
        (17 . rep-17-face)
        (18 . rep-18-face)
        (19 . rep-19-face)
        (20 . rep-20-face)
        (21 . rep-21-face)
        (22 . rep-22-face)
        (23 . rep-23-face)
        (24 . rep-24-face)
        (25 . rep-25-face)
        (26 . rep-26-face)
        (27 . rep-27-face)
        (28 . rep-28-face)
        (29 . rep-29-face)
        (30 . rep-30-face)
        (31 . rep-31-face)
        (32 . rep-32-face)
        (33 . rep-33-face)
        ))

;; We then look-up a font like so:
;;  (cdr (assoc 1 rep-face-alist))

;; Indirect lookup using number in variable
;;   (setq rep-change-numb 3)
;;   (cdr (assoc rep-change-numb rep-face-alist))

;; DEBUG
(defun rep-change-face-region-by-numb (beg end)
  "Just making sure there's no trouble with face lookup given
a change number."
  (interactive "r")
  (let* ( (rep-change-numb 15)
         (this-change-face (cdr (assoc rep-change-numb rep-face-alist)))
         )
    (put-text-property beg end
                       'face this-change-face
                       (current-buffer))))

;; DEBUG
(defun rep-change-face-region (beg end)
  "Just making sure there's no trouble with my face.
Looks good."
  (interactive "r")
  (put-text-property beg end
                     'face 'rep-changed-face
                     (current-buffer)))

(defun rep-do-these-changes-other-window ()
  "Two buffers must be open, the changes_list and the file to act on,
with the changes_list selected.
Uses the pass number to choose fonts to mark-up changes.
Turns off font-lock to avoid conflict with existing syntax coloring.
"
  (interactive)
  (let* (
          perl-rep-cmd data
          substitution-lines
         ( bak-extension "bak" )  ;; TODO choose unique extension, push info onto a buffer-local stack
;;         ( rep-pl "rep.pl" )    ;; TODO sort out path problem
         ( rep-pl "/home/doom/End/Cave/Rep/Wall/Emacs-Rep/scripts/rep.pl")

          (changes-list-file    (buffer-file-name))
          (changes-list-buffer  (current-buffer)) ;; unused?

          target-file target-file-buffer
         )
         (other-window 1)
         (setq target-file          (buffer-file-name))
         (setq target-file-buffer   (current-buffer))

         (setq perl-rep-cmd
               (format
                "perl %s --extension %s --substitutions %s %s "
                rep-pl
                bak-extension
                changes-list-file
                target-file))
         (setq data (shell-command-to-string perl-rep-cmd))

         (set-buffer target-file-buffer)
         (revert-buffer t t t) ;;last option: "preserve-modes" what does it do?
         (font-lock-mode -1)

         ;; TODO Doesn't allow multi-line 'orig'.
         ;;   Add semi-colons to data format? And escape embedded semi-c?
         ;; split data into lines
         (setq substitution-lines (split-string data "\n" t))
         (dolist (line  substitution-lines)
           (cond ((not (string-equal "" line)) ;; skip blank lines
                  ;; split each line into five fields
                  (let* (
                         (fields (rep-split-limited ":" line 5) )
                          (pass   (string-to-number (nth 0 fields)))
                          (beg    (string-to-number (nth 1 fields)))
                          (end    (string-to-number (nth 2 fields)))
                          (delta  (string-to-number (nth 3 fields))) ;; unused?
                          (orig   (nth 4 fields))
                          (markup-face (rep-lookup-markup-face pass))
                          )
                    (put-text-property beg end 'face markup-face target-file-buffer)
                    (put-text-property beg end 'rep-original-replaced-string orig target-file-buffer)
                    )))
           )))

;; TODO a bit hacky having this routine apply underlining.
;; work on the defmacro defface (sigh)
(defun rep-lookup-markup-face (pass)
  "Given an integer PASS, returns an appropriate face from \[[rep-face-alist]].
These faces are named rep-NN-face where NN is a two-digit integer.
In the event that PASS exceeds the number of such defined faces, this
routine will wrap around and begin reusing the low-numbered fonts.
As a side effect, this function makes the face underlined in red."
  (interactive "npick a number: ") ;; DEBUG
  (let ( markup-face limit index )
    (setq pass (+ pass 12))
    (setq limit (length rep-face-alist) )
    (setq index (mod pass limit))
    (setq markup-face (cdr (assoc index rep-face-alist)))
    (message (pp-to-string markup-face))
;;    (set-face-underline-p markup-face t)
    (set-face-underline-p markup-face "red")  ;; question: need light/dark?
    markup-face
    ))

(defun rep-split-limited (delimiter line limit)
  "Split LINE on DELIMITER into no more than LIMIT fields.
This is something like perl's limit feature on splits.
Using this additional, superfluous delimiters are allowed in the final field.
Example:
 (rep-split-limited \":\" \"hey:ho:lets:go:gabba:gabba:hey\" 5)
 (\"hey\" \"ho\" \"lets\" \"go\" \"gabba:gabba:hey\")
"
  (let* ((raw (split-string line delimiter))
         (new-list ())
         (i 0)
         (i-limit (- limit 1))
         )
    (while (< i i-limit)
      (push (pop raw) new-list)
      (setq i (1+ i))
      )
    (push (mapconcat 'identity raw delimiter) new-list)
    (nreverse new-list)
    ))


;; TODO eventually need something that reads the most recent revert
;; file off of a buffer-local stack
(defun rep-baby-revert-changes ()
  "Revert to the *.bak file."
  (interactive)
  (let* ( (bfn1 (buffer-file-name))
          (cb1  (current-buffer))
          (bak-file (concat bfn1 ".bak"))
               )

    (copy-file bak-file bfn1 t)
    (revert-buffer t t)
    ))


;;
(defun rep-message-properties-at-point ()
  "Tells you what properties you have at point."
  (interactive)
  (let* (capture)
;;    (goto-char (point-min))
    (setq capture (text-properties-at (point)))
    (message (pp-to-string capture))
;;    (forward-char 1)
    ))



(defun rep-what-was-changed-here ()
  "Tells you what the original replaced string was for the next change."
  (interactive)
  (let* (capture)
    ;; TODO skip forward to next occurence of property rep-original-replaced-string
    ;; echo value of just that property

    (setq capture (text-properties-at (point)))
    (message (pp-to-string capture))
;;    (forward-char 1)
    ))



;;; rep.el ends here
