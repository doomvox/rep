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


;; TODO research how flyspell mode works.
;;      research overlays
;;      review macros again (sigh)

;; Put this file into your load-path and the following into your ~/.emacs:
;;   (require 'rep)

;;; Code:

(provide 'rep)
(eval-when-compile
  (require 'cl))




;;---------
;;  User Options, Variables

(defvar rep-substitutions-changes-data ()
  "Data describing substitution changes made to a file. A buffer local variable.")
(make-variable-buffer-local 'rep-substitutions-changes-data)
(put 'rep-substitutions-changes-data 'risky-local-variable t)


;; TODO Add setting to
;; ~/End/Cave/Rep/Wall/Emacs-Rep/elisp/examples/rep-setup.el
(defvar rep-default-substitutions-directory "/tmp")

;;--------
;; generating colorized faces used to mark-up changes

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
 "Faces keyed by number (an integer to font association).
Used by function \\[rep-lookup-markup-face].")
;; hardcoded generation of look-up table (stupid, but simple)
;; TODO modify the rep-make-face macro to generate this table?
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

;;--------
;; functions used by commands below

;; Used by rep-substitutions-apply-to-other-window
(defun rep-run-perl-substitutions ( changes-list-file target-file )
  "Applies substitutions in a CHANGES-LIST-FILE to a TARGET-FILE.
The CHANGES-LIST-FILE should contain substitutions in the traditional
unix 's///' style \(perl5 flavor\), one on each line.  The changes
are made throughout the TARGET-FILE as though the /g modifier was
used on all of them.  The original file is saved under a back-up file,
with extension choosed by the function (( TODO -- for now, it's always bak ))."
  (let* (
         (bak-extension "bak" )  ;; TODO choose unique extension, push info onto a buffer-local stack
;;         ( rep-pl "rep.pl" )    ;; TODO sort out this path problem
         (rep-pl "/home/doom/End/Cave/Rep/Wall/Emacs-Rep/scripts/rep.pl")

         (perl-rep-cmd
               (format
                "perl %s --extension %s --substitutions %s %s "
                rep-pl
                bak-extension
                changes-list-file
                target-file))
         (data (shell-command-to-string perl-rep-cmd))
         )
    data))

;; Used by rep-substitutions-apply-to-other-window
(defun rep-markup-target-buffer (data target-file-buffer)
  "Applies the given change DATA to the TARGET-FILE-BUFFER.
Highlights the changes using different color faces."

  (set-buffer target-file-buffer)
  (revert-buffer t t t) ;;last option: "preserve-modes" what does it do?
  (font-lock-mode -1)
  (rep-modified-mode t)

  ;; make the same data available to other routines via this buffer-local var.
  (setq rep-substitutions-changes-data data)

  ;; TODO does this deal with multi-line strings?
  (setq substitution-lines (rep-split-on-semicolon-delimited-lines data))

  (dolist (line  substitution-lines)
    (cond ((not (string-equal "" line)) ;; skip blank lines
           ;; split each line into five fields
           (let* (
                  (fields (rep-split-limited ":" line 5) )
                  (pass   (string-to-number (nth 0 fields)))
                  (beg    (string-to-number (nth 1 fields)))
                  (end    (string-to-number (nth 2 fields)))
                  (delta  (string-to-number (nth 3 fields)))
                  (orig   (nth 4 fields))
                  (markup-face (rep-lookup-markup-face pass))
                  (len    (+ (length orig) delta) )
                  ;; initialize with the existing stack
                  (stack
                   (get-text-property beg 'rep-change-stack target-file-buffer))
                  )
             (put-text-property beg end 'face markup-face target-file-buffer)
             (put-text-property beg end 'rep-replaced-string orig target-file-buffer)
             (put-text-property beg end 'rep-length-of-replacement len target-file-buffer)

             (push fields stack) ;; experimental structure TODO
             ;; save stack off as text property
             (put-text-property beg end 'rep-change-stack stack target-file-buffer)

             )))))

;; TODO would be better if font-lock-mode status were probed and
;; saved, so that rep-modified-accept-changes can turn it on only if it
;; were already on.
;; Used by rep-substitutions-apply-to-other-window
(defun rep-markup-lines (buffer)
  "Mark-up the lines in the given BUFFER.
Uses the line number with rep-lookup-markup-face to assign a color.
Acts on the given BUFFER, but leaves the current window active."
  (save-excursion ;; but that trick *never* works... so don't trust it
    (let* ( (original-buffer (current-buffer))
            line-number markup-face
            (lines-left 1)
            )
      (set-buffer buffer)
      (font-lock-mode -1)
      (goto-char (point-min))
      (setq line-number 0)
      (while lines-left
        (setq markup-face (rep-lookup-markup-face line-number))
        (let* ( (beg (point))
                end )
          (move-end-of-line 1)
          (setq end (point))
          (put-text-property beg end 'face markup-face)
          )
        (setq line-number (1+ line-number))
        (setq lines-left (= 0 (forward-line 1)))
        )
      (set-buffer original-buffer)
      )
    ))

;; TODO This routine can be modified to apply underlining at this
;; stage (though that's hacky, I know). someday, work out how
;; add an underline to the defface macro
(defun rep-lookup-markup-face (pass)
  "Given an integer PASS, returns an appropriate face from \[[rep-face-alist]].
These faces are named rep-NN-face where NN is a two-digit integer.
In the event that PASS exceeds the number of such defined faces, this
routine will wrap around and begin reusing the low-numbered fonts.
As a side effect, this function makes the face underlined in red."
;;  (interactive "npick a number: ") ;; DEBUG
  (let ( markup-face limit index )
    (setq limit (length rep-face-alist) )
    (setq index (mod pass limit))
    (setq markup-face (cdr (assoc index rep-face-alist)))
    (message (pp-to-string markup-face))
;; TODO not sure there's any point to underlines (I like my colorization now)
;;    (set-face-underline-p markup-face t)
;;    (set-face-underline-p markup-face "red")  ;; question: need light/dark?
    markup-face
    ))


;; TODO   Instead of writing this, could've used dired's
;; "dired-split" which is even closer to perl's split (can use a regexp):
;;   (dired-split PAT STR &optional LIMIT)
(defun rep-split-limited (delimiter line limit)
  "Split LINE on DELIMITER into no more than LIMIT fields.
This is something like perl's limit feature on splits.
Using this function additional, superfluous delimiters are
allowed in the final field.
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

(defun rep-split-on-semicolon-delimited-lines ( text )
  "Splits text on line-endings with semi-colons.
This allows for \"lines\" with embedded newlines, but any
embedded semi-colons are expected to be escaped with a backslash.
The escaping backslashes are removed."
  (let* (
        ;; match a semicolon not preceeded by backwhack, at eol
        ;; captures all to 1.  Note, this eats a preceeding char that
        ;; isn't part of the end of line.
        (pat "\\([^\\\\];\s*$\\)")
        (fin (length text))
        (beg  0) ;; start of next line, a cursor sweeping through text
        (lines ())
        end
        )
    (while (< beg fin)
      (let* ( ;; skip -- unused now.  TODO DELETE skip usages
              ;; look for location with expected line ending...
             (loc (string-match pat text beg)) )
        (cond (loc ;; if that's found, we've found end of next record
               (setq end (1+ loc ))
               ;; (setq skip (length (match-string 1 text)))
               )
              (t ;; loc is nil, so we're near end of text
               (setq end fin)
               ;; (setq skip 0) ;; being neat
               ))
        (setq line (substring text beg end))
        (setq line
              (replace-regexp-in-string "\\\\" "" line))
        (push line lines)

;;         (setq beg (+ end skip)) ;; skipping past line ending
        (setq beg (string-match "^" text end)) ;; more portable?
        ))
    (setq lines (nreverse lines))
    lines))

;;---------
;; controlling  modes

;; This systems "controllers" come in three stages:
;;  (1) a global key binding to create and edit a new substitutions file-buffer.
;;  (2) a rep-substitutions-mode, with C-x# binding to apply to other window.
;;  (3) a rep-modified-mode: a minor-mode automatically enabled in that
;;      other window once it's been modified.  This has keybindings to
;;      examine, undo, accept or revert the changes.

;; TODO What's the right way to do a binding in all modes,
;;      or at least any of a list of modes you're interested in?
;;      (I've researched this before, and ran into problems...)
;;      What if I created a minor-mode for this? And turned it on
;;      with an after-mode hook of some sort?
;; TODO review this choice: default binding to begin it all: C-c.S
(defun rep-define-global-key-binding (&optional prefix)
  "Defines a global keybinding to open a new substitutions buffer.
Defaults to \"Control-c . S\".  A different prefix may be given
as an argument, for example:
  (rep-define-global-key-binding \"M-o\")
would define the key-strokes \"Alt o S\"."
  (interactive) ;; DEBUG
  (unless prefix (setq prefix "\C-c."))
  (global-set-key (format "%sS" prefix) 'rep-open-substitutions-buffer)
  (message "Defined bindings for key: S under the prefix %s" prefix)
  )

;; TODO watch out for small windows without room to split.
;; 10 lines? maybe better, a percentage of the window (or just if it's too small?)
;; TODO add local-vars table (or something) so you get the right mode if you
;; save and open again.
;; TODO this *has* to be saved to a file for rep.pl to work.
;; TODO add a "file" param to override the default.
;; TODO by default, should get a unique file
(defun rep-open-substitutions-buffer ()
  "Open a new substitutions buffer."
  (interactive)
  (let* (( upper-size 10 )
         ( dir rep-default-substitutions-directory )
         ( default-buffer-file (concat dir "/" "substitutions.rep") )
         )
    (split-window-vertically upper-size)
    (find-file default-buffer-file)
    (rep-substitutions-mode)
    ))

(define-derived-mode rep-substitutions-mode
  cperl-mode "rep-substitutions"
  "Major mode to enter stack of substitutions to be applied.
Derived from cperl-mode, because we're editing substitutions
that use perl's syntax \(and are interpreted using perl\).
\\{rep-substitutions-mode-map}"
  (use-local-map rep-substitutions-mode-map)
  )
(define-key rep-substitutions-mode-map "\C-x#"
  'rep-substitutions-apply-to-other-window)

(define-minor-mode rep-modified-mode
  "Toggle Rep Modified mode.
     With no argument, this command toggles the mode.
     Non-null prefix argument turns on the mode.
     Null prefix argument turns off the mode.

     When Rep Modified mode is enabled, key bindings are defined
     to examine and undo the changes made by rep substitutions.
     These are commands such as
         \\[rep-modified-what-was-changed-here]
         \\[rep-revert-change-here]
         \\[rep-modified-revert-all-changes]    "
  ;; The initial value.
  :init-value nil
  ;; The indicator for the mode line.
  :lighter " Reppy"
  ;; The minor mode bindings.
  :keymap
  '(
    ("\C-c.w" . rep-modified-what-was-changed-here)
    ("\C-c.x" . rep-modified-examine-properties-at-point)
    ("\C-c.u" . rep-modified-undo-change-here)
    ("\C-c.R" . rep-modified-revert-all-changes)
    ("\C-c.@" . rep-modified-accept-changes)
    ("\C-i"   . rep-modified-skip-to-next-change)
    )
  )



;;--------
;; rep-substitutions-mode function(s)

(defun rep-substitutions-apply-to-other-window ()
  "Two buffers must be open, the changes_list and the file to act on,
with the changes_list selected.
Uses the pass number to choose fonts to mark-up changes.
Turns off font-lock to avoid conflict with existing syntax coloring."
  (interactive)
  (let* (
          pass perl-rep-cmd data
          substitution-lines

          (changes-list-file    (buffer-file-name))
          (changes-list-buffer  (current-buffer))

          target-file target-file-buffer
         )
         (other-window 1) ;; cursor in buffer to modify now
         (setq target-file          (buffer-file-name))
         (setq target-file-buffer   (current-buffer))

         (setq data
               (rep-run-perl-substitutions changes-list-file target-file))

         (rep-markup-target-buffer data target-file-buffer)
         (rep-markup-lines changes-list-buffer)

         ;; jump to the first change in the modified buffer
         (goto-char
          (next-single-property-change
           (point-min) 'rep-change-stack target-file-buffer))
         ))

;;--------
;; rep-modified-mode functions

;; TODO eventually need something that reads the most recent revert
;; file off of a buffer-local stack
(defun rep-modified-revert-all-changes ()
  "Revert to the *.bak file."
  (interactive)
  (let* ( (bfn (buffer-file-name))
          (cb  (current-buffer))
          (bak-file (concat bfn ".bak"))
               )
    (copy-file bak-file bfn t)
    (revert-buffer t t)

    ;; covering flakiness in revert-buffer & text properties.
    (font-lock-fontify-buffer)
    (put-text-property (point-min) (point-max) 'rep-replaced-string "" cb)
    (put-text-property (point-min) (point-max) 'rep-change-stack () cb)
    ))

(defun rep-modified-accept-changes ()
  "Accept changes made in buffer, return to normal state."
  (interactive)
  (rep-modified-mode nil)
  (font-lock-mode 1)
  )

;; TODO note that this monitors the rep-change-stack
;;      older code like this monitors rep-replaced-string
(defun rep-modified-skip-to-next-change ()
  "Skip to next region modified by a substitution."
  (interactive)
  ;; Check if we're inside a changed region first
  (let* ( (stack (get-text-property (point) 'rep-change-stack))
          )
    (cond (stack  ;;   ;; we are inside a changed region and must get out first
           (goto-char
            (1+
             (next-single-property-change (point) 'rep-change-stack)))
           ))
    ;; jump to the next changed region
    (goto-char
     (next-single-property-change
      (point) 'rep-change-stack))
    ))

(defun rep-modified-examine-properties-at-point ()
  "Tells you what properties you have at point."
  (interactive)
  (let* (capture)
    (setq capture (text-properties-at (point)))
    (message (pp-to-string capture))
    ))

(defun rep-modified-what-was-changed-here ()
  "Tells you the original string was before it was replaced.
Looks at the changed string under the cursor, or if not defined
there, tries to advance the cursor to the next change."
  (interactive)
  (let* ( (orig (get-text-property (point) 'rep-replaced-string)) )
    (unless orig
      (let ( (spot (next-single-property-change (point) 'rep-replaced-string)) )
        (setq orig (get-text-property spot 'rep-replaced-string))
        (goto-char spot)
        ))
    (message orig)
    ))

(defun rep-modified-undo-change-here (&optional dryrun)
  "Undos the individual rep substitution change near the cursor.
Undos the change at point, or if none is there, the next change
afterwards.  With prefix argument (or DRYRUN option), will show
extent to be reverted without performing the change.

Note that this has nothing to do with the usual emacs \"undo\"
system, which operates completely independantly.

Limitation: this can be confused by casacading, overlapping
changes.  When the text near point was modified by multiple
passes of substitution commands this can typically only undo the
last change. A warning message is generated if it can not undo
a change.
"
  (interactive "P")
;; Start out with the theory that the cursor is inside of the region to undo,
;; otherwise, must search forward for the next changed region.
  (let* ( (orig (get-text-property (point) 'rep-replaced-string))
          (len  (get-text-property (point) 'rep-length-of-replacement))
          beg end current-string stack
          )
    (cond ((not orig) ;; find a changed region
            (setq beg
                  (next-single-property-change (point) 'rep-replaced-string))
            (setq end
                  (next-single-property-change beg     'rep-replaced-string))
            (setq orig (get-text-property beg 'rep-replaced-string))
            (setq len  (get-text-property beg 'rep-length-of-replacement))
          )
          (t         ;; we are inside a changed region
           (setq beg
                 (previous-single-property-change (point) 'rep-replaced-string))
           (setq end
                 (next-single-property-change (point) 'rep-replaced-string))
           ))

    (cond ((and beg end len orig)

           (setq stack (get-text-property beg 'rep-change-stack))
           (setq current-string (buffer-substring-no-properties beg end))

           (cond (dryrun
                  (goto-char beg)
                  (set-mark beg)
                  (goto-char end)
                  (exchange-point-and-mark)
                  )
                 ((not (= len (- end beg)))
                  (message "Can't revert fragment: %s." current-string)
                  )
                 (t
                  (kill-region beg end)
                  (insert orig)

                  (let* ((adjusted-end (+ end (- len (length orig)))))
                    (pop stack)
                    (put-text-property beg adjusted-end 'rep-change-stack stack)
                    )
                  (message orig)
                  )))
          (t
           (message "There are no changed regions to undo after point.")
           )
          )
    ))




;;; rep.el ends here
