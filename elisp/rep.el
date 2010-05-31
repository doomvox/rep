;;; rep.el --- find and replace using perl5

;; Copyright 2010 Joseph Brenner
;;
;; Author: doom@kzsu.stanford.edu
;; Version: $Id: rep.el,v 0.0 2010/05/14 01:49:29 doom Exp $
;; Keywords:
;; X-URL: not distributed yet

;; Note: in the event that the licensing of the Emacs::Rep module
;; (see the file lib/Emacs/Rep.pm in this package)
;; conflicts with the following statement, the terms of Emacs::Rep
;; shall be used.

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

;; If it isn't there already, put this file (rep.el) into your load-path.

;; You will also need the script "rep.pl" to be located somewhere
;; in your system PATH, and the Emacs::Rep perl module
;; (i.e. the file Emacs/Rep.pm) installed somewhere that rep.pl
;; can find it (e.g. a location in your PERL5LIB environment variable).
;; Note that these needs should be taken care of by installing the
;; Emacs-Rep CPAN package.  See the README file inside that package.

;; Add the following into your ~/.emacs (or equivalent):
;;   (require 'rep)
;;   (global-set-key "C-c.S" 'rep-open-substitutions)

;; Then, when editing a file you'd like to modify interactively
;; using perl substitution commands (i.e. s///g;), you can use
;; "C-c.S" to open a small window associated with a *.rep file,
;; suitable for entering a series of substitution commands.
;; When you're ready, "C-x#" will run these substitutions on
;; the other window.  The usual font-lock syntax coloring will
;; be temporarily shut-off, so that changed strings can be
;; indicated with colors that correspond to the particular
;; s///g command that made the change.

;; The modified file has a "rep-modified-mode" minor mode switched on
;; with keybindings to do various useful tasks as you evaluate the
;; changes made by your substitutions.

;;    TAB       rep-modified-skip-to-next-change
;;              You can easily skip to the next change with the tab key.

;;    "C-c.w"   rep-modified-what-was-changed-here
;;              Tells you want the changed string at the cursor
;;              was before it was modified.

;;    "C-c.u"   rep-modified-undo-change-here
;;              Does an undo of an individual change, changing it back
;;              to the string indicated with "C-c.w".
;;              Note: there's no relation between this and the usual emacs
;;              undo mechanism, they operate independantly.

;;    "C-c.R"   rep-modified-revert-all-changes
;;              All of the changes from a run of substitutions
;;              can be reverted all at once, using this command.
;;              If there have been multiple substitutions runs
;;              on the same file-buffer, repeated uses of this
;;              command will continue reverting the previous run.

;;     "C-c.@"  rep-modified-accept-changes
;;              When you're convinced that the substitutions run did
;;              what you wanted, you can use this to accept the changes
;;              and get your normal syntax coloring back.

;;  Note that the *.rep files you create with C-c.S can be run again
;;  on other files.  This should simplify making similar changes to
;;  a large number of files (though at present there are no recursive
;;  descent features provided by this library).

;;  Additionally, there's the command:

;;    "C-c.x"   rep-modified-examine-properties-at-point
;;              This is more informative than C-c.w, but less neat:
;;              this can be used to look at the whole rep-change-stack,
;;              which might help when multiple cascading changes have
;;              taken place in the same location.

;;; Code:

(provide 'rep)
(eval-when-compile
  (require 'cl))



;;---------
;;  User Options, Variables

(defcustom rep-underline-changes-color nil
  "If this is set to a color name such as \"red\" then the
substitution changes will also be underlined in that color.  If
it is set to t, then the changes will be underlined in the same
color as their markup face.  See \\[rep-lookup-markup-face].")

(defvar rep-font-lock-buffer-status ()
  "Buffer local variable to store the previous font-lock-mode status.
This allows us to remember that font-lock-mode was on, and should be
re-enabled after changes are accepted.")
(make-variable-buffer-local 'rep-font-lock-buffer-status)

(defvar rep-standard-substitutions-directory nil
  "The location to place newly created files of substitution commands.
Note: include a trailing slash.
If this is nil, then a sub-directory named \".rep\" will
be created in parallel with the file to be modified.")

(defvar rep-standard-substitutions-file-name-prefix "substitutions"
  "This is used to name newly created files of substitution commands.
By default, the name would typically be something like
\"substitutions-DJE.rep\".")

;; Note, at present, the *.rep file-extension is hard-coded.

(defvar rep-standard-backup-location nil
  "The location to place back-up copies of modified files.
Note: include a trailing slash.
If this is nil, then a sub-directory named \".rep\" will
be created in parallel with the file to be modified.")

;; Note, at present, the *.bak file-extension is hard-coded.

(defvar rep-previous-versions-stack ()
  "Buffer local stack of previous backup versions.
Each run of substitutions on a file will generate another backup
file.  Reverts can trace this stack upwards to get back to
any version.")
(make-variable-buffer-local 'rep-previous-versions-stack)
(put 'rep-previous-versions-stack 'risky-local-variable t)

;;--------
;; colorized faces used to mark-up changes
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

(rep-make-face rep-00-face 00 "DarkGoldenrod4" "DarkGoldenrod2")
(rep-make-face rep-01-face 01 "MediumPurple4" "MediumPurple1")
(rep-make-face rep-02-face 02 "forest green" "light green")
(rep-make-face rep-03-face 03 "PaleVioletRed4" "PaleVioletRed1")
(rep-make-face rep-04-face 04 "gold4" "gold1")
(rep-make-face rep-05-face 05 "salmon4" "salmon1")
(rep-make-face rep-06-face 06 "RoyalBlue1" "RoyalBlue1")
(rep-make-face rep-07-face 07 "DarkOrchid4" "DarkOrchid1")
(rep-make-face rep-08-face 08 "green4" "green1")
(rep-make-face rep-09-face 09 "khaki1" "khaki4")
(rep-make-face rep-10-face 10 "DarkOrange4" "DarkOrange1")
(rep-make-face rep-11-face 11 "SeaGreen4" "SeaGreen1")
(rep-make-face rep-12-face 12 "maroon4" "maroon1")
(rep-make-face rep-13-face 13 "firebrick4" "firebrick1")
(rep-make-face rep-14-face 14 "PeachPuff4" "PeachPuff1")
(rep-make-face rep-15-face 15 "CadetBlue4" "CadetBlue1")
(rep-make-face rep-16-face 16 "aquamarine4" "aquamarine1")
(rep-make-face rep-17-face 17 "OliveDrab4" "OliveDrab1")
(rep-make-face rep-18-face 18 "SpringGreen4" "SpringGreen1")
(rep-make-face rep-19-face 19 "chocolate4" "chocolate1")
(rep-make-face rep-20-face 20 "DarkSeaGreen4" "DarkSeaGreen1")
(rep-make-face rep-21-face 21 "LightSalmon4" "LightSalmon1")
(rep-make-face rep-22-face 22 "DeepSkyBlue4" "DeepSkyBlue1")
(rep-make-face rep-23-face 23 "chartreuse4" "chartreuse1")
(rep-make-face rep-24-face 24 "cyan4" "cyan1")
(rep-make-face rep-25-face 25 "magenta4" "magenta1")
(rep-make-face rep-26-face 26 "blue4" "blue1")
(rep-make-face rep-27-face 27 "DeepPink4" "DeepPink1")
(rep-make-face rep-28-face 28 "DarkOliveGreen4" "DarkOliveGreen1")
(rep-make-face rep-29-face 29 "coral4" "coral1")
(rep-make-face rep-30-face 30 "PaleGreen4" "PaleGreen1")
(rep-make-face rep-31-face 31 "tan4" "tan1")
(rep-make-face rep-32-face 32 "orange4" "orange1")
(rep-make-face rep-33-face 33 "cornsilk4" "cornsilk1")

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
;; utility functions used by commands below

;; TODO work out how add an underline to the defface macro,
;; rather than do it here?  Could adapt to light/dark backgrounds that way.
(defun rep-lookup-markup-face (pass)
  "Given an integer PASS, returns an appropriate face from \[[rep-face-alist]].
These faces are named rep-NN-face where NN is a two-digit integer.
In the event that PASS exceeds the number of such defined faces, this
routine will wrap around and begin reusing the low-numbered faces.
If PASS is nil, this will return nil.
Underlining may be turned on with `rep-underline-changes-color'."
;;  (interactive "npick a number: ") ;; DEBUG
  (cond (pass
         (let ( markup-face limit index )
           (setq limit (length rep-face-alist) )
           (setq index (mod pass limit))
           (setq markup-face (cdr (assoc index rep-face-alist)))
           (message (pp-to-string markup-face))
           (cond (rep-underline-changes-color
                  (set-face-underline-p markup-face rep-underline-changes-color)
                  ))
           markup-face))
        (t
         nil)))

;; TODO   Instead of this function, could've used dired's
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
      (let* (;; look for location with expected line ending...
             (loc (string-match pat text beg)) )
        (cond (loc ;; if that's found, we've found end of next record
               (setq end (1+ loc))
               )
              (t ;; loc is nil, so we're near end of text
               (setq end fin)
               ))
        (setq line (substring text beg end))
        (setq line
              (replace-regexp-in-string "\\\\" "" line))
        (push line lines)
        (setq beg (string-match "^" text end))
        ))
    (setq lines (nreverse lines))
    lines))

(defun rep-sub-directory (file-location)
  "Given a directory, returns path to a '.rep' sub-directory.
If the sub-directory does not exist, this will create it. "
  (let* ( (dir
           (substitute-in-file-name
            (convert-standard-filename
             (file-name-as-directory
              (expand-file-name file-location))))) ;; being defensive
          (standard-subdir-name ".rep")
          (subdir (concat dir  standard-subdir-name))
         )
    (unless (file-directory-p subdir)
      (make-directory subdir t))
    subdir))

(defun rep-generate-random-suffix ()
  "Generate a three character suffix, pseudo-randomly."
;; As written, this is always 3 upper-case asci characters.
;;  (interactive);; DEBUG
  (let (string)
    (random t)
    (setq string
          (concat
           (format "%c%c%c"
                   (+ (random 25) 65)
                   (+ (random 25) 65)
                   (+ (random 25) 65)
                   )
           ))
    ))

;;---------
;; controlling  modes

;; This systems "controllers" come in three stages:
;;  (1) a global key binding to create and edit a new substitutions file-buffer.
;;  (2) a rep-substitutions-mode, with C-x# binding to apply to other window.
;;  (3) a rep-modified-mode: a minor-mode automatically enabled in that
;;      other window once it's been modified.  This has keybindings to
;;      examine, undo, accept or revert the changes.


;; This folderol is silly for a single binding.
;; Just recommend this:
;;  (global-set-key "C-c.S" 'rep-open-substitutions)
(defun rep-define-global-key-binding (&optional prefix)
  "Defines a global keybinding to open a new substitutions buffer.
Defaults to \"Control-c . S\".  A different prefix may be given
as an argument, for example:
  (rep-define-global-key-binding \"M-o\")
would define the key-strokes \"Alt o S\"."
  (interactive) ;; DEBUG
  (unless prefix (setq prefix "\C-c."))
  (global-set-key (format "%sS" prefix) 'rep-open-substitutions)
  (message "Defined bindings for key: S under the prefix %s" prefix)
  )

(defun rep-open-substitutions ()
  "Open a new substitutions file buffer.
This is the intended entry-point command.  It should have a
\"global\" keybinding which will also available in all (or mostly
all) modes.

This will typically open up a file something like this:

   .rep/substitutions-832-JDE.rep

Where the sub-directory '.rep' is located in the same place
as the file it looked like you were about to modify, and the
unique 3 letter suffix is randomly chosen.

If you have the `rep-standard-substitutions-directory' variable
set to some location, then the *.rep files will all be located
there.

The standard prefix (default: \"substitutions\") comes from this
variable: `rep-standard-substitutions-file-name-prefix'."
  (interactive)
  (let* ((file-location (file-name-directory (buffer-file-name)))
         (dir (or
               rep-standard-substitutions-directory
               (rep-sub-directory file-location)))
         (name rep-standard-substitutions-file-name-prefix )
         (ext  "rep" )
         (pid (number-to-string (emacs-pid)))
         (suffix (rep-generate-random-suffix))
         (full-file-name (concat dir "/" name "-" pid "-" suffix "." ext))
         )
     (while (file-exists-p full-file-name)
       (setq suffix (rep-generate-random-suffix))
       (setq full-file-name (concat dir "/" name "-" pid "-" suffix "." ext)))
    (rep-open-substitutions-file-buffer-internal full-file-name )
    ))

(defun rep-open-substitutions-prompt (name)
  "Open a new substitutions file buffer, prompting for the NAME.
This is an alternate entry-point command, much like
\\[rep-open-substitutions]."
  (interactive "FName of substitutions file:")
  (rep-open-substitutions-file-buffer-internal name )
  )

;; TODO watch out for small starting windows without room to split.
;; Instead of 10 lines, a percentage of the window?
(defun rep-open-substitutions-file-buffer-internal ( file )
  "Open a new substitutions file buffer, given the full FILE name.
This just handles the window management and template insertion.
Choosing the file name and location is a job for routines such as
\\[rep-open-substitutions]."
  (interactive)
  (let* (( number-lines 10 )
         ( hint
           (concat
           "# Enter s///g lines "
           "/e not allowed /g assumed, "
           "'C-x #' applies to other window") )
         ( template "s///g;" )
         )
    (split-window-vertically number-lines)
    (find-file file)
    (rep-substitutions-mode)
    (insert hint)
    (insert "\n") ;; portability?
    (insert template)
    (move-beginning-of-line 1)
    (forward-char 2)
    ))

;; An alternate entry point: you might just open an existing *.rep file
(add-to-list
 'auto-mode-alist
 '("\\.\\(rep\\)\\'" . rep-substitutions-mode))

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
  :lighter " Rep"
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

              target-file target-file-buffer backup-file
              )
    (save-buffer)
    (other-window 1) ;; cursor in buffer to modify now
    (setq target-file          (buffer-file-name))
    (setq target-file-buffer   (current-buffer))
    (setq backup-file          (rep-generate-backup-file-name target-file))
    (save-buffer)

    (setq data
          (rep-run-perl-substitutions
           changes-list-file target-file backup-file))

    (cond (data
           (rep-markup-target-buffer data target-file-buffer backup-file)
           ;; (rep-markup-lines changes-list-buffer)
           (rep-markup-substitution-lines changes-list-buffer)

           ;; jump to the first change in the modified buffer
           (goto-char
            (next-single-property-change
             (point-min) 'rep-change-stack target-file-buffer))
           )
          (t
           (message "No changes made by substitutions.")) )
    ))

;; Used by rep-substitutions-apply-to-other-window
(defun rep-generate-backup-file-name (file)
  "Given a FILE name, generate a unique backup file name.
If `rep-standard-backup-location' is non-nil, it will be used
as the standard location for backups, otherwise, a \".rep\"
sub-directory will be used in parallel with the FILE."

  (interactive)
  (let* ((file-location (file-name-directory file))
         (name          (file-name-nondirectory file))
         (dir (or
               rep-standard-backup-location
               (rep-sub-directory file-location)))
         (ext "bak")
         (suffix (rep-generate-random-suffix))
         (pid (number-to-string (emacs-pid)))
         (full-file-name (concat dir "/" name "-" pid "-" suffix "." ext))
         )

    (while (file-exists-p full-file-name)
       (setq suffix (rep-generate-random-suffix))
       (setq full-file-name (concat dir "/" name "-" pid "-" suffix "." ext))
       )
    full-file-name))

(defun rep-probe-for-program (program)
  "Probe the system for the given external PROGRAM."
  (let* (
         (cmd1
          (format "which %s"
                  (shell-quote-argument program)))
         (cmd2
          (format "locate /%s"
                  (shell-quote-argument program)))
         (result1 (shell-command-to-string cmd1))
;;         (result2 (shell-command-to-string cmd2))
;;         (ret (concat result1 " " result2))
         (ret result1)
         )
    (cond ((not (> (length result1) 1))
           (message "The program %s does not seem to be in your PATH." program)
          nil)
          (t
           ret))
    ))

;; Used by rep-substitutions-apply-to-other-window
(defun rep-run-perl-substitutions ( changes-list-file target-file backup-file )
  "Applies substitutions in a CHANGES-LIST-FILE to a TARGET-FILE.
The CHANGES-LIST-FILE should contain substitutions in the traditional
unix 's///' style \(perl5 flavor\), one on each line.  The changes
are made throughout the TARGET-FILE as though the /g modifier was
used on all of them.  The original file is saved as the given BACKUP-FILE."
  (let* (
         (rep-pl "rep.pl")

         ;; TODO SOON probe system for program, provide informative error message.
         ;; TODO capture stderr somehow?
         (perl-rep-cmd
               (format
                "%s --backup %s --substitutions %s --target %s "
                rep-pl
                (shell-quote-argument
                 backup-file)
                (shell-quote-argument
                 changes-list-file)
                (shell-quote-argument
                 target-file)))
         (check (rep-probe-for-program rep-pl))
         data
         )
    (cond (check
           (setq data (shell-command-to-string perl-rep-cmd))
           ))
    data))

;; Used by rep-substitutions-apply-to-other-window
(defun rep-markup-target-buffer (data target-buffer backup-file)
  "Applies the given change DATA to the TARGET-BUFFER.
Highlights the changes using different color faces."
  (set-buffer target-buffer)
  (revert-buffer t t t) ;;last option is "preserve-modes", what does it do?
  (font-lock-mode -1)
  (rep-modified-mode t)

  (push backup-file rep-previous-versions-stack)

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
                   (get-text-property beg 'rep-change-stack target-buffer))
                  )
             (put-text-property beg end 'face markup-face target-buffer)
             (push fields stack)
             ;; save stack off as text property
             (put-text-property beg end 'rep-change-stack stack target-buffer)
             )))))

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
      ;; if font-lock-mode was on, save that information
      (setq rep-font-lock-buffer-status font-lock-mode)
      (font-lock-mode -1) ;; turns off font-lock unconditionally
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


;; TODO can be confused by embedded semicolons. I suspect,
(defun rep-markup-substitution-lines (buffer)
  "Mark-up the substitution lines in the given BUFFER.
Uses a the line number with rep-lookup-markup-face to
Assigns a color to each substitution command in the buffer,
\(by counting from the top and feeding the position number
to \\[rep-lookup-markup-face]\).

Presumes all substitution commands begin with ^s.

Acts on the given BUFFER, but leaves the current window active.
"
  (save-excursion ;; but that trick *never* works... so don't trust it
    (let* ( (original-buffer (current-buffer))
            (comment_pat  "^\s*?#")
            (scmd_beg_pat "^\s*?s")
;;            (scmd_end_pat ";\s*?\(#\|$\)")  ;; n.g.
            (scmd_end_pat ";\s*?$")  ;; eh.
            (scmd_count 0)
            markup-face
            )
      (set-buffer buffer)
      ;; if font-lock-mode was on, save that information
      (setq rep-font-lock-buffer-status font-lock-mode)
      (font-lock-mode -1) ;; turns off font-lock unconditionally
      (goto-char (point-min))

      (while (re-search-forward scmd_beg_pat nil t)
        (setq markup-face (rep-lookup-markup-face scmd_count))

        (let ( beg end )
          (setq beg (match-beginning 0))
          (cond ((re-search-forward scmd_end_pat nil t)
                 (setq end (match-end 0))
                 (put-text-property beg end 'face markup-face)
                 (setq scmd_count (1+ scmd_count))
                 (goto-char (- end 1))
                 )
                (t ;; found beginning but not ending...
                 (message "Incomplete substitution command.")
                 )
                )
          ))
      (set-buffer original-buffer)
      )
    ))


;;--------
;; rep-modified-mode functions

(defun rep-modified-revert-all-changes ()
  "Revert last substitutions, restoring the previous backup file.
Uses the `rep-previous-versions-stack' buffer local variable."
  (interactive)
  (let* ( (current-buffer-file-name (buffer-file-name))
          (backup-file (pop rep-previous-versions-stack))
          (preserve-stack rep-previous-versions-stack)
               )
    (copy-file backup-file current-buffer-file-name t)
    (revert-buffer t t)

    ;; covering flakiness in revert-buffer & text properties.
    (font-lock-fontify-buffer)
    (put-text-property (point-min) (point-max) 'rep-change-stack ())
    ;; in case you want to revert again...
    (rep-modified-mode t)
    (setq rep-previous-versions-stack preserve-stack)
    ))

(defun rep-modified-accept-changes ()
  "Accept changes made in buffer, return to normal state."
  (interactive)
  (rep-modified-mode nil)
  ;; turn font-lock back on if it was on
  (if rep-font-lock-buffer-status
      (font-lock-mode 1))
  )

;; TODO rep-modified-display-changes-again ?
;;      inverse of the above

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
  (let* ( (stack (get-text-property (point) 'rep-change-stack))
          )
    (cond ( (not stack)  ;;   ;; we are not yet inside a changed region
            (goto-char
             (next-single-property-change (point) 'rep-change-stack))
            ))
    (let* ( (fields (pop stack))
            (orig  (nth 4 fields))
            )
      (push fields stack) ;; TODO 98% certain this is silly
      (message orig)
      )))

(defun rep-modified-undo-change-here ()
  "Undos the individual rep substitution change under the cursor.
Undos the change at point, or if none is there, warns and does nothing.
Note that this has nothing to do with the usual emacs \"undo\"
system, which operates completely independantly."
  (interactive)
  (let* ( (stack (get-text-property (point) 'rep-change-stack))
         orig-len expected-len beg end current-string orig fields pass delta
          )
    (cond (stack ;; we are inside a changed region, so find it's extent
           (setq beg
                 (previous-single-property-change (point) 'rep-change-stack))
           (setq end
                 (next-single-property-change (point) 'rep-change-stack))
           ))
    (cond ((not (and stack beg end))
           (message "No changed region to undo at point.")
           )
          (t
           (setq fields (pop stack)) ;;  (("4" "41" "47" "3" "ket")
           (setq orig  (nth 4 fields))
           (setq pass  (string-to-number (nth 0 fields)))
           (setq delta (string-to-number (nth 3 fields)))

           (setq orig-len  (length orig))

           (setq current-string (buffer-substring-no-properties beg end))

           (setq expected-len (+ orig-len delta))
           (cond ((not (= expected-len (- end beg)))
                  (message (concat
                            "Can't revert fragment: %s. "
                            "Must undo adjacent change first." current-string))
                  )
                 (t
                  (kill-region beg end)
                  (insert orig)

                  ;; mark up restored 'orig' with appropriate stack entry
                  (let* ((new-end (+ beg orig-len)))
                    (put-text-property beg new-end 'rep-change-stack stack)
                    ;; peek up one level to get pass number for right face
                    (let* ((orig-pass (rep-last-pass stack))
                           (orig-face (rep-lookup-markup-face orig-pass)) )
                      (put-text-property beg new-end 'face orig-face) )
                    )
                  )))
          )
    ))

(defun rep-last-pass (stack)
   "Peek up the STACK one-level and get the last pass.
If given STACK is nil, will return nil."
   (cond (stack
          (let* ((fields (pop stack))
                 (last-pass (string-to-number (nth 0 fields)))
                 )
            (push fields stack) ;; TODO is this needed?  I be tired.
            last-pass)
          )
         (t
          nil)))

;;; rep.el ends here
