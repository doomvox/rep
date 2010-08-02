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

;; "Rep" is a system for doing global finds and replaces throughout
;; a file with a set of perl substitution commands (that is, "s///g").

;; This elisp code is the interactive front-end, which uses perl
;; code as a back-end to execute the changes, so you get to use
;; actual perl5 regexps, and you have (almost) all of the features
;; of perl substitutions.

;; The interactive features of rep.el include the ability to
;; reject individual changes, or to revert all of the changes and
;; start over.

;; INSTALLATION and SET-UP

;; If it isn't there already, put this file (rep.el) somewhere in
;; your emacs load-path.

;; If possible, you should just install the Emacs-Rep CPAN
;; package (and see the README file inside that package).

;; Otherwise, you will also need to make sure the script "rep.pl"
;; to be located somewhere in your system PATH, and the Emacs::Rep
;; perl module (i.e. the file Emacs/Rep.pm) is installed somewhere
;; that rep.pl can find it (e.g. a location in your PERL5LIB
;; environment variable).

;; Just add the following into your ~/.emacs (or equivalent):
;;   (require 'rep)
;;   (rep-standard-setup)

;; Alternately, if you don't want to use "\C-c." (control c dot) as
;; your standard prefix, you might do this to, for example, use
;; "\C-c|" (control c pipe) instead:

;;   (require 'rep)
;;   (setq rep-key-prefix [(control ?c) ?|]   ;; for rep-modified-mode
;;   (rep-standard-setup)

;; USAGE

;; When editing a file you'd like to modify you can use "C-c.S" to open a
;; small window suitable for entering a series of perl substitution
;; commands, typically of the form: "s/<find_pattern>/<replace_string>/g;".

;; When you're ready, "C-c.R" will run these substitutions on the
;; other window.

;; The usual font-lock syntax coloring will be temporarily
;; shut-off, so that modified strings can be indicated easily,
;; with colors correspond to the particular s///g command that
;; made the change.

;; In the buffer for the modified file a "rep-modified-mode" minor
;; mode has been switched on with keybindings to do various useful
;; tasks as you evaluate the changes made by your substitutions.

;;    TAB       rep-modified-skip-to-next-change
;;              You can easily skip to the next change with the tab key.
;;              The message in the status bar tells you what it was
;;              before the change.

;;    "C-c.u"   rep-modified-undo-change-here
;;              Does an undo of an individual change, changing it back
;;              to the string indicated with "C-c.w".

;;    "C-c.R"   rep-modified-revert-all-changes
;;              Reverts all of the changes at once.
;;              If there have been multiple substitutions runs
;;              on the same file-buffer, repeated uses of this
;;              command will continue reverting the previous run.

;;     "C-c.A"  rep-modified-accept-changes
;;              When you're convinced that the substitutions did
;;              what you wanted, you can use this to accept the changes
;;              and get your normal syntax coloring back.

;;  Note that the *.rep files you create with C-c.S can be run again
;;  on other files.  This should simplify making similar changes to
;;  a large number of files.

;; For more information:

;; Web pages about this code:
;;    http://obsidianrook.com/rep

;; The CPAN Emacs-Rep package:
;;    http://search.cpan.org/search?query=Emacs%3A%3ARep&mode=all

;; The latest code:
;;   http://github.com/doomvox/rep

;;; Code:

(provide 'rep)
(eval-when-compile
  (require 'cl)
  (require 'dired-aux)
  (require 'json)
  )



;;---------
;;  User Options, Variables

(defvar rep-version "0.07"
 "Version number of the rep.el elisp file.
This version should match the versions of
the rep.pl script and the Rep.pm \(Emacs::Rep\)
perl library.")

(defvar rep-debug nil
  "Set to t to enable some debug messages.")
;; (setq rep-debug nil)
;; (setq rep-debug t)

(defvar rep-trace nil
  "Set to t to enable subroutine trace messages.")
;; (setq rep-trace nil)
;; (setq rep-trace t)

(defcustom rep-underline-changes-color nil
  "If this is set to a color name such as \"red\" then the
substitution changes will also be underlined in that color.  If
it is set to t, then the changes will be underlined in the same
color as their markup face.  See \\[rep-lookup-markup-face].")

(defvar rep-font-lock-buffer-status nil
  "Buffer local variable to store the previous font-lock-mode status.
This allows us to remember that font-lock-mode was on, and should be
re-enabled after changes are accepted.")
(make-variable-buffer-local 'rep-font-lock-buffer-status)

(defvar rep-default-substitutions-directory nil
  "The location to place newly created files of substitution commands.
Note: include a trailing slash.
If this is nil, then a sub-directory named \".rep\" will
be created in parallel with the file to be modified.")

;; Note, at present, the *.rep file-extension is hard-coded.
(defvar rep-default-substitutions-file-name-prefix "substitutions"
  "This is used to name newly created files of substitution commands.
By default, the name would typically be something like
\"substitutions-273-DJE.rep\".")

;; Note, at present, the *.bak file-extension is hard-coded.
(defvar rep-standard-backup-location nil
  "The location to place back-up copies of modified files.
Note: include a trailing slash.
If this is nil, then a sub-directory named \".rep\" will
be created in parallel with the file to be modified.")

(defvar rep-previous-versions-stack ()
  "Buffer local stack of previous backup versions.
Each run of a set of substitutions on a file will generate
another backup file.  Reverts can trace this stack upwards to get
back to any version.")
(make-variable-buffer-local 'rep-previous-versions-stack)
(put 'rep-previous-versions-stack 'risky-local-variable t)

(defvar rep-change-metadata ()
  "Buffer local stash of the change metadata returned from rep.pl.
This has been unserialized into a list-of-lists.
The fields in each record: pass, beg, end, delta, orig,
all integers except for orig, which is a string.")
(make-variable-buffer-local 'rep-change-metadata)
(put 'rep-change-metadata 'risky-local-variable t)

;; TODO Document?
;; Text properties:
;;   rep-last-change
;;   rep-change-stack


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
;; hardcoded look-up table (stupid, but simple)
;; TODO (1) look into vectors... or just use an array?
;;      (2) have rep-make-face generate.
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
(defun rep-lookup-markup-face (pass)
  "Given an integer PASS, returns an appropriate face from \\[rep-face-alist].
These faces are named rep-NN-face where NN is a two-digit integer.
In the event that PASS exceeds the number of such defined faces, this
routine will wrap around and begin reusing the low-numbered faces.
If PASS is nil, this will return nil.
Underlining may be turned on with `rep-underline-changes-color'."
  (if rep-trace (message "%s" "rep-lookup-markup-face"))
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

(defun rep-split-on-semicolon-delimited-lines ( text )
  "Splits text on line-endings with semi-colons.
This allows for \"lines\" with embedded newlines, but any
embedded semi-colons are expected to be escaped with a backslash.
The escaping backslashes are removed."
  (if rep-trace (message "%s" "rep-split-on-semicolon-delimited-lines"))
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
    (while (<= beg (1- fin))
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
              (replace-regexp-in-string "\\\\;" ";" line))
        (push line lines)
        (setq beg (string-match "^" text end))
        ))
    (setq lines (nreverse lines))
    lines))

(defun rep-sub-directory (file-location)
  "Given a directory, returns path to a '.rep' sub-directory.
If the sub-directory does not exist, this will create it. "
  (if rep-trace (message "%s" "rep-sub-directory"))
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
  (if rep-trace (message "%s" "rep-generate-random-suffix"))
  ;; As written, this is always 3 upper-case asci characters.
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
;;  (2) a rep-substitutions-mode, with C-c.R binding to apply to other window.
;;  (3) a rep-modified-mode: a minor-mode automatically enabled in that
;;      other window once it's been modified.  This has keybindings to
;;      examine, undo, revert or accept the changes.

(defun rep-standard-setup (&optional dont-touch-tab)
  "Perform the standard set-up operations.
Calling this is intended to be a single step to get useful
keybindings and so on.
If you agree with our ideas about set-up, you can just run this,
if you'd rather do it yourself, then skip this, and the rep.el
package will use more unobtrusive defaults.
Note: the \"standard\" behavior is what is better documented.
If the optional DONT-TOUCH-TAB flag is set to t, tab and backtab
bindings should be left alone."
  (if rep-trace (message "%s" "rep-standard-setup"))
  (cond ((rep-probe-for-rep-pl)
         (message "rep.pl must be in PATH for rep.el to work.")
         ))

  (unless dont-touch-tab
    (rep-define-rep-modified-rebind-tab))

  (add-to-list
   'auto-mode-alist
   '("\\.\\(rep\\)\\'" . rep-substitutions-mode))

  (define-key rep-substitutions-mode-map "\C-x#"
    'rep-substitutions-apply-to-other-window)

  ;; bind global "entry point" command to "C-c.S"
  (let* ((prefix rep-key-prefix)
         )
    (global-set-key (format "%sS" prefix) 'rep-open-substitutions)
    (if rep-debug
        (message "Defined bindings for key: S under the prefix %s" prefix)))
  )

(defun rep-open-substitutions ()
  "Open a new substitutions file buffer.
This is the intended entry-point command.  It should have a
\"global\" keybinding which ideally would be available in all
\\(or mostly all\\) modes \\(though emacs doesn't make that easy\\).

This will typically open up a file something like this:

   .rep/substitutions-832-JDE.rep

Where the sub-directory '.rep' is located in the same place
as the file it was assuming you were about to modify.
The numeric value in the name is the process id, and
the unique 3 letter suffix is randomly chosen.

If you have the `rep-default-substitutions-directory' variable
set to some location, then the *.rep files will all be located
there.

The standard prefix \\(default: \"substitutions\"\\) comes from
this variable: `rep-default-substitutions-file-name-prefix'."
  (if rep-trace (message "%s" "rep-open-substitutions"))
  (interactive)
  (let* ((file-location (file-name-directory (buffer-file-name)))
         (dir (or
               rep-default-substitutions-directory
               (rep-sub-directory file-location)))
         (name rep-default-substitutions-file-name-prefix )
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
  (if rep-trace (message "%s" "rep-open-substitutions-prompt"))
  (interactive "FName of substitutions file:")
  (rep-open-substitutions-file-buffer-internal name )
  )

(defun rep-open-substitutions-file-buffer-internal ( file )
  "Open a new substitutions file buffer, given the full FILE name.
This goes through some gyrations to get enough space to create the new
window without being too obnoxious about it.
This just handles the window management and template insertion.
Choosing the file name and location is a job for routines such as
\\[rep-open-substitutions]."
  (if rep-trace (message "%s" "rep-open-substitutions-file-buffer-internal"))
  (interactive)
  (let* (
         (hint
           (concat
           "# Enter s///g; lines, "
           "/e not allowed /g assumed, "
           "C-c.r runs on other window"))
         (length-header (length hint))
         (substitution-template "s///g;" )
         start-here
         (hint2
           (concat
           "# In modififed buffer, the key prefix is 'C-c.':\n"
           "# Skip to change: C-c.n "
           "Undo one change: C-C.u "
           "Accept all: C-c.A "
           "Revert all: C-c.R "))
         (f-height (frame-height) )
         (w-height (window-body-height) )
         (number-lines 10 )
         (need-window-lines (round (* 1.5 number-lines)) )
         (expansion-limit (- f-height w-height))
         (current-deficit (- need-window-lines w-height ))
         )
    (cond ((> w-height need-window-lines)
           (split-window-vertically number-lines)
          )
          ((<= current-deficit expansion-limit)
           (enlarge-window current-deficit)
           (split-window-vertically number-lines)
           )
          (t
           ;; fall back: enlarge a few lines, cut new window size in half,
           (enlarge-window 2)
           (split-window-vertically (round (/ number-lines 2)))
           )
          )
    (find-file file)
    (rep-substitutions-mode)
    (insert hint)
    (put-text-property 1 length-header 'read-only t)
    (insert "\n") ;; check portability?

    (setq start-here (point))

    (open-line 6)
    (goto-char (point-max))
    (insert hint2)

    (goto-char start-here)
    (insert substitution-template)
    (move-beginning-of-line 1)
    (forward-char 2)
    ))


(define-derived-mode rep-substitutions-mode
  cperl-mode "rep-substitutions"
  "Major mode to enter stack of substitutions to be applied.
Derived from cperl-mode, because we're editing substitutions
that use perl's syntax \(and are interpreted using perl\).
\\{rep-substitutions-mode-map}"
  (use-local-map rep-substitutions-mode-map))


;; TODO
;; An issue that I don't quite understand:  Using footnote.el as
;; an example, you'll see that it defines the key prefix for it's
;; minor-mode using some sort of vector of chars, like the
;; "rep-key-prefix-internal" here:
;;
;; (defcustom rep-key-prefix-internal [(control ?c) ?.]
;;   "Prefix key to use for the rep-modified-mode minor mode.
;; This is in the \"internal Emacs key representation\".
;; The value of this variable is checked as part of loading rep-modififed-mode.
;; After that, changing the prefix key requires manipulating keymaps."
;;   )
;;
;; The reason for this is apparently some sort of limitation of custom.el.
;; Since I can't stand custom.el, I'm dropping support for it for now,
;; and going with a method of specifying keybindings that I understand
;; better.

(defvar rep-key-prefix (kbd "C-c .")
  "Prefix key to use for the rep-modified-mode minor mode.")

(defvar rep-modified-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map "w"
      'rep-modified-what-was-changed-here-verbose)
    (define-key map "x"
      'rep-modified-examine-properties-at-point)
    (define-key map "X" 'describe-text-properties)
    (define-key map "u" 'rep-modified-undo-change-here)
    (define-key map "R" 'rep-modified-revert-all-changes)
    (define-key map "@" 'rep-modified-accept-changes)
    (define-key map "A" 'rep-modified-accept-changes)
    (define-key map "n" 'rep-modified-skip-to-next-change)
    (define-key map "p" 'rep-modified-skip-to-prev-change)
    map))

(defvar rep-modified-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map rep-key-prefix rep-modified-mode-map)
    map)
  "Keymap used for binding rep-modified minor mode.")

(define-minor-mode rep-modified-mode
  "Toggle Rep Modified mode.
     With no argument, this command toggles the mode.
     Non-null prefix argument turns on the mode.
     Null prefix argument turns off the mode.

     When Rep Modified mode is enabled, key bindings are defined
     to examine and undo the changes made by rep substitutions.
     These are commands such as \\[rep-modified-undo-change-here], and
      \\[rep-modified-revert-all-changes].
      See: \\{rep-modified-mode-map}."
  ;; The initial value.
  :init-value nil
  ;; The indicator for the mode line.
  :lighter " Rep"
  :keymap     rep-modified-minor-mode-map
  )


(defun rep-define-rep-modified-rebind-tab ()
  "Re-binds the tab (and backtab) key in rep-modified-mode."
  (define-key rep-modified-minor-mode-map [tab]
    'rep-modified-skip-to-next-change)
  (define-key rep-modified-minor-mode-map [backtab]
    'rep-modified-skip-to-prev-change)
  )

;;--------
;; rep-substitutions-mode function(s)
(defun rep-substitutions-apply-to-other-window ()
  "Two buffers must be open, the list of substitution command
and the file they will modify, with the substitutions window
selected.  Each substitution command and the changes it produces
in the other window will be highlighted in corresponding colors.
Turns off font-lock to avoid conflict with existing syntax coloring."
  (interactive)
  (if rep-trace (message "%s" "rep-substitutions-apply-to-other-window"))
  (let ( raw-change-metadata
         change-metadata
         changes-list-file
         changes-list-buffer
         target-file
         target-file-buffer
         backup-file
         )

    (setq changes-list-file    (buffer-file-name))
    (setq changes-list-buffer  (current-buffer))
    (save-buffer)

    (other-window 1) ;; cursor in buffer to modify now
    (setq target-file          (buffer-file-name))
    (setq target-file-buffer   (current-buffer))
    (setq backup-file          (rep-generate-backup-file-name target-file))
    (save-buffer)

    (setq raw-change-metadata
          (rep-run-perl-substitutions
           changes-list-file target-file backup-file t))

    (if rep-debug
        (message "raw-change-metadata from rep-run-perl-substitutions: %s"
                 raw-change-metadata))

    (cond ((not (> (length raw-change-metadata) 1))
           (message "No changes made by substitutions."))
          ((string-match "^Problem" raw-change-metadata) ;; error message
           (message "%s" raw-change-metadata))
          (t
           (setq change-metadata
                 (rep-unserialize-change-metadata raw-change-metadata))

           (if rep-debug
             (message "change-metadata from rep-unserialize-change-metadata: %s"
                        change-metadata))

           (rep-markup-target-buffer-replay-perl-changes
               change-metadata target-file-buffer backup-file)
           (rep-markup-substitution-lines changes-list-buffer)

           ;; jump to the first change in the modified buffer
           (let ((spot (next-single-property-change
                        (point-min)
                        'rep-last-change target-file-buffer))
                 )
             (cond ((not (integer-or-marker-p spot))
                    (message "No marked-up changes found in buffer."))
                   (t
                    (goto-char spot)
                    (rep-modified-what-was-changed-here)
                    )))
           )
          )
    ))

;; Used by rep-substitutions-apply-to-other-window
(defun rep-generate-backup-file-name (file)
  "Given a FILE name, generate a unique backup file name.
If `rep-standard-backup-location' is non-nil, it will be used
as the standard location for backups, otherwise, a "e"sb -directory will be used in parallel with the FILE."
  (interactive)
  (if rep-trace (message "%s" "rep-generate-backup-file-name"))
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

(defun rep-probe-for-rep-pl ()
  "Probe the system for the \"rep.pl\" external program.
Returns t if found, nil otherwise.  As a side-effect, generates a
warning message if it isn't found."
  (if rep-trace (message "%s" "rep-probe-for-rep-pl"))
  (let* (
         (rep-pl "rep.pl")
         (cmd    (format "%s --version" (shell-quote-argument rep-pl)))
         (result (shell-command-to-string cmd))
         (expected-pat
          (format "^Running[ ]+%s[ ]+version:" rep-pl))
         )
    (cond ((not (string-match expected-pat result))
           (message "The program %s does not seem to be in your PATH." rep-pl)
           nil)
          (t
           t)
         )
    ))

;; Used by rep-substitutions-apply-to-other-window
(defun rep-run-perl-substitutions ( changes-list-file target-file backup-file
                                       &optional no-changes )
  "Applies substitutions in a CHANGES-LIST-FILE to a TARGET-FILE.
The CHANGES-LIST-FILE should contain substitutions in the traditional
unix 's///' style \(perl5 flavor\), one on each line.  The changes
are made throughout the TARGET-FILE as though the /g modifier was
used on all of them.  The original file is saved as the given BACKUP-FILE.
If NO-CHANGES is t, then the TARGET-FILE will not actually be modified."
  (if rep-trace (message "%s" "rep-run-perl-substitutions"))
  (let* (
         (rep-pl "rep.pl")
         perl-rep-cmd
         data
         cmd-format
         )
    (setq cmd-format
          (cond (no-changes
                 "%s --backup %s --substitutions %s --target %s --trialrun"
                 )
                (t
                 "%s --backup %s --substitutions %s --target %s "
                 )
                ))
    (setq perl-rep-cmd
               (format cmd-format
                rep-pl
                (shell-quote-argument
                  backup-file)
                (shell-quote-argument
                  changes-list-file)
                (shell-quote-argument
                  target-file)))
    (if rep-debug
        (message "%s" perl-rep-cmd))
    (setq data (shell-command-to-string perl-rep-cmd))
    (if rep-debug
        (message "%s" data))
    data))

;; Used by rep-substitutions-apply-to-other-window
(defun rep-markup-target-buffer-replay-perl-changes (metadata target-buffer
                                                              backup-file)
  "Applies the given change METADATA to the TARGET-BUFFER.
Highlights the changes using different color faces.
Sets rep-last-change for each modified region to the record
number of the METADATA.
Sets up the rep-change-stack text-property for each modification.
Presumes the target-buffer contains the original, unmodified
text.
Requires the METADATA to be in a list-of-alists form."
  (if rep-trace (message "%s" "rep-markup-target-buffer-replay-perl-changes"))
  (set-buffer target-buffer)
  ;; if font-lock-mode was on in target, save that information
  (setq rep-font-lock-buffer-status font-lock-mode)
  (font-lock-mode -1)

  (rep-modified-mode t)

  (push backup-file rep-previous-versions-stack)  ;; buffer-local variable
  (setq rep-change-metadata metadata)             ;; buffer-local variable
  (let ((count (length rep-change-metadata))
        (i 0)
        record
        )
    (while (<= i (1- count)) ;; stepping forward through change records
      ;; skip any empty records (if any)
      (if (setq record (aref rep-change-metadata i))
          (let* (
                 (pass   (rep-get 'record 'pass))
                 (beg    (rep-get 'record 'beg))
                 (end    (rep-get 'record 'end))     ;; after change
                 (fin    (rep-adjust-end end beg))
                 (delta  (rep-get 'record 'delta))
                 (end1   (- end delta))              ;; before change
                 (fin1   (rep-adjust-end end1 beg))
                 (orig   (rep-get 'record 'orig))
                 (rep    (rep-get 'record 'rep))      ;; NEW
                 (pre    (rep-get 'record 'pre))      ;; NEW
                 (post   (rep-get 'record 'post))     ;; NEW
                 (markup-face (rep-lookup-markup-face pass))
                 string1
                 )

            ;; check the substring at beg & end1: make sure it matches orig
            (setq string1 (buffer-substring-no-properties beg end1))
            (cond ((not (string= string1 orig))
                   (message
                    "Warning: at %d, \"%s\" is not \"%s\"" beg string1 orig)
                   )
                  (t
                   ;; TODO EXTRA CREDIT: try to recover from problems here.
                   ;; check pre/post values, use them to sync up, in the
                   ;; event of positioning errors
                   ;; (robust, defensive programming, or sloppy hackery? You
                   ;; decide).
                   )
                  )
            ;; delete the old substring, insert rep
            (setq string1 (buffer-substring beg end1))
            (delete-region beg end1)
            (goto-char beg)
            (insert rep)

            ;; markup: set text-properties face, rep-last-change...
            (put-text-property beg
                               end 'face markup-face)

            (put-text-property beg fin 'rep-last-change i) ;; the record number

            ;; push stack with string1 (orig *with* text-properties)
            (rep-push-change-stack string1 beg end)
            ))
      (setq i (1+ i))
      )
    ))

(defun rep-push-change-stack (string beg end)
  "Given the BEG and END of a range, pushes the STRING string onto the stack.
The stack in this case is the text-property \"rep-change-stack\".
This operates on the current buffer."
  (if rep-trace (message "%s" "rep-push-change-stack"))
  (let* ( stack i fin )
    ;; we must preserve history of each char, so we must push the
    ;; stack for each, individually, though we're pushing the same
    ;; string onto every stack.
    (setq fin (rep-adjust-end end beg))
    (setq i beg)
    (while (<= i fin)
      (setq stack
            (get-text-property i
                               'rep-change-stack))
      (push string stack)
      (put-text-property i
                         (1+ i)
                         'rep-change-stack stack)
      (setq i (1+ i))
      )))

(defun rep-pop-change-stack (beg end)
  "Given the BEG and END of a range, pops the current state off of the stack.
If the top of the stack for each character in the range contains the same
string, it will return that string.  Otherwise, it will refuse to pop, and
will return nil.
The stack in this case is the text-property \"rep-change-stack\".
This operates on the current buffer."
  (if rep-trace (message "%s" "rep-pop-change-stack"))
  (let* (
         (temp-string "")
         (string "")
         (stack ())
         (fin (rep-adjust-end end beg))
         (temp-list ())
         )
    ;; peek at top values of stack for all chars, stash in temp-list
    (let ((i beg))
      (while (<= i (1- fin))
        (setq stack
              (get-text-property i
                                 'rep-change-stack
                                ))
        (setq temp-string
              (nth 0 stack))

        (unless (stringp temp-string)
          (message "On char %d, not a string: %s" i temp-string))

        ;; was getting stringp error, when (nth 0 stack) was nil
        (cond ((stringp temp-string)
               (setq string
                     (substring-no-properties temp-string))

               (push string temp-list)
               ))
        (setq i (1+ i))
        ))

    ;; skip unless the top of the stack is same for all chars
    (cond ((not (rep-all-ok-p temp-list))
           nil)
          (t
           ;; pop stack on each character in range
           (let ((i beg))
             (while (<= i (1- fin))
               (setq stack
                     (get-text-property i
                                        'rep-change-stack
                                        ))
               (setq string
                     (pop stack))
               (put-text-property i
                                  (1+ i)
                                  'rep-change-stack
                                  stack
                                  )
               (setq i (1+ i))
               ))
           string
           ))
    ))

(defun rep-all-ok-p (string-list)
  "Check that all elements in STRING-LIST are the same string *and* non-nil."
  (if rep-trace (message "%s" "rep-all-ok-p"))
  (and (car string-list)
       (rep-all-equal-p temp-list)
       ))

(defun rep-all-equal-p (string-list)
  "Check that all elements in STRING-LIST are the same string.
If the list is short, with less than two elements, this will return t,
declaring them \"all-equal\" by definition."
  (if rep-trace (message "%s" "rep-all-equal-p"))
  (let* ((len (length string-list)))
    (cond ((<= len 1) ;; for 0 or 1 elements, by definition, all are equal
           t)
          (t
           (let* ((check-val (pop string-list)) ;; ref to compare to others
                  (temp-list (mapcar (function
                                      (lambda (str)
                                        (string= str check-val)))
                                     string-list))
                  (accum t)
                  )
             (dolist (condy temp-list)
               (setq accum (and accum condy))
               )
             accum)))))

;; Used by rep-markup-target-buffer-common
;;         rep-markup-target-buffer-replay-perl-changes
(defun rep-adjust-end (end beg)
  "If END and BEG are equal, returns END plus 1, otherwise just END.
This is to be used with the markup routines that may need to set a text
property on a string 0 characters long."
  (if rep-trace (message "%s" "rep-adjust-end"))
  (let ((adjusted-end
         (cond ((= beg end)
                (1+ end))
               (t
                end))))
    adjusted-end))

;; Used by rep-modified-undo-change-here
(defun rep-unadjust-end (end beg expected-len)
  "When END > BEG and yet EXPECTED-LEN is 0, return END minus 1, otherwise END.
This is used to compensate for when the markup routines need to
set a text property on a string 0 characters long: we cheat, and
pretend the string is one char long."
  (if rep-trace (message "%s" "rep-unadjust-end"))
  (let ((diff (- end beg))
        adjusted-end
        )
    (setq adjusted-end
          (cond ((and
                  (> diff expected-len)
                  (equal end (1+ beg)))
                 (1- end))
                (t
                 end)
                ))
    adjusted-end))

;; Used by rep-modified-accept-changes
(defun rep-kick-props (&optional buffer)
  "Clears the rep.el properties for the entire BUFFER.
Defaults to current buffer."
  (if rep-trace (message "%s" "rep-kick-props"))
  (setq buffer-read-only nil)
  (unless buffer
    (setq buffer (current-buffer)))
  (set-buffer buffer)
  (remove-list-of-text-properties (point-min) (point-max)
                                  '(rep-last-change
                                    rep-change-stack
                                    ))
  ;; clears *all* face settings.  if you want some, font-lock
  ;; better put 'em there.  (TODO)
  (remove-text-properties (point-min) (point-max) '(face nil))
  )

;; used by: rep-substitutions-apply-to-other-window
(defun rep-unserialize-change-metadata (data)
  "Converts the raw DATA from rep.pl to a lisp data structure.
That \"raw\" DATA is an aref of hrefs, and it is passed in JSON
form, so simply using the json package to decode it gets an
elisp array of alists."
  (if rep-trace (message "%s" "rep-unserialize-change-metadata"))
  (let* (change-metadata)
    (cond (data
           (setq change-metadata (json-read-from-string data))
           )
          (t
           (message "No change data returned from rep.pl.")
           ))
    change-metadata))


;; Used by rep-substitutions-apply-to-other-window
(defun rep-markup-substitution-lines (buffer)
  "Mark-up the substitution lines in the given BUFFER.
Uses the line number with rep-lookup-markup-face to
Assign a color to each substitution command in the buffer,
\(by counting from the top and feeding the position number
to \\[rep-lookup-markup-face]\).
Presumes all substitution commands begin with \"s\".
Acts on the given BUFFER, but leaves the current window active."
  (if rep-trace (message "%s" "rep-markup-substitution-lines"))
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

;; Used by: rep-modified-accept-changes, rep-modified-revert-all-changes
(defun rep-substitutions-mode-p ()
  "Check if the current buffer has the rep-substitutions-mode on."
  (if rep-trace (message "%s" "rep-substitutions-mode-p"))
  (let* ((this-mode major-mode)
         (mode-name "rep-substitutions-mode")
         )
    (string= this-mode mode-name)
    ))


;;--------
;; rep-modified-mode functions

(defun rep-modified-revert-all-changes ()
  "Revert last substitutions, restoring the previous backup file.
Uses the `rep-previous-versions-stack' buffer local variable."
  (interactive)
  (if rep-trace (message "%s" "rep-modified-revert-all-changes"))
  (let* ( (current-buffer-file-name (buffer-file-name))
          (previous-file (pop rep-previous-versions-stack))
          (preserve-stack rep-previous-versions-stack)
               )
    (cond ((not previous-file)
           (message "No previous version found on stack."))
          ((not (file-exists-p previous-file))
            (message "rep.el backup file not found: %s" previous-file))
          (t
           (copy-file previous-file current-buffer-file-name t)
           (revert-buffer t t)))

    ;; covering flakiness in revert-buffer & text properties.
    (font-lock-fontify-buffer)

    ;; in case you want to revert another step up the stack
    (rep-modified-mode t)
    (setq rep-previous-versions-stack preserve-stack)
    ;; also restore cperl syntax colors in substitutions window
    (save-excursion
      (other-window -1)
      (cond ((rep-substitutions-mode-p)
             (font-lock-mode -1)
             (font-lock-fontify-buffer)
             (other-window 1)))
      )
    ))

;; TODO could write an inverse of this:
;;     rep-modified-display-changes-again
(defun rep-modified-accept-changes ()
  "Accept changes made in buffer, return to normal state.
Restores the standard syntax coloring, etc."
  (interactive)
  (if rep-trace (message "%s" "rep-modified-accept-changes"))
  (let ((file  (buffer-file-name))
        )
    (setq buffer-read-only nil)
    (rep-modified-mode -1)

    ;; turn font-lock back on if it was on
    (cond (rep-font-lock-buffer-status
        (font-lock-mode 1)
        (font-lock-fontify-buffer)
        ))

    (rep-kick-props)
    (save-buffer)
    ;; also restore cperl syntax colors in substitutions window
    (save-excursion
      (other-window -1)
      (cond ((rep-substitutions-mode-p)
             (font-lock-mode 1)
             (font-lock-fontify-buffer)
             (other-window 1))))
    (message "rep.el: Changes accepted to %s." file)
    ))

(defun rep-modified-skip-to-next-change ()
  "Move forward to the next beginning point of a changed region.
Uses `rep-last-change'."
  (interactive)
  (if rep-trace (message "%s" "rep-modified-skip-to-next-change"))
  (let* ((property 'rep-last-change)
         (loc (point))
         )
    (while (progn
             (setq loc
                   (next-single-property-change loc property))
             (cond ((integer-or-marker-p loc)
                    (goto-char loc)
                    (not (rep-modified-at-start-of-changed-region))
                    );; keep going until at beg edge
                   (t
                    (message "No more changed regions after this.")
                    nil) ;; so exit while loop
                   )
             ))
    (if (integer-or-marker-p loc)
        (rep-modified-what-was-changed-here))
    ))

(defun rep-modified-skip-to-prev-change ()
  "Move back to the previous changed region, stopping at the beginning point.
Uses `rep-last-change'."
  (interactive)
  (if rep-trace (message "%s" "rep-modified-skip-to-prev-change"))
  (let* ((property 'rep-last-change)
         (loc (point))
         )
    (while (progn
             (setq loc
                   (previous-single-property-change loc property))
             (cond ((integer-or-marker-p loc)
                    (goto-char loc)
                    (not (rep-modified-at-start-of-changed-region))
                    );; keep going until at beg edge
                   (t
                    (message "No more changed regions before this.")
                    nil) ;; so exit while loop
                   )
             ))
    (if (integer-or-marker-p loc)
        (rep-modified-what-was-changed-here))
    ))

(defun rep-modified-examine-properties-at-point ()
  "Tells you all of the text property settings at point.
This function displays the properties in the message bar,
as opposed to `describe-text-properties' which opens another
buffer window for them."
  (interactive)
  (if rep-trace (message "%s" "rep-modified-examine-properties-at-point"))
  (let* (capture)
    (setq capture (text-properties-at (point)))
    (message (pp-to-string capture))
    ))

(defun rep-modified-what-was-changed-here ()
  "Tells you the original string was before it was replaced."
  (interactive)
  (if rep-trace (message "%s" "rep-modified-what-was-changed-here"))
  (let* ( (last-change (get-text-property (point) 'rep-last-change))
          )
    (cond ((eq rep-change-metadata nil)
           (message "Problem in %s: rep-change-metadata is nil."
                    "rep-modified-what-was-changed-here")
           )
          (last-change
           (let* (
                  (record (aref rep-change-metadata last-change))
                  (last-string (rep-get 'record 'orig))
                  )
             (message "Was: %s" last-string)
             ))
          )
    ))

(defun rep-modified-what-was-changed-here-verbose ()
  "Tells you the original string was before it was replaced.
Looks at the changed string under the cursor, or if we're not
inside a change, tries to advance the cursor to the next change.
This also supplies additional information like the number of the
substitution that made the change."
  (interactive)
  (if rep-trace (message "%s" "rep-modified-what-was-changed-here-verbose"))
  (let* ( (last-change (get-text-property (point) 'rep-last-change))
          )
    (cond ( (not last-change) ;; we are not yet inside a changed region
            (goto-char
             (next-single-property-change (point) 'rep-last-change))
            (setq last-change (get-text-property (point) 'rep-last-change))
            ))
    (cond (last-change
            (let* (
                  (record (aref rep-change-metadata last-change))
                  (last-string (rep-get 'record 'orig))
                  (pass        (rep-get 'record 'pass))
                  )
              (message
               "This was: %s (changed by the substitution on line: %d)."
               last-string
               (1+ pass)
               )
              ))
          (t
           (message "There are no further substitution changes in this buffer.")
           ))
    ))

(defun rep-modified-undo-change-here ()
  "Undos the individual rep substitution change under the cursor.
Undos the change at point, or if none is there, warns and does nothing.
Note that this has nothing to do with the usual emacs \"undo\"
system, which operates completely independently."
  (interactive)
  (if rep-trace (message "%s" "rep-modified-undo-change-here"))
  (let* ((buffer         (current-buffer))
         (pair           (rep-modified-extent-of-change))
         (beg            (nth 0 pair))
         (end            (nth 1 pair))
         stack
         )
    (cond ((not (and beg end))
           (message "No changed region to undo at point.")
           )
          (t
           (let* ((record-number
                   (get-text-property beg 'rep-last-change))

                  (record (aref rep-change-metadata record-number))
                  (pass        (rep-get 'record 'pass))
                  (delta       (rep-get 'record 'delta))
                  (orig        (rep-get 'record 'orig))

                  (orig-len  (length orig))
                  (expected-len (+ orig-len delta))

                  (end (rep-unadjust-end end beg expected-len))
                  (existing (buffer-substring-no-properties beg end))
                  )
             (cond ((not (= expected-len (- end beg)))
                    (message (concat
                              (format "Can't revert fragment: %s. " existing)
                              "Must undo adjacent change first." )) )
                   (t
                    (let* ((stringy ""))

                      (setq stringy (rep-pop-change-stack beg end))
                      (delete-region beg end)
                      (goto-char beg)
                      (insert stringy)

                     (goto-char beg)
                     (message "Change reverted: %s" existing)
                     ))))
           ))))

;;--------
;; utilitites used by other rep-modified-* functions

;; TODO This really belongs in a general package of property utilities
(defun rep-rising-or-falling-edge (property &optional loc)
  "Examines PROPERTY at LOC (or point) tells where you are in it's extent.
Tells if you're at the \"start\", the \"middle\" and/or the \"end\",
returning the set of results as a list of strings.  Note:
you can be both at the \"start\" and the \"end\", because
a region can be one-character long.
Returns nil if the property is not defined."
  (if rep-trace (message "%s" "rep-rising-or-falling-edge"))
  (unless loc
    (setq loc (point)))
  (let ((ret ())
        ;; the property's setting at this point
        (this (get-text-property loc property))
        )
    (cond ((not this) ;; we're not inside a marked region
           (setq ret nil))
          (t ;; we are inside
           ;; first check whether we're at the beginning and/or end of buffer
           ;; (Note: the buffer might have only one character)
           (if (and this (eq loc (point-min)))
               (push "start" ret)
             )
           (if (and this (eq loc (point-max)))
               (push "end" ret)
             )
           ;; only if we're not at the top or bottom
           (cond ((not (or
                        (eq loc (point-min)) (eq loc (point-max))))
                  (let* ((prev (get-text-property (1- loc) property))
                         (next (get-text-property (1+ loc) property))
                         )
                    (cond ((and (equal next this) (equal prev this))
                           (push "middle" ret))
                          ((not (equal prev this))
                           (push "start" ret))
                          ((not (equal next this))
                           (push "end" ret))
                          ))))
           ))
    ret))

(defun rep-modified-at-start-of-changed-region ()
  "Returns t if point is at the start of a new rep-last-change regions.
That means we're at the beginning of a range where the text-property
rep-last-change is set to some value."
  (if rep-trace (message "%s" "rep-modified-at-start-of-changed-region"))
  (let* ((list (rep-rising-or-falling-edge 'rep-last-change))
         (ret (member "start" list))
         )
    ret))

(defun rep-modified-at-end-of-changed-region ()
  "Returns t if point is at the end of a new rep-last-change regions.
That means we're at the beginning of a range where the text-property
rep-last-change is set to some value."
  (if rep-trace (message "%s" "rep-modified-at-end-of-changed-region"))
  (let* ((list (rep-rising-or-falling-edge 'rep-last-change))
         (ret (member "end" list))
         )
    ret))

(defun rep-modified-extent-of-change ()
  "Returns the BEG and END of extent of the change at the cursor.
This looks for the `rep-last-change' text-property.  If the cursor
is inside a modified region, this function finds the extent of it.
Returns a list of the two coordinates (character numbers
counting from the start of the buffer)."
  (if rep-trace (message "%s" "rep-modified-extent-of-change"))
;; The difficulty here is that if we're just at the start of the change,
;; "previous-single-property-change" wants to skip us way back to the
;; previous change.  We have to dance around this irritating behavior.
;; TODO similar problem at end of change?
  (interactive)
  (let ( mod beg end peek-back )
    (setq mod (get-text-property (point) 'rep-last-change))
    ;; since mod is defined we are inside a changed region..
    (cond (mod
           ;;... but we need to worry about being at the start
           ;; of the change.
           (setq beg
                 ;; TODO break-out as rep-move-to-start-of-change
                 (cond ((rep-modified-at-start-of-changed-region)
                        (point)
                        )
                       (t
                        (previous-single-property-change (point)
                                                         'rep-last-change)
                        )))
           (setq end
                 ;; TODO break-out as rep-move-to-end-of-change.  mimic above?
                 (next-single-property-change (point)
                                              'rep-last-change))
                 )
          ;; any handling of the not-inside-change-case?
          )
    (list beg end)))

;;========
;; general elisp utililities
;; alist manipulation

(defun rep-get (alist-symbol key)
  "Look up value for given KEY in ALIST.
Example:
   (rep-get 'rep-general-alist 'C)
Note: must quote alist name."
  (if rep-trace (message "%s" "rep-get"))
  (let* ((value (cdr (assoc key (eval alist-symbol))))
         )

    ;; blatant hackery... sometimes it's car/cdr, sometimes just cdr.
    (setq value
          (cond ((listp value)
                 (car value)
                 )
                (t
                 value
                 )))

    ;; automatic conversion of numeric strings to numbers
    ;; (dammit, I'm a perl programmer!)
    (cond ((not (stringp value))
           value)
          ((string-match "^[-+.0-9]+$" value)
           (string-to-number (match-string 0 value))
           )
          (t
           value)
          )
    ))

(defun rep-set (alist-symbol key value)
  "Store given VALUE under KEY in ALIST.
Example:
   (rep-set 'rep-general-alist 'C \"CCC\")
Note: must quote alist name."
  (if rep-trace (message "%s" "rep-set"))
  (set alist-symbol (cons (list key value) (eval alist-symbol)))
  )

;;========
;; debug tools

(defun rep-modified-select-change ()
  "Selects the changed region at point."
  (interactive)
  (if rep-trace (message "%s" "rep-modified-select-change"))
  (let* ((pair (rep-modified-extent-of-change))
         (beg (nth 0 pair))
         (end (nth 1 pair)))
    (goto-char beg)
    (set-mark beg)
    (goto-char end)
    (exchange-point-and-mark)
    ))

;;; rep.el ends here
