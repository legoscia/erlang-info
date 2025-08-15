;;; erlang-info.el --- browse Erlang documentation in Info  -*- lexical-binding: t; -*-

;; Copyright (C) 2025  Magnus Henoch

;; Author: Magnus Henoch <magnus.henoch@gmail.com>
;; Keywords: docs, hypermedia
;; Version: 0.0.1

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Browse Erlang documentation in Emacs using Info.
;;
;; This tool extracts documentation from BEAM files (using Docs chunks
;; as described in EEP 48), and does a moderately convincing
;; on-the-fly conversion to Info format, such that each Erlang/Elixir
;; module appears to be an Info file, with a node for each function or
;; type in the module.
;;
;; Since all the documentation is read from a local drive, this is
;; much faster than navigating documentation in a web browser.
;;
;; To use it, run M-x erlang-info and type in the name of a module.
;;
;; It tries to be clever in order to find the BEAM file for the module
;; in question:
;;
;; - When invoked from a directory within a Mix project, it will look
;;   for modules in the "_build" directory.
;; - If Erlang and/or Elixir are installed using ASDF, it will use
;;   "asdf where" to find the modules that come with the system.
;; - If it can't find the module, it will prompt for the path to the
;;   BEAM file.
;;
;; If it gets confused and ends up finding the wrong files, try
;; M-x erlang-info-clear-caches.

;;; Code:

(eval-when-compile (require 'subr-x))

(defvar erlang-info--beam-path ())

(defvar erlang-info--file->docs-chunk-cache (make-hash-table :test 'equal))

(defvar erlang-info--module->file-cache (make-hash-table :test 'equal))

(defvar erlang-info--looked-for-elixir-lib-dir nil)

(defvar erlang-info--looked-for-erlang-lib-dir nil)

(defvar erlang-info--minibuffer-history ())

;;;###autoload
(defun erlang-info (module)
  (interactive
   (list (completing-read
          "View documentation for Erlang module (or empty for menu): "
          (erlang-info--cached-module-names)
          nil nil nil 'erlang-info--minibuffer-history)))
  (if (equal module "")
      (info "*Erlang*")
    (let ((beam-file (erlang-info--ensure-module-found module t)))
      (if (null beam-file)
          (user-error "BEAM file for `%s' not found" module)
        (puthash module beam-file erlang-info--module->file-cache)
        (info (concat "*Erlang-" module))))))

(defun erlang-info-clear-caches ()
  (interactive)
  (setq erlang-info--beam-path ()
        erlang-info--looked-for-elixir-lib-dir nil
        erlang-info--looked-for-erlang-lib-dir nil)
  (clrhash erlang-info--file->docs-chunk-cache)
  (clrhash erlang-info--module->file-cache))

(defun erlang-info--ensure-module-found (module interactivep)
  (or
   ;; We've visited documentation for this module before
   (let ((file (gethash module erlang-info--module->file-cache)))
     (if (and file (file-readable-p file))
         file
       ;; But the file is no longer there!
       (remhash module erlang-info--module->file-cache)
       nil))
   ;; We can find this module in one of the directories we know about
   (catch 'found
     (dolist (dir erlang-info--beam-path)
       (let ((file (erlang-info--find-beam-file-in-dir module dir)))
         (when file
           (throw 'found file)))))
   ;; We can find a Mix "_build" directory in the current directory or
   ;; a parent directory
   (let ((dir-with-build (locate-dominating-file default-directory "_build")))
     (when dir-with-build
       (let* ((build-dir (expand-file-name "_build" dir-with-build))
              ;; Maybe it contains the file we're looking for?
              (file (erlang-info--find-beam-file-in-dir module build-dir)))
         ;; If so, remember the build dir for future reference
         (when file
           (add-to-list 'erlang-info--beam-path build-dir)
           file))))
   ;; We can find an Elixir executable, and from it find the Elixir lib directory
   (unless erlang-info--looked-for-elixir-lib-dir
     (let ((elixir-lib-dir (erlang-info--find-lib-dir "elixir" "elixir")))
       (setq erlang-info--looked-for-elixir-lib-dir t)
       (add-to-list 'erlang-info--beam-path elixir-lib-dir)
       (erlang-info--find-beam-file-in-dir module elixir-lib-dir)))
   ;; We can find an Erlang executable, and from it find the Erlang lib directory
   (unless erlang-info--looked-for-erlang-lib-dir
     (let ((erlang-lib-dir (erlang-info--find-lib-dir "erl" "erlang")))
       (setq erlang-info--looked-for-erlang-lib-dir t)
       (add-to-list 'erlang-info--beam-path erlang-lib-dir)
       (erlang-info--find-beam-file-in-dir module erlang-lib-dir)))
   ;; We can ask the user where the beam file is
   (when interactivep
     (let* ((beam-file (read-file-name
                        (concat "BEAM file for module " module ": ")
                        nil nil t nil
                        ;; not working?
                        ;; (lambda (n) (string-suffix-p ".beam" n))
                        ))
            (directory (file-name-directory beam-file)))
       (add-to-list 'erlang-info--beam-path directory)
       beam-file))))

(defun erlang-info--find-lib-dir (executable-name asdf-name)
  (let ((executable (executable-find executable-name)))
    (cond
     ((null executable)
      ;; Nothing found
      nil)
     ((string-match-p "/.asdf/shims/" executable)
      ;; We're using ASDF - ask it where the installation directory is
      (let ((installation-dir
             (string-trim
              (with-output-to-string
                (call-process "asdf" nil (list standard-output nil) nil "where" asdf-name)))))
        (when (and (not (string-empty-p installation-dir)) (file-directory-p installation-dir))
          (let ((lib-dir (concat installation-dir "/lib")))
            (when (file-directory-p lib-dir)
              lib-dir)))))
     (t
      ;; We found an executable and we're not using ASDF
      ;; TODO: handle this case
      nil))))

(defun erlang-info-find-file (filename &optional _noerror)
  (cond
   ((equal filename "*Erlang*")
    ;; top-level menu
    filename)
   ((string-match "^\\*Erlang-\\(.*\\)" filename)
    (let ((module (match-string 1 filename)))
      (or
       (erlang-info--ensure-module-found module nil)
       (error "BEAM file for module `%s' not found" module))))
   (t
    filename)))

(defun erlang-info--find-beam-file-in-dir (module dir)
  ;; module names are case sensitive, so use case sensitive regexp matching
  (let* ((case-fold-search nil)
         (regexp (concat "^\\(?:Elixir\\.\\)?" (regexp-quote module) "\\.beam$"))
         (files (directory-files-recursively dir regexp nil t))
         (readable-files (cl-remove-if-not 'file-readable-p files)))
    (when readable-files
      (car readable-files))))

(defun erlang-info-find-node (filename nodename &optional _no-going-back)
  (cond
   ((and (equal filename "*Erlang*") (equal nodename "Top"))
    (erlang-info-top-node))
   (t
    (insert "Filename: " filename "\nOther node: " nodename))))

(defun erlang-info-top-node ()
  ;; Hide references in module list
  (setq-local Info-hide-note-references t)
  (insert "\^_\nNode: Top\n\n"
          "Erlang module documentation in Info format.\n\n")
  (insert-button "View documentation for module..."
                 'action (lambda (_) (call-interactively 'erlang-info)))
  (insert "\n\n")
  (if (hash-table-empty-p erlang-info--file->docs-chunk-cache)
      (insert "No modules visited yet.")
    (let ((module-names (erlang-info--cached-module-names)))
      (insert "Previously visited modules:\n\n"
              "* Menu:\n\n")
      (dolist (m module-names)
        (let* ((file (gethash m erlang-info--module->file-cache))
               (docs-chunk (gethash file erlang-info--file->docs-chunk-cache))
               (summary (erlang-info--doc-summary
                         (erlang-info--pick-doc-language
                          (erlang-info--get-module-doc
                           docs-chunk)))))
          (insert (format "* %s: (*Erlang-%s)."
                          m m))
          (when summary
            (insert "  " summary))
          (insert "\n"))))))

(defun erlang-info--module-name-sans-elixir (s)
  (string-remove-prefix "Elixir." s))

(defun erlang-info--cached-module-names ()
  (let ((keys (hash-table-keys erlang-info--module->file-cache)))
    (cl-remove-duplicates
     (sort keys 'string<)
     :test 'equal)))

(defun erlang-info-beam-find-node (filename _nodename &optional _no-going-back)
  ;; Don't add "see" before links
  (setq-local Info-hide-note-references 'hide)
  ;;(message "looking for %S" nodename)
  (let ((docs-chunk (erlang-info--get-docs-chunk filename)))
    (insert "\^_\nNode: Top,  Up: ")
    (let* ((module-name-sans-elixir (erlang-info--module-name-sans-elixir (file-name-base filename)))
           (last-dot (cl-search "." module-name-sans-elixir :from-end t)))
      (if last-dot
          ;; A module name with dots, like Kernel.SpecialForms.
          ;; The "up" link should go to Kernel.
          (insert "(*Erlang-" (substring module-name-sans-elixir 0 last-dot) ")")
        ;; No dot in the module name.
        ;; The "up" link goes to the list of modules we've seen.
        (insert "(*Erlang*)")))
    (insert "\n\nThis is file " filename ".\n\n")
    (let* ((module-doc (erlang-info--get-module-doc docs-chunk))
           (entities-docs (erlang-info--get-entities-docs docs-chunk))
           prev)
      ;;(message "reading %S" docs-chunk)
      (erlang-info--insert-markdown (erlang-info--pick-doc-language module-doc))
      (insert "\n\n")
      (insert "* Menu:\n\n")
      (dolist (e entities-docs)
        (let ((doc (nth 4 e)))
          (unless (equal "hidden" doc)
            (insert "* " (erlang-info--entity-node-name e) "::")
            (let ((summary (erlang-info--doc-summary (erlang-info--pick-doc-language doc))))
              (when summary
                (insert "  " summary)))
            (insert "\n"))))
      (insert "\n\n")

      (while entities-docs
        (let* ((e (pop entities-docs))
               (kind-name-arity (nth 1 e))
               (kind (nth 1 kind-name-arity))
               ;; (name (nth 2 kind-name-arity))
               ;; (arity (nth 3 kind-name-arity))
               (signatures (cdr (nth 3 e)))
               (doc (nth 4 e)))
          (insert "\^_\nFile: " filename ",  Node: " (erlang-info--entity-node-name e))
          (when (car entities-docs)
            (insert ",  Next: " (erlang-info--entity-node-name (car entities-docs))))
          (when prev
            (insert ",  Prev: " (erlang-info--entity-node-name prev)))
          (insert ",  Up: Top\n\n")
          (setq prev e)

          (insert kind "\n\n")
          (dolist (signature signatures)
            (insert signature "\n"))
          (insert "\n")
          (erlang-info--insert-markdown (erlang-info--pick-doc-language doc))
          (insert "\n\n"))))))

(defun erlang-info--get-module-doc (docs-chunk)
  (nth 5 docs-chunk))

(defun erlang-info--get-entities-docs (docs-chunk)
  (cdr (nth 7 docs-chunk)))

(defun erlang-info--pick-doc-language (doc)
  (cond
   ((equal doc "none")
    "No documentation exists.")
   ((equal doc "hidden")
    "Documentation explicitly disabled.")
   ((and (listp doc) (eq (car doc) :map))
    ;; Try to find documentation in English
    (let ((en-doc (assoc "en" (cdr doc))))
      (if en-doc
          ;; TODO: check if this is text/markdown or application/erlang+html
          (decode-coding-string (cdr en-doc) 'utf-8)
        "No documentation in English available.")))))

(defun erlang-info--doc-summary (s)
  (when s
    ;; Extract the first paragraph, or use the entire string if there are no line breaks
    (let ((first-double-newline (string-match "\n\n" s)))
      (when first-double-newline
        (setq s (substring s 0 first-double-newline))))
    ;; 100 characters should be a reasonable length limit?
    (when (< (length s) 100)
      (replace-regexp-in-string "\n" " " s))))

(defun erlang-info--entity-node-name (entity)
  (let* ((kind-name-arity (nth 1 entity))
         (name (nth 2 kind-name-arity))
         (arity (nth 3 kind-name-arity)))
    (format "%s/%s" name arity)))

(defun erlang-info--insert-markdown (s)
  (let ((beg (point)))
    (insert s)
    (save-excursion
      (with-restriction beg (point)
        ;; Convert Markdown titles to Info format
        (goto-char (point-min))
        (while (re-search-forward "^\\(#+\\) \\(.*$\\)" nil t)
          (let ((title-level (- (match-end 1) (match-beginning 1)))
                (title-length (- (match-end 2) (match-beginning 2))))
            (replace-match "\\2" t)
            (insert "\n" (make-string title-length
                                      (cl-case title-level
                                        (1 ?*)
                                        (2 ?=)
                                        (3 ?-)
                                        (t ?.))))))
        ;; Convert Markdown links, when they point to modules or functions
        (goto-char (point-min))
        (while (re-search-forward "\\(?:\\[\\([^]\n]+\\)\\](`\\([^` \n]+\\))\\)\\|`\\([^` \n]+\\)`\\([.,]\\)?" nil t)
          (let* ((label (match-string 1))
                 (target (or (match-string 2) (match-string 3)))
                 (target-node-spec (erlang-info--link-node-spec target))
                 (final-dot-or-comma (match-string 4)))
            (when target-node-spec
              (if label
                  (replace-match (format "*note %s:%s%s"
                                         (erlang-info--cross-xref-label label)
                                         target-node-spec
                                         (or final-dot-or-comma ","))
                                 nil t)
                (replace-match (format "*note %s::%s"
                                       target-node-spec
                                       (or final-dot-or-comma "")))))))))))

(defun erlang-info--link-node-spec (target)
  (let ((case-fold-search nil))
    (save-match-data
      (cond
       ((string-match "^[a-z][^:./]*/[0-9]+$" target)
        ;; Link to a local function, e.g. "foo/1".
        ;; First character must be a lower-case character, to avoid false positives
        target)
       ((string-match "^\\(?:[ct]:\\)?\\([^:./]+/[0-9]+\\)$" target)
        ;; Link to a local callback or type
        (match-string 1 target))
       ((string-match "^\\(?:[ct]:\\)?\\([^:/]+\\)[:.]\\([^:./]+/[0-9]+\\)$" target)
        ;; Link to a remote function, e.g. "foo:bar/1"
        ;; Or a link to a remote type or callback, prefixed with "c:" or "t:"
        (format "(*Erlang-%s)%s" (match-string 1 target) (match-string 2 target)))
       ((string-match "^m:\\([^:/]+\\)$" target)
        ;; Link to a module, e.g. "m:foo"
        (format "(*Erlang-%s)Top" (match-string 1 target)))
       (t
        ;; Something else.  Let's not try to make a link out of it.
        nil)))))

(defun erlang-info--cross-xref-label (label)
  ;; In theory, cross-reference labels with colons should be quoted
  ;; with <del> characters (aka ^?), according to:
  ;; (info "(texinfo)Info Format Cross Reference")
  ;; But it turns out it's only supported by the stand-alone Info
  ;; reader, not by the one inside Emacs.
  ;;
  ;; Let's work around that by turning colons into something similar
  ;; but different.
  (replace-regexp-in-string
   ":"
   (string #xff1a) ;; U+FF1A FULLWIDTH COLON
   label))

(defun erlang-info--get-docs-chunk (filename)
  "Return the parsed Docs chunk of FILENAME.
Use cached value if possible."
  (let ((cached (gethash filename erlang-info--file->docs-chunk-cache)))
    (if (or (file-has-changed-p filename :erlang-info)
            (null cached))
        (puthash filename (erlang-info--decode-term
                           (erlang-info--extract-docs-chunk filename))
                 erlang-info--file->docs-chunk-cache)
      cached)))

(defun erlang-info--extract-docs-chunk (filename)
  (with-temp-buffer
    (set-buffer-multibyte nil)
    (insert-file-contents-literally filename)
    (goto-char (point-min))
    (when (looking-at "\x1f\x8b")
      (unless (and (fboundp 'zlib-available-p) (zlib-available-p))
        (error "This BEAM file is compressed, but Emacs is not built with zlib"))
      (with-silent-modifications
        (zlib-decompress-region (point-min) (point-max))))
    (goto-char (point-min))
    (unless (looking-at-p "FOR1")
      (error "Not a BEAM file"))
    (goto-char (+ (point) 12))
    (let (chunk-id-and-end)
      (while (progn
               (setq chunk-id-and-end (erlang-info--chunk-id-and-end))
               (and
                (not (equal (car chunk-id-and-end) "Docs"))
                (< (cdr chunk-id-and-end) (point-max))))
        (goto-char (cdr chunk-id-and-end)))
      (if (equal (car chunk-id-and-end) "Docs")
          (buffer-substring (+ (point) 8) (cdr chunk-id-and-end))
        (error "No Docs chunk found in `%s'" filename)))))

(defun erlang-info--chunk-id-and-end ()
  (let* ((chunk-id (buffer-substring (point) (+ (point) 4)))
         (chunk-length-string (buffer-substring (+ (point) 4) (+ (point) 8)))
         (chunk-length (erlang-info--decode-be-uint32 chunk-length-string))
         (chunk-length-padded
          (let ((rem (% chunk-length 4)))
            (if (zerop rem)
                chunk-length
              (+ chunk-length (- 4 rem)))))
         (chunk-ends-at (+ (point) 8 chunk-length-padded)))
    (cons chunk-id chunk-ends-at)))

(defun erlang-info--decode-be-uint16 (s)
  (logior
   (ash (aref s 0) 8)
   (ash (aref s 1) 0)))

(defun erlang-info--decode-be-uint32 (s)
  (logior
   (ash (aref s 0) 24)
   (ash (aref s 1) 16)
   (ash (aref s 2) 8)
   (ash (aref s 3) 0)))

(defun erlang-info--decode-term (s)
  (unless (= (aref s 0) 131)
    (error "Unexpected version number %s" (aref s 0)))
  (setq s (substring s 1))
  (let* ((decoded-and-end (erlang-info--decode-term-1 s))
         (decoded (car decoded-and-end)))
    decoded))

(defun erlang-info--decode-term-1 (s)
  (cl-case (aref s 0)
    (80
     ;; compressed
     (let ((decompressed
            (with-temp-buffer
              (set-buffer-multibyte nil)
              (insert (substring s 5))
              (zlib-decompress-region (point-min) (point-max))
              (buffer-string))))
       (erlang-info--decode-term-1 decompressed)))
    (97
     ;; unsigned 8-bit integer
     (cons (aref s 1) 2))
    (98
     ;; signed 32-bit-integer
     (let ((n (erlang-info--decode-be-uint32 (substring s 1 5))))
       (unless (zerop (logand n #x80000000))
         (setq n (- -1 (logxor n #xffffffff))))
       (cons n 5)))
    (104
     ;; small tuple
     (let ((tuple-size (aref s 1))
           (p 2)
           tuple-elements)
       (dotimes (_n tuple-size)
         (if (>= p (length s))
             (push :xxx tuple-elements)
           (let ((term-and-end (erlang-info--decode-term-1 (substring s p))))
             (push (car term-and-end) tuple-elements)
             (setq p (+ p (cdr term-and-end))))))
       (cons (cons :tuple (nreverse tuple-elements)) p)))
    (106
     ;; empty list
     (cons () 1))
    ((107 100)
     ;; STRING_EXT or ATOM_EXT: 16-bit length field followed by the text
     (let* ((len (erlang-info--decode-be-uint16 (substring s 1 3)))
            (data (substring s 3 (+ 3 len))))
       (cons data (+ 3 len))))
    (108
     ;; list with 32-bit length
     (let ((list-size (erlang-info--decode-be-uint32 (substring s 1 5)))
           (p 5)
           list-elements
           tail-element)
       (dotimes (_n list-size)
         (if (>= p (length s))
             (push :xxx list-elements)
           (let ((term-and-end (erlang-info--decode-term-1 (substring s p))))
             (push (car term-and-end) list-elements)
             (setq p (+ p (cdr term-and-end))))))
       ;; and a tail element
       (let ((term-and-end (erlang-info--decode-term-1 (substring s p))))
         (setq tail-element (car term-and-end))
         (setq p (+ p (cdr term-and-end))))
       (setq list-elements (nconc (nreverse list-elements) tail-element))
       (cons (cons :list list-elements) p)))
    (109
     ;; binary with 32-bit length
     (let* ((len (erlang-info--decode-be-uint32 (substring s 1 5)))
            (data (substring s 5 (+ 5 len))))
       (cons data (+ 5 len))))
    (116
     ;; map
     (let ((arity (erlang-info--decode-be-uint32 (substring s 1 5)))
           (p 5)
           map-pairs)
       (dotimes (_n arity)
         (if (>= p (length s))
             (push (cons :xxx :xxx) map-pairs)
           (let* ((key-and-end (erlang-info--decode-term-1 (substring s p)))
                  (value-and-end (erlang-info--decode-term-1 (substring s (+ p (cdr key-and-end))))))
             (push (cons (car key-and-end) (car value-and-end)) map-pairs)
             (setq p (+ p (cdr key-and-end) (cdr value-and-end))))))
       (cons (cons :map (nreverse map-pairs)) p)))
    (119
     ;; SMALL_ATOM_UTF8_EXT
     (let* ((len (aref s 1))
            (atom-name (decode-coding-string (substring s 2 (+ 2 len)) 'utf-8)))
       (cons atom-name (+ 2 len))))
    (t
     (cons
      (cons (aref s 0) (substring s 1))
      (length s)))))

(with-eval-after-load "info"
  (add-to-list 'Info-virtual-files
               '("\\`\\*Erlang.*"
                 (find-file . erlang-info-find-file)
                 (find-node . erlang-info-find-node)))
  (add-to-list 'Info-virtual-files
               '("\\.beam\\'"
                 (find-file . erlang-info-find-file)
                 (find-node . erlang-info-beam-find-node))))

(provide 'erlang-info)
;;; erlang-info.el ends here
