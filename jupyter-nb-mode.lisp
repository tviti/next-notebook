(uiop:define-package :next/jupyter-nb-mode
    (:use :next :common-lisp)
    (:documentation "Mode for interacting with Jupyter notebooks."))
(in-package :next/jupyter-nb-mode)

;; TODO: I think a macro might be nice for all of these glue-calls to JS.
;; That or lean on parenscript for doing this.

(define-command select-next-cell ()
  "Move one cell upwards, also moving the buffer's focus."
  (rpc-buffer-evaluate-javascript
   (current-buffer)
   (format nil "
Jupyter.notebook.select_next(true);
Jupyter.notebook.focus_cell();")))

(define-command select-prev-cell ()
  "Move one cell downwards, also moving the buffer's focus."
  (rpc-buffer-evaluate-javascript
   (current-buffer)
   (format nil "
Jupyter.notebook.select_prev(true);
Jupyter.notebook.focus_cell();")))

(define-command select-cell (idx)
  "Select cell idx, also moving the buffer's focus."
  (rpc-buffer-evaluate-javascript
   (current-buffer)
   (format nil "
Jupyter.notebook.select(~d, true);
Jupyter.notebook.focus_cell();" idx)))

(define-command select-first-cell ()
  (select-cell 0))

(define-command select-last-cell ()
  (rpc-buffer-evaluate-javascript
   (current-buffer)
   "
(function () {
    var nb = Jupyter.notebook;
    var cells = nb.get_cells();
    nb.select(cells.length - 1);
    nb.focus_cell();
})();"))

(define-command execute-selected-cells ()
  "Run the currently selected cell(s)."
  (rpc-buffer-evaluate-javascript
   (current-buffer)
   "
Jupyter.notebook.execute_selected_cells();"))

(define-command execute-all-cells ()
  "Run the entire notebook."
  (rpc-buffer-evaluate-javascript
   (current-buffer)
   "
Jupyter.notebook.execute_all_cells();"))

(define-command edit-cell ()
  "Open the selected cell's source in an emacs buffer for editing, using a
temporary file (whose location is currently hardcoded). The temporary files
extension is chosen based on the cell's type, with code cells given a `.py`
extension, markdown cells a `.md`, and all others `.txt`. Thus, emacs should
drop you into the appropriate mode when the buffer is created."
  (request-cell-data #'edit-cell-callback))

(define-command save-notebook ()
  "Save the currently active notebook."
  (rpc-buffer-evaluate-javascript
   (current-buffer)
   "
Jupyter.notebook.save_notebook();"))

(define-command save-checkpoint ()
  "Create a checkpoint (not sure; I don't manually do this so I'm not positive
what constitutes a checkpoint)."
  (rpc-buffer-evaluate-javascript
   (current-buffer)
   "
Jupyter.notebook.save_checkpoint();"))

(define-command copy-cell ()
  "Copy the selected cell(s)."
  (rpc-buffer-evaluate-javascript
   (current-buffer)
   "
Juptery.notebook.copy_cell();"))

(define-command scroll-some (ammt)
  "Scroll the page by ammt using the notebooks scroll_manager."
  (rpc-buffer-evaluate-javascript
   (current-buffer)
   (format nil "
Jupyter.notebook.scroll_manager.scroll_some(~f);" ammt)))

(define-mode jupyter-nb-mode ()
  "A mode for interacting with Jupyter notebooks, with facilities for editing
the notebook's contents using the emacsclient mechanism."
  ((keymap-schemes
   :initform
   (let ((vi-map (make-keymap)))
     (define-key :keymap vi-map
       "g g" #'select-first-cell
       "G" #'select-last-cell
       "C-c C-c" #'execute-selected-cells
       "C-c C-l" #'execute-all-cells
       "e" #'edit-cell
       "j" #'select-next-cell
       "k" #'select-prev-cell
       "C-e" (lambda () (scroll-some 0.25))
       "C-y" (lambda () (scroll-some -0.25)))
     (list :vi-normal vi-map)))))

(defun edit-str-with-emacs (str tempfile)
  "Dump the contents of str to the temporary file tempfile, then open tempfile
in Emacs for editing. Note that this call is synchronous!"
  ;; TODO: This is copy pasta w/ my init.lisp! (break off into a package?)
  ;; Dump the cell's contents to a tempfile
  (with-open-file (s tempfile :direction :output :if-exists :supersede)
    ;; Replace \n with literal newlines
    (format s "~a" (ppcre:regex-replace-all "\\\\n" str "
")))
  ;; Open an emacs buffer pointed at the file
  (let* ((shell-cmd `("emacsclient" ,tempfile)))
    (uiop:run-program shell-cmd :output :string))
  ;; Read the file's contents to the browser buffer's active input field.
  (with-open-file (s tempfile :direction :input)
    (let ((contents (make-string (file-length s))))
      (read-sequence contents s)
      ;; Escape backticks (since those are JS multiline string char).
      (ppcre:regex-replace-all "`" contents "\\\\`"))))

(defun edit-cell-callback (js-result)
  (let* ((json-result (cl-json:decode-json-from-string js-result))
	 (cell--type (rest (assoc :cell--type json-result)))
	 (source (rest (assoc :source json-result)))
	 (tempfile (format nil "/tmp/next-tmp.~a"
			   ;; Use the right file extension 
			   (cond ((string= cell--type "markdown") "md")
				 ((string= cell--type "code") "py")
				 (t ".txt"))))
	 (output nil))
    (setq output (edit-str-with-emacs source tempfile))
    (rpc-buffer-evaluate-javascript (current-buffer) (format nil "
(function () {
    var cell = Jupyter.notebook.get_selected_cell();
    cell.set_text(`~a`);
})();" output))))

(defun request-cell-data (callback)
  "Request the cell type, source, and metadata."
  (let ((cmd "
(function () {
    Jupyter.notebook.save_checkpoint();
    var cell = Jupyter.notebook.get_selected_cell();
    var retval = new Object();
    retval.cell_type = cell.cell_type;
    retval.source = cell.get_text();
    retval.metadata = cell.metadata;
    return JSON.stringify(retval);
})();
"))
    (rpc-buffer-evaluate-javascript (current-buffer) cmd :callback callback)))

(defun edit-cell-metadata-callback (js-result)
  ;; The metadata object is usually a nested JSON obj, which we re-encoded to a
  ;; JSON string for shipment to emacs.
  ;; TODO: Setup the decoder so that sub-level items are always strings.
  (let* ((json-result (cl-json:decode-json-from-string js-result))
	 (metadata (cl-json:encode-json-to-string
		    (rest (assoc :metadata json-result))))
	 (output (edit-str-with-emacs metadata "/tmp/next-tmp.json")))
    (rpc-buffer-evaluate-javascript (current-buffer)
				    (format nil "
(function () {
    var cell = Jupyter.notebook.get_selected_cell();
    cell.metadata = ~a;
})();" output))))

(define-command edit-cell-metadata ()
  "Open the selected cell's metadata in an emacs buffer for editing, using a
temporary file (whose location is currently hardcoded). The tempfile's extension
will be .json."
  (request-cell-data #'edit-cell-metadata-callback))
