(uiop:define-package :next/jupyter-nb-mode
    (:use :next :common-lisp :ps)
    (:documentation "Mode for interacting with Jupyter notebooks."))
(in-package :next/jupyter-nb-mode)

;; TODO: I think a macro might be nice for all of these glue-calls to JS.
;; That or lean on parenscript for doing this.

(ps:defpsmacro %nb-chain (&rest args)
  `(ps:chain *jupyter notebook ,@args))

(ps:defpsmacro %get-nb ()
  '(%nb-chain))

;; TODO: These could be implemented as closures of a single function called
;; something like `select-cell-relative' (that does relative cell motion).
(define-command select-next-cell (&optional (buffer (current-buffer)))
  "Move one cell upwards, also moving the buffer's focus."
  (rpc-buffer-evaluate-javascript
   buffer
   (ps:ps (%nb-chain (select_next t))
	  (%nb-chain (focus_cell)))))

(define-command select-prev-cell (&optional (buffer (current-buffer)))
  "Move one cell downwards, also moving the buffer's focus."
  (rpc-buffer-evaluate-javascript
   buffer
   (ps:ps (%nb-chain (select_prev t))
	  (%nb-chain (focus_cell)))))

(define-command select-cell (idx &optional (buffer (current-buffer)))
  "Select cell idx, also moving the buffer's focus."
  (rpc-buffer-evaluate-javascript
   buffer
   (ps:ps (%nb-chain (select (ps:lisp idx) t))
	  (%nb-call-method focus_cell))))

(define-command select-first-cell (&optional (buffer (current-buffer)))
  (select-cell 0 buffer))

(define-command select-last-cell (&optional (buffer (current-buffer)))
  (rpc-buffer-evaluate-javascript
   buffer
   (ps:ps (let* ((n-cells (ps:@ (%nb-call-method get_cells) length)))
	    (%nb-chain (select (- n-cells 1)))
	    (%nb-chain (focus_cell))))))

(define-command execute-selected-cells (&optional (buffer (current-buffer)))
  "Run the currently selected cell(s)."
  (rpc-buffer-evaluate-javascript
   buffer
   (ps:ps (%nb-chain (execute_selected_cells)))))

(define-command execute-all-cells (&optional (buffer (current-buffer)))
  "Run the entire notebook."
  (rpc-buffer-evaluate-javascript
   buffer
   (ps:ps (%nb-chain (execute_all_cells)))))

(define-command edit-cell (&optional (buffer (current-buffer)))
  "Open the selected cell's source in an emacs buffer for editing, using a
temporary file (whose location is currently hardcoded). The temporary files
extension is chosen based on the cell's type, with code cells given a `.py`
extension, markdown cells a `.md`, and all others `.txt`. Thus, emacs should
drop you into the appropriate mode when the buffer is created."
  ;; TODO: Change this to `request-cell-contents'
  (request-cell-data (lambda (retval)
		       (edit-cell-callback retval buffer))))

(define-command save-notebook (&optional (buffer (current-buffer)))
  "Save the currently active notebook."
  (rpc-buffer-evaluate-javascript
   buffer
   (ps:ps (%nb-chain (save_notebook)))))

(define-command save-checkpoint (&optional (buffer (current-buffer)))
  "Create a checkpoint (not sure; I don't manually do this so I'm not positive
what constitutes a checkpoint)."
  (rpc-buffer-evaluate-javascript
   buffer
   (ps:ps (%nb-chain (save_checkpoint)))))

(define-command copy-cell (&optional (buffer (current-buffer)))
  "Copy the selected cell(s)."
  (rpc-buffer-evaluate-javascript
   buffer
   (ps:ps (%nb-chain (copy_cell)))))

(define-command scroll-some (ammt &optional (buffer (current-buffer)))
  "Scroll the page by ammt using the notebooks scroll_manager."
  (rpc-buffer-evaluate-javascript
   buffer
   (ps:ps (%nb-chain scroll_manager (scroll_some (ps:lisp ammt))))))

(define-command scroll (ammt &optional (buffer (current-buffer)))
  "Scroll the page by ammt using the notebooks scroll_manager."
  (rpc-buffer-evaluate-javascript
   buffer
   (ps:ps (%nb-chain scroll_manager (scroll (ps:lisp ammt))))))

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
       "C-e" (lambda () (scroll-some 0.1))
       "C-y" (lambda () (scroll-some -0.1)))
     (list :vi-normal vi-map)))))

;; TODO: This is copy pasta w/ my init.lisp! (break off into a package?)
(defun edit-str-with-emacs (str tempfile)
  "Dump the contents of str to the temporary file tempfile, then open tempfile
in Emacs for editing. Note that this call is synchronous!"
  ;; Dump the cell's contents to a tempfile
  (with-open-file (s tempfile :direction :output :if-exists :supersede)
    ;; Replace \n with literal newlines
    (format s "~a" str))
  ;; Open an emacs buffer pointed at the file
  (let* ((shell-cmd `("emacsclient" ,tempfile)))
    (uiop:run-program shell-cmd :output :string))
  ;; Read the file contents back in
  (with-open-file (s tempfile :direction :input)
    (let ((contents (make-string (file-length s))))
      (read-sequence contents s)
      contents)))

(defun edit-cell-callback (js-result buffer)
  (let* ((json-result (cl-json:decode-json-from-string js-result))
	 (cell--type (rest (assoc :cell--type json-result)))
	 (source (rest (assoc :source json-result)))
	 (tempfile (format nil "/tmp/next-tmp.~a"
			   ;; Use the right file extension 
			   (cond ((string= cell--type "markdown") "md")
				 ((string= cell--type "code") "py")
				 (t ".txt"))))
	 (output (edit-str-with-emacs source tempfile)))
    (rpc-buffer-evaluate-javascript
     buffer
     (ps:ps (%nb-chain (get_selected_cell) (set_text (ps:lisp output)))))))

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
