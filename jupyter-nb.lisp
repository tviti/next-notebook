(uiop:define-package :next/jupyter-mode
    (:use :next :common-lisp)
    (:documentation "Mode for interacting with Jupyter notebooks."))
(in-package :next/jupyter-mode)

;; Cell movement commands
(define-command select-next-cell ()
  (rpc-buffer-evaluate-javascript
   (current-buffer)
   (format nil "
Jupyter.notebook.select_next(true);
Jupyter.notebook.focus_cell();")))

(define-command select-prev-cell ()
  (rpc-buffer-evaluate-javascript
   (current-buffer)
   (format nil "
Jupyter.notebook.select_prev(true);
Jupyter.notebook.focus_cell();")))

(define-mode jupyter-mode ()
  "Mode for interacting with Jupyter notebooks."
  ((keymap-schemes
   :initform
   (let ((vi-map (make-keymap)))
     (define-key :keymap vi-map
       "e" #'edit-cell
       "j" #'select-next-cell
       "k" #'select-prev-cell)
     (list :vi-normal vi-map)))))

(defun edit-str-with-emacs (str tempfile)
  "Dump the contents of str to the temporary file tempfile, then open tempfile
in Emacs for editing. Note that this call is synchronous!"
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
  ;; TODO: Communicate the cell as a JSON object instead of a custom array
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

(define-command edit-cell ()
  "Open the selected cell's source in an emacs buffer for editing."
  (request-cell-data #'edit-cell-callback))

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
  "Open the selected cell's metadata in an emacs buffer for editing."
  (request-cell-data #'edit-cell-metadata-callback))
