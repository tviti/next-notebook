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
    (let ((contents (make-string (file-length s)))
	  (output nil))
      (read-sequence contents s)
      contents)))

(defun edit-cell-callback (js-result)
  ;; TODO: Communicate the cell as a JSON object instead of a custom array
  (ppcre:register-groups-bind (cell-type cell-contents)
      ("\\['(.*?)',.*?['\"](.*?)['\"]\\]" js-result)
    (let ((tempfile (format nil "/tmp/next-tmp.~a"
			    ;; Use the right file extension 
			    (cond ((string= cell-type "markdown") "md")
				  ((string= cell-type "code") "py")
				  (t ".txt"))))
	  (contents nil))
      (setq contents (edit-str-with-emacs cell-contents tempfile))
      ;; Escape backticks (since those are JS multiline string char).
      (setq output (ppcre:regex-replace-all "`" contents "\\\\`"))
      (rpc-buffer-evaluate-javascript
       (current-buffer)
       (format nil "
(function () {
    var cell = Jupyter.notebook.get_selected_cell();
    cell.set_text(`~a`);
})();" output)))))

(define-command edit-cell ()
  "Open a new emacs frame using the `emacsclient' mechanism, and place the value
  of the currently selected input element in a temporary buffer. Upon exiting
  using the <C-x #> keybinding, the text will be placed in the next-buffer's
  active input element."
  (let ((cmd "
(function () {
    Jupyter.notebook.save_checkpoint();
    var cell = Jupyter.notebook.get_selected_cell();
    return [cell.cell_type, cell.get_text()];
})();
"))
    (rpc-buffer-evaluate-javascript
     (current-buffer) cmd
     :callback #'edit-cell-callback)))

(define-command edit-cell-metadata ()
  ""
  (let ((cmd "
(function () {
    Jupyter.notebook.save_checkpoint();
    var cell = Jupyter.notebook.get_selected_cell();
    return cell.metadata;
})();
"))
    (rpc-buffer-evaluate-javascript
     (current-buffer) cmd
     :callback #'edit-cell-callback)))
