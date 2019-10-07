(uiop:define-package :next/jupyter-mode
    (:use :next :common-lisp)
    (:documentation "Mode for interacting with Jupyter notebooks."))
(in-package :next/jupyter-mode)

(define-mode jupyter-mode ()
  "Mode for interacting with Jupyter notebooks."
  ())

(defun edit-cell-callback (js_result &optional (tempfile "/tmp/next-tmp.py"))
  ;; Dump the field's contents to a tempfile
  (with-open-file (s tempfile :direction :output :if-exists :supersede)
    (format s "~a" js_result))
  ;; Open an emacs session pointed at the file
  (let* ((shell-cmd `("emacsclient" "--create-frame" ,tempfile)))
    (uiop:run-program shell-cmd :output :string))
  ;; Read the file's contents to the buffer's active input field.
  (with-open-file (s tempfile :direction :input)
    (let ((contents (make-string (file-length s)))
	  (output nil))
      (read-sequence contents s)
      ;; Escape backticks (since those are JS multiline string char).
      (setq output (ppcre:regex-replace-all "`" contents "\\\\`"))
      ;; ;; Strip weird chars added by gmail
      ;; (setq (remove #\Nul contents))
      (rpc-buffer-evaluate-javascript
       (current-buffer)
       (format nil "cell.set_text(`~a`);" output)))))

(define-command edit-cell ()
  "Open a new emacs frame using the `emacsclient' mechanism, and place the value
  of the currently selected input element in a temporary buffer. Upon exiting
  using the <C-x #> keybinding, the text will be placed in the next-buffer's
  active input element."
  (let ((cmd "var cell = Jupyter.notebook.get_selected_cell();
cell.get_text()"))
    (rpc-buffer-evaluate-javascript
     (current-buffer) cmd
     :callback #'edit-cell-callback)))
