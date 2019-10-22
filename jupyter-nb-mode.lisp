(uiop:define-package :next/jupyter-nb-mode
    (:use :next :common-lisp :ps)
  (:documentation "Mode for interacting with Jupyter notebooks."))
(in-package :next/jupyter-nb-mode)

(ps:defpsmacro %nb-chain (&rest args)
  "Like calling `ps:chain', where each result starts with Jupyter.notebook"
  `(ps:chain *jupyter notebook ,@args))

(ps:defpsmacro %get-nb ()
  '(%nb-chain))

(ps:defpsmacro %parse-json (str)
  `(ps:chain *json* (parse ,str)))

(defmacro define-ps-command (script-name args &body script-body)
  "This macro is a ripoff of `define-parenscript', modified to create a command
callable from the browser (wheras the `define-parenscript' just makes a function

Define parenscript command SCRIPT-NAME.  SCRIPT-BODY must be a valid parenscript
and will be wrapped in (PS:PS ...).  Any Lisp expression must be wrapped in
(PS:LISP ...).

The returned function is called over 3 key arguments beside ARGS: - %CALLBACK: a
function to call when the script returns.  Defaults to nil.  - %BUFFER: The
buffer used to execute the script.  Defaults to the current buffer.

Those variables can be used from the SCRIPT-BODY (the parenscript code).

ARGS must be key arguments."
  `(progn
     (define-command ,script-name ,(append '(&key ((:callback %callback) nil)
                                    ((:buffer %buffer) (current-buffer)))
					   args)
       ,(if (stringp (first script-body)) (first script-body))
       (rpc-buffer-evaluate-javascript %buffer
                                       (ps:ps ,@script-body)
                                       :callback %callback))))

;; TODO: These could be implemented as closures of a single function called
;; something like `select-cell-relative' (that does relative cell motion).
(define-ps-command select-next-cell ()
  "Move one cell upwards, also moving the buffer's focus."
   (%nb-chain (select_next t))
   (%nb-chain (focus_cell)))

(define-ps-command select-prev-cell ()
  "Move one cell downwards, also moving the buffer's focus."
   (%nb-chain (select_prev t))
   (%nb-chain (focus_cell)))

(define-ps-command select-cell ((idx))
  "Select cell idx, also moving the buffer's focus."
   (%nb-chain (select (ps:lisp idx) t))
   (%nb-chain (focus_cell)))

(define-command select-first-cell (&optional (buffer (current-buffer)))
  (select-cell :idx 0 :buffer buffer))

(define-ps-command select-last-cell ()
  (let* ((n-cells (ps:@ (%nb-chain (get_cells)) length)))
    (%nb-chain (select (- n-cells 1)))
    (%nb-chain (focus_cell))))

(define-command execute-selected-cells (&optional (buffer (current-buffer)))
  "Run the currently selected cell(s)."
  (rpc-buffer-evaluate-javascript
   buffer
   (ps:ps (%nb-chain (execute_selected_cells)))))

(define-ps-command execute-all-cells ()
  "Run the entire notebook."
  (%nb-chain (execute_all_cells)))

(define-ps-command open-below ()
  (%nb-chain (insert_cell_below))
  (%nb-chain (select_next))
  (%nb-chain (focus_cell)))

(define-ps-command open-above ()
  (%nb-chain (insert_cell_above))
  (%nb-chain (select_prev))
  (%nb-chain (focus_cell)))

(define-ps-command copy-cell ()
  (%nb-chain (copy_cell)))

(define-ps-command delete-cells ()
  (%nb-chain (delete_cells)))

(define-command edit-cell (&optional (buffer (current-buffer)))
  "Open the selected cell's source in an emacs buffer for editing, using a
temporary file (whose location is currently hardcoded). The temporary files
extension is chosen based on the cell's type, with code cells given a `.py`
extension, markdown cells a `.md`, and all others `.txt`. Thus, emacs should
drop you into the appropriate mode when the buffer is created."
  ;; TODO: Change this to `request-cell-contents'
  (request-cell-data (lambda (retval)
		       (edit-cell-callback retval buffer))))

(define-ps-command save-notebook ()
  "Save the currently active notebook."
  (%nb-chain (save_notebook)))

(define-ps-command save-checkpoint ()
  "Create a checkpoint (not sure; I don't manually do this so I'm not positive
what constitutes a checkpoint)."
  (%nb-chain (save_checkpoint)))

(define-ps-command copy-cell ()
  "Copy the selected cell(s)."
  (%nb-chain (copy_cell)))

(define-ps-command nb-scroll ((ammt))
  "Scroll the page by ammt using the notebook's scroll_manager."
   (%nb-chain scroll_manager (scroll (ps:lisp ammt))))

(define-command nb-scroll-page-up (&optional (buffer (current-buffer)))
  "Scroll the document upwards by one page."
  (nb-scroll :ammt -1))

(define-command nb-scroll-page-down (&optional (buffer (current-buffer)))
  "Scroll the document downwards by one page."
  (nb-scroll :ammt 1))

(define-parenscript %restart-kernel ()
  ;; Note: THIS WILL NOT PROMPT!
  (%nb-chain kernel (restart)))

(define-command restart-kernel ()
  "Restart the kernel for the active buffer."
  (with-result (y-n-p (read-from-minibuffer
		       (make-instance 'minibuffer
				      :input-prompt "Are you sure you want to restart the kernel? (yes or no):"
				      :completion-function
				      (lambda (x) '("no" "yes")))))
    (when (string= y-n-p "yes")
      (%restart-kernel))))

(define-parenscript %clear-all-output ()
  (%nb-chain (clear_all_output)))

(define-command clear-all-output ()
  "Restart the kernel for the active buffer, and clear all cell outputs."
  (with-result (y-n-p (read-from-minibuffer
		       (make-instance 'minibuffer
				      :input-prompt "Are you sure you want to clear all outputs? (yes or no):"
				      :completion-function
				      (lambda (x) '("no" "yes")))))
    (when (string= y-n-p "yes")
      (%clear-all-output))))

(define-command restart-clear-all-output ()
  "Restart the kernel for the active buffer, and clear all cell outputs."
  (with-result (y-n-p (read-from-minibuffer
		       (make-instance 'minibuffer
				      :input-prompt "Are you sure you want to restart the kernel and clear all outputs? (yes or no):"
				      :completion-function
				      (lambda (x) '("no" "yes")))))
    (when (string= y-n-p "yes")
      (%restart-kernel)
      (%clear-all-output))))

(define-parenscript %shutdown-kernel ()
  (%nb-chain kernel (kill)))

(define-command shutdown-kernel ()
  "Shutdown the kernel for the active buffer."
  (with-result (y-n-p (read-from-minibuffer
		       (make-instance 'minibuffer
				      :input-prompt "Are you sure you want to shutdown the kernel? (yes or no):"
				      :completion-function
				      (lambda (x) '("no" "yes")))))
    (when (string= y-n-p "yes")
      (%shutdown-kernel))))

(define-ps-command toggle-cell-output ()
  "Toggle output folding in a selected code cell."
  (let ((cell (%nb-chain (get_selected_cell))))
    (when (string= (ps:chain cell cell_type) (ps:lisp "code"))
      (ps:chain cell (toggle_output)))))

(define-command goto-parent-dir-new-buffer ()
  "Navigate to the active buffer's parent dir, in the NB web interface."
  (let* ((buffer (make-buffer))
	 (url (url (current-buffer))))
    (set-current-buffer buffer)
    (cl-ppcre:register-groups-bind (parent-url fn)
	("(.*\\/)(.*?\\.ipynb)"  url)
      (set-url parent-url :buffer buffer))))

(define-mode jupyter-nb-mode ()
  "A mode for interacting with Jupyter notebooks, with facilities for editing
the notebook's contents using the emacsclient mechanism."
  ((keymap-schemes
   :initform
   (let ((vi-map (make-keymap))
	 (emacs-map (make-keymap))
	 (scroll-ammt 0.25))

     (define-key :keymap vi-map
       "TAB" #'toggle-cell-output
       "g g" #'select-first-cell
       "G" #'select-last-cell
       "C-c C-c" #'execute-selected-cells
       "C-c C-l" #'execute-all-cells
       "C-c '" #'edit-cell
       "C-c \"" #'edit-cell-metadata
       "o" #'open-below  ;; NOTE: These will override default bindings!
       "O" #'open-above  ;; NOTE: These will override default bindings!
       "d" #'delete-cells
       "j" #'select-next-cell
       "k" #'select-prev-cell
       "C-e" (lambda () (nb-scroll :ammt scroll-ammt))
       "C-y" (lambda () (nb-scroll :ammt (* -1 scroll-ammt)))
       "C-f" #'nb-scroll-page-down
       "C-b" #'nb-scroll-page-up
       "C-x B" #'switch-to-nb-buffer ;; FIXME: undef. warning
       "C-x C-s" #'save-checkpoint
       "^" #'goto-parent-dir-new-buffer)

     (define-key :keymap emacs-map
       "TAB" #'toggle-cell-output
       "C-n" #'select-next-cell
       "C-p" #'select-prev-cell
       "C-c C-c" #'execute-selected-cells
       "C-c C-l" #'execute-all-cells
       "SPACE" #'nb-scroll-page-down
       "s-SPACE" #'nb-scroll-page-up
       "C-x C-s" #'save-checkpoint)

       (list :vi-normal vi-map
	     :emacs emacs-map)))))

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

(defun request-cell-data (callback &optional (buffer (current-buffer)))
  "Request the cell type, source, and metadata. The result is packed to JSON."
  (rpc-buffer-evaluate-javascript
   buffer
   (ps:ps (let ((retval (ps:new *Object))
		(cell (%nb-chain (get_selected_cell))))
	    (%nb-chain (save_checkpoint))
	    (ps:setf (ps:@ retval cell-type) (ps:chain cell cell_type))
	    (ps:setf (ps:@ retval cell-source) (ps:chain cell (get_text)))
	    (ps:setf (ps:@ retval metadata) (ps:@ cell metadata))
	    (ps:chain *json* (stringify retval))))
   :callback callback))

(defun edit-cell-callback (js-result buffer)
  (let* ((json-result (cl-json:decode-json-from-string js-result))
	 (cell-type (rest (assoc :cell-type json-result)))
	 (cell-source (rest (assoc :cell-source json-result)))
	 (tempfile (format nil "/tmp/next-tmp.~a"
			   ;; Use the right file extension 
			   (cond ((string= cell-type "markdown") "md")
				 ((string= cell-type "code") "py")
				 (t ".txt"))))
	 (output (edit-str-with-emacs cell-source tempfile)))
    (rpc-buffer-evaluate-javascript
     buffer
     (ps:ps (%nb-chain (get_selected_cell)
		       (set_text (ps:lisp output)))))))

(defun edit-cell-metadata-callback (js-result buffer)
  ;; The metadata object is usually a nested JSON obj, which we re-encoded to a
  ;; JSON string for shipment to emacs.
  ;; TODO: Setup the decoder so that sub-level items are always strings.
  (let* ((json-result (cl-json:decode-json-from-string js-result))
	 (metadata-str (cl-json:encode-json-to-string
			(rest (assoc :metadata json-result))))
	 (output-str (edit-str-with-emacs metadata-str "/tmp/next-tmp.json")))
    (rpc-buffer-evaluate-javascript
     buffer
     (ps:ps (let ((new-metadata (%parse-json (ps:lisp output-str))))
	      (setf (%nb-chain (get_selected_cell) metadata) new-metadata))))))

(define-command edit-cell-metadata ()
  "Open the selected cell's metadata in an emacs buffer for editing, using a
temporary file (whose location is currently hardcoded). The tempfile's extension
will be .json."
  (request-cell-data (lambda (js-result)
		       (edit-cell-metadata-callback
			js-result (current-buffer)))))

(in-package :next)
(define-command switch-to-nb-buffer ()
  "Query for and switch to a jupyter-nb-mode buffer."
  (let* ((buffers (alexandria:hash-table-values (buffers *interface*)))
	 (blist))
    (mapcar (lambda (b)
	      (when (find-mode b 'jupyter-nb-mode)
		(push b blist))) buffers)
    (with-result (buffer (read-from-minibuffer
			  (make-instance 'minibuffer
					 :input-prompt "Switch to buffer:"
					 :completion-function (lambda (input)
								(fuzzy-match input blist)))))
      (set-current-buffer buffer))))
