;;; tabnine.el --- minor mode for TabNine AI coding assitant
;;
;; Copyright (c) 2023 Steven Taylor
;;
;;; Commentary:
;;

;;
;; Dependencies
;;

(require 'cl-lib)
(require 'dash)
(require 'json)
(require 's)
(require 'url)
(require 'projectile nil 't)

;;
;; Constants
;;

(defconst tabnine--buffer-name "tabnine-log")
(defconst tabnine--version-tempfile "version")
(defconst tabnine--process-name "tabnine--process")
(defconst tabnine--hooks-alist nil)
(defconst tabnine--trace-message-buffer-name "*TabNineTraceMessages*")
(defconst tabnine--user-message-buffer-name "*TabNineUserMessages*")

;;
;; Customisation
;;

(defgroup tabnine nil
  "Tabnine."
  :group 'completion
  :prefix "tabnine-")

(defface tabnine-overlay-face
  '((t  :inherit shadow))
  "Face for tabnine overlay."
  :group 'tabnine)

(defcustom tabnine-wait 2.50
  "Number of seconds to wait for TabNine to respond."
  :group 'tabnine
  :type 'float)

(defcustom tabnine-idle-timeout 0.50
  "Number of seconds to wait for TabNine to respond."
  :group 'tabnine
  :type 'float)

(defcustom tabnine-context-radius 2500
  "The number of chars before/after point to send for completion.
Note that setting this too small will cause TabNine
to not be able to read the entire license activation key."
  :group 'tabnine
  :type 'integer)

(defcustom tabnine-max-num-results 10
  "Maximum number of results to show."
  :group 'tabnine
  :type 'integer)

(defcustom tabnine-clear-overlay-ignore-commands nil
  "List of commands that should not clear the overlay when called."
  :group 'tabnine
  :type '(repeat function))

(defcustom tabnine-enable-predicates '(evil-insert-state-p
                                       tabnine--buffer-changed)
  "A list of predicate functions with no argument to enable Tabnine.
Tabnine will be triggered only if all predicates return t."
  :type '(repeat function)
  :group 'tabnine)

(defcustom tabnine-binaries-directory "~/.TabNine"
  "Path to TabNine binaries folder.
`tabnine-install-binary' will use this directory."
  :group 'tabnine
  :type 'string)

(defcustom tabnine-log-file-path nil
  "If non-nil, next TabNine restart will write debug log to this path."
  :group 'tabnine
  :type 'string)

(defcustom tabnine-log-level "Warn"
  "If tabnine-log-file-path is set then this log level is used.
Valid values Trace, Debug, Info, Warn, Error."
  :group 'tabnine
  :type 'string)

(defcustom tabnine-max-restart-count 10
  "Maximum number of times TabNine can consecutively restart.
This may be due to errors in or automatic server updates.
Any successful completion will reset the consecutive count."
  :group 'tabnine
  :type 'integer)

(defcustom tabnine-log-messages 't
  "If non-nil then log input output messages to buffer."
  :group 'tabnine
  :type 'boolean)

(defcustom tabnine-wait 10.00
  "Number of seconds to wait for TabNine to respond."
  :group 'tabnine
  :type 'float)

(defcustom tabnine-disable-predicates nil
  "A list of predicate functions with no argument to disable Tabnine.
Tabnine will not be triggered if any predicate returns t."
  :type '(repeat function)
  :group 'tabnine)

;;
;;; Variables:
;;

(defvar tabnine--correlation-id 1
  "Correlation id from requests to tabnine.")

(defvar tabnine--response nil
  "Temporarily stored TabNine server responses.")

(defvar tabnine--post-command-timer nil)

(defvar tabnine-mode-map (make-sparse-keymap)
  "Keymap used by `tabnine-mode'.")

(defvar tabnine-active-map
  (let ((keymap (make-sparse-keymap)))
    (define-key keymap "\C-g" #'tabnine-abort)
    (define-key keymap (kbd "<tab>") #'tabnine-next-completion)
    (define-key keymap (kbd "C-<tab>") #'tabnine-accept-completion)
    keymap)
  "Keymap for Tabnine completion overlay.")

(defvar-local tabnine--line-bias 1
  "Line bias for Tabnine completion.")

(defvar-local tabnine--overlay nil
  "Overlay for tabnine completion.")

(defvar-local tabnine--real-posn nil
  "Posn information without overlay.
To work around posn problems with after-string property.")

(defvar-local tabnine--response-idx 0
  "Position in list of currenly displayed item.")

(defvar-local tabnine--current-result nil
  "Currently selected tabnine response.")

(defvar-local tabnine--last-buffer-version 0
  "The document version of the last completion.")

(defvar-local tabnine--buffer-version 0
  "The document version of the current buffer. Incremented after each change.")

(defvar tabnine--protocol-version "4.0.0"
  "The string to store the protocol version derived from the current installed.")

(defvar tabnine--process nil
  "TabNine server process.")

(defvar tabnine-executable-args nil
  "Arguments passed to TabNine.")

(defvar tabnine--restart-count 0
  "Number of times TabNine server has restarted abnormally.
Resets every time successful completion is returned.")

(defvar-local tabnine--response-chunks nil
  "The string to store response chunks from TabNine server.")

(defvar-local tabnine--response nil
  "Temporarily stored TabNine server responses.")

(defvar tabnine-process-status nil
  "Current status of the TabNine process (alist).")

(defvar tabnine-update-timer nil
  "Timer to update status and workspace roots.")
;;
;;; Code:
;;

(defmacro tabnine--satisfy-predicates (enable disable)
  "Return t if satisfy all predicates in ENABLE and none in DISABLE."
  `(and (cl-every (lambda (pred)
                    (if (functionp pred) (funcall pred) t))
                  ,enable)
        (cl-notany (lambda (pred)
                     (if (functionp pred) (funcall pred) nil))
                   ,disable)))

(defun tabnine--satisfy-trigger-predicates ()
  "Return t if all trigger predicates are satisfied."
  (tabnine--satisfy-predicates
   tabnine-enable-predicates
   tabnine-disable-predicates))

(defun tabnine--buffer-changed ()
  "Return non-nil if the buffer has changed since last completion."
  (not (= tabnine--last-buffer-version tabnine--buffer-version)))

(defsubst tabnine--overlay-visible ()
  "Return whether the `tabnine--overlay' is avaiable."
  (and (overlayp tabnine--overlay)
       (overlay-buffer tabnine--overlay)))

(defun tabnine--get-overlay ()
  "Create or get overlay for Copilot."
  (unless (overlayp tabnine--overlay)
    (setq tabnine--overlay (make-overlay 1 1))
    (overlay-put tabnine--overlay 'keymap tabnine-active-map)
    (overlay-put tabnine--overlay 'priority 100))
  tabnine--overlay)

(defun tabnine--set-overlay-text (ov)
  "Set overlay OV with COMPLETION."
  (let ((tn-current-results (cdr (assoc 'results tabnine--response))))
    (when (> (length tn-current-results) 0)
      (progn
        (setq tabnine--current-result (elt
                                       tn-current-results
                                       tabnine--response-idx))
        (let ((old-prefix-lenght (length (cdr (assoc 'old_prefix tabnine--response))))
              (p-completion-prefix
               (propertize
                (cdr (assoc 'new_prefix tabnine--current-result))
                'face
                'tabnine-overlay-face))
              (p-completion-suffix
               (propertize
                (cdr (assoc 'new_suffix tabnine--current-result))
                'face
                'tabnine-overlay-face)))
          (if (eolp)
              (move-overlay ov (- (point) old-prefix-lenght) (+ (point) 1))
            (move-overlay ov (- (point) old-prefix-lenght) (+ (line-end-position) 1) ))
          (overlay-put ov 'display p-completion-prefix)
          (overlay-put ov 'after-string (concat p-completion-suffix "\n\n"))
          (overlay-put ov 'completion-idx  tabnine--response-idx)
          (overlay-put ov 'start (- (point) old-prefix-lenght)))))))

(defun tabnine--display-overlay-completion ()
  "Show COMPLETION at USER-POS."
  (tabnine-clear-overlay)
  ;; (message tabnine--response)
  (save-restriction
    (widen)
    (save-excursion
      (let* ((ov (tabnine--get-overlay)))
        (tabnine--set-overlay-text ov)))))

(defun tabnine-clear-overlay ()
  "Clear overlay."
  (interactive)
  (when (tabnine--overlay-visible)
    (delete-overlay tabnine--overlay)
    (setq tabnine--real-posn nil)))

(defun tabnine-complete ()
  "Complete at the current point."
  (interactive)
  (setq tabnine--response-idx 0)
  (let ((request (tabnine--make-completion-request)))
    (setq tabnine--response (tabnine-send-request request)))
  (tabnine--display-overlay-completion))

(defun tabnine-next-completion ()
  "Load next completion."
  (interactive)
  (message "Next completion")
  (let ((tmp-results (cdr (assoc 'results tabnine--response))) )
    (when (tabnine--overlay-visible)
      (setq tabnine--response-idx (+ 1 tabnine--response-idx))
      (if (>= tabnine--response-idx (length tmp-results))
          (setq tabnine--response-idx 0))
      (tabnine--set-overlay-text (tabnine--get-overlay)))))

(defun tabnine-accept-completion ()
  "Accept the current completion."
  (interactive)
  (let* ((old-prefix-length (length (cdr (assoc 'old_prefix tabnine--response))))
         (new-comp-prefix (cdr (assoc 'new_prefix tabnine--current-result)))
         (new-comp-suffix (cdr (assoc 'new_suffix tabnine--current-result)))
         (old-comp-suffix (cdr (assoc 'old_suffix tabnine--current-result)))
         (new-comp-suffix-length (length new-comp-suffix))
         (old-comp-suffix-lenght (length old-comp-suffix)))
    (if (> old-comp-suffix-lenght 0)
        (delete-char old-comp-suffix-lenght))
    (if (> old-prefix-length 0)
        (delete-char (* old-prefix-length -1)))

    (if (> (length new-comp-suffix) 0)
        (progn
          (insert (concat new-comp-prefix new-comp-suffix))
          (backward-char new-comp-suffix-length))
      (progn
        (insert (concat new-comp-prefix old-comp-suffix))
        (backward-char old-comp-suffix-lenght))))
  (setq tabnine--last-buffer-version tabnine--buffer-version)
  (setq tabnine--current-result nil)
  (tabnine-clear-overlay)
  (setq tabnine--response-idx 0))

(defun tabnine-abort ()
  "Abort current completion."
  (interactive)
  (tabnine-clear-overlay)
  (when tabnine--post-command-timer
    (cancel-timer tabnine--post-command-timer))
  (setq tabnine--last-buffer-version tabnine--buffer-version)
  (setq tabnine--response-idx 0))

(defun tabnine--make-completion-request()
  "Tabnine autocompletion request."
  (let* ((buffer-min 1)
         (buffer-max (1+ (buffer-size)))
         (before-point
          (max (point-min) (- (point) tabnine-context-radius)))
         (after-point
          (min (point-max) (+ (point) tabnine-context-radius))))
    (setq tabnine--correlation-id (+ tabnine--correlation-id 1))
    (list :Autocomplete
          (list
           :before (buffer-substring-no-properties before-point (point))
           :after (buffer-substring-no-properties (point) after-point)
           :filename (or (buffer-file-name) nil)
           :region_includes_beginning (if (= before-point buffer-min)
                                          t json-false)
           :region_includes_end (if (= after-point buffer-max)
                                    t json-false)
           :max_num_results tabnine-max-num-results
           :correlation_id tabnine--correlation-id))))

(defun tabnine--make-prefetch-request()
  "Tabnine prefetch request."
  (list :Prefetch
        (list
         :filename (or (buffer-file-name) nil))))

(defun tabnine--make-getidentifierregex-request()
  "Tabnine getidentifierregex request."
  (list :GetIdentifierRegex
        (list
         :filename (or (buffer-file-name) nil))))

(defun tabnine--prefetch()
  "Prefetch file on activation of tabnine-minor-mode."
  (let ((fname (buffer-file-name)))
    (if fname
        (let ((request (list :Prefetch (list :filename fname))))
          (tabnine-send-request request)))))

;;; tabnine binary management

(defun tabnine--error-no-binaries ()
  "Signal error for when TabNine binary is not found."
  (error "No TabNine binaries found.  Run M-x tabnine-install-binary to download binaries"))

(defun tabnine--get-exe ()
  "Return TabNine's binary file name.  Used for finding the correct binary."
  "TabNine")

(defun tabnine--get-target ()
  "Return TabNine's system configuration.  Used for finding the correct binary."
  (let* ((arch (car (s-split "-" system-configuration))))
    (concat arch "-unknown-linux-gnu")))

(defun tabnine-install-binary ()
  "Install TabNine binary into `tabnine-binaries-directory'."
  (interactive)
  (let ((version-tempfile (concat
                           (file-name-as-directory
                            tabnine-binaries-directory)
                           tabnine--version-tempfile))
        (target (tabnine--get-target))
        (exe (tabnine--get-exe))
        (binaries-dir tabnine-binaries-directory))
    (message version-tempfile)
    (message "Getting current version...")
    (make-directory (file-name-directory version-tempfile) t)
    (url-copy-file "https://update.tabnine.com/bundles/version" version-tempfile t)
    (let ((version (s-trim (with-temp-buffer (insert-file-contents version-tempfile) (buffer-string)))))
      (when (= (length version) 0)
        (error "TabNine installation failed.  Please try again"))
      (message "Current version is %s" version)
      (let* ((url (concat "https://update.tabnine.com/bundles/" version "/" target "/TabNine.zip"))
             (version-directory (file-name-as-directory
                                 (concat
                                  (file-name-as-directory
                                   (concat (file-name-as-directory binaries-dir) version)))))
             (target-directory (file-name-as-directory (concat version-directory target) ))
             (bundle-path (concat version-directory (format "%s.zip" target)))
             (target-path (concat target-directory exe)))
        (message "Installing at %s. Downloading %s ..." target-path url)
        (make-directory target-directory t)
        (url-copy-file url bundle-path t)
        (condition-case ex
            (let ((default-directory target-directory))
              (if (or (eq system-type 'ms-dos)
                      (eq system-type 'windows-nt)
                      (eq system-type 'cygwin))
                  (shell-command (format "tar -xf %s" (expand-file-name bundle-path)))
                (shell-command (format "unzip -o %s -d %s"
                                       (expand-file-name bundle-path)
                                       (expand-file-name target-directory)))))
          ('error
           (error "Unable to unzip automatically. Please go to [%s] and unzip the content of [%s] into [%s/]"
                  (expand-file-name version-directory)
                  (file-name-nondirectory bundle-path)
                  (file-name-sans-extension (file-name-nondirectory bundle-path)))))
        (mapc (lambda (filename)
                (set-file-modes (concat target-directory filename) (string-to-number "744" 8)))
              (--remove (member it '("." "..")) (directory-files target-directory)))
        (delete-file bundle-path)
        (delete-file version-tempfile)
        (message "TabNine installation complete.")))))

(defun tabnine--executable-path ()
  "Find and return the path of the latest TabNine binary for the current system."
  (let ((parent tabnine-binaries-directory))
    (if (file-directory-p parent)
        (let* ((children (->> (directory-files parent)
                              (--remove (member it '("." "..")))
                              (--filter (file-directory-p
                                         (expand-file-name
                                          it
                                          (file-name-as-directory
                                           parent))))
                              (--filter (ignore-errors (version-to-list it)))
                              (-non-nil)))
               (sorted (nreverse (sort children #'version<)))
               (target (tabnine--get-target))
               (filename (tabnine--get-exe)))
          (setq tabnine--protocol-version (car sorted))
          (cl-loop
           for ver in sorted
           for fullpath = (expand-file-name (format "%s/%s/%s"
                                                    ver target filename)
                                            parent)
           if (and (file-exists-p fullpath)
                   (file-regular-p fullpath))
           return fullpath
           finally do (tabnine--error-no-binaries)))
      (tabnine--error-no-binaries))))

(defun tabnine-start-process ()
  "Start TabNine process."
  (tabnine-kill-process)
  (let ((process-connection-type nil))
    (setq tabnine--process
          (make-process
           :name tabnine--process-name
           :command (append
                     (cons (tabnine--executable-path)
                           (when tabnine-log-file-path
                             (list
                              "--log-file-path"
                              (expand-file-name tabnine-log-file-path)
                              "--log-level"
                              tabnine-log-level
                              ))
                           )
                     (list "--client" "emacs")
                     tabnine-executable-args)
           :coding 'utf-8
           :connection-type 'pipe
           :filter #'tabnine--process-filter
           :sentinel #'tabnine--process-sentinel
           :noquery t)))
  (message "TabNine server started.")
  ;; start status timer
  (setq tabnine-update-timer (run-with-idle-timer 5 't 'tabnine--update-process))
  ;; hook setup
  (dolist (hook tabnine--hooks-alist)
    (add-hook (car hook) (cdr hook))))

(defun tabnine-kill-process ()
  "Kill TabNine process."
  (interactive)
  (when tabnine--process
    (let ((process tabnine--process))
      (setq tabnine--process nil) ; this happens first so sentinel don't catch the kill
      (delete-process process)))
  (when tabnine-update-timer
    (cancel-timer tabnine-update-timer)
    (setq tabnine-update-timer nil))
  ;; hook remove
  (dolist (hook tabnine--hooks-alist)
    (remove-hook (car hook) (cdr hook))))

(defun tabnine--process-sentinel (process event)
  "Sentinel for TabNine server process.
PROCESS is the process under watch, EVENT is the event occurred."
  (when (and tabnine--process
             (memq (process-status process) '(exit signal)))

    (message "TabNine process %s received event %s."
             (prin1-to-string process)
             (prin1-to-string event))

    (if (>= tabnine--restart-count
            tabnine-max-restart-count)
        (progn
          (message "TabNine process restart limit reached.")
          (setq tabnine--process nil))

      (message "Restarting TabNine process.")
      (tabnine-start-process)
      (setq tabnine--restart-count
            (1+ tabnine--restart-count)))))

(defun tabnine--process-filter (process output)
  "Filter for TabNine server process.
PROCESS is the process under watch, OUTPUT is the output received."
  (unless (or (string-prefix-p "null" output) (= (length output) 0))
    (push output tabnine--response-chunks)
    (when (s-ends-with-p "\n" output)
      (let ((response
             (mapconcat #'identity
                        (nreverse tabnine--response-chunks)
                        nil)))
        (setq tabnine--response (tabnine--decode response))
        (setq tabnine--response-chunks nil)))))

(defun tabnine--decode (msg)
  "Decode TabNine server response MSG, and return the decoded object."
  (when tabnine-log-messages
    (with-current-buffer (get-buffer-create tabnine--trace-message-buffer-name)
      (goto-char (point-max))
      (insert msg)))
  ;; (unless (string-prefix-p "null" msg)
  ;; (message (concat msg " not null"))
  (ignore-errors
    (json-parse-string msg :object-type 'alist)))

(defun tabnine-restart-server ()
  "Start/Restart TabNine server."
  (interactive)
  (tabnine-start-process))

(defun tabnine--update-process ()
  "Get tabnine status and update project roots."
  (let ((request (list :State (list :dummy t))))
    (setq tabnine-process-status (tabnine-send-request request)))

  (let* ((projects (projectile-open-projects))
         (request (list :Workspace (list :root_paths (vconcat projects)))))
    ;;)
    (tabnine-send-request request))
  )

(defun tabnine-send-request (request)
  "Send REQUEST to tabnine and retrun response."
  (interactive)
  (unless tabnine--process
    (tabnine-start-process))

  (let ((full_request (list
                       :version tabnine--protocol-version
                       :request request)))
    (tabnine--send-request full_request))
  ;; (print tabnine-bin--response (get-buffer-create tabnine-bin--user-message-buffer-name))
  (let* ((user-message (cdr (assoc 'user_message tabnine--response)))
         (message-count (length user-message))
         (counter 0))
    (when (> message-count 0)
      (with-current-buffer (get-buffer-create tabnine--user-message-buffer-name)
        (goto-char (point-max))
        (while (< counter message-count)
          (insert (format "%s\n" (elt user-message counter)))
          (setq counter (+ counter 1))))))
  tabnine--response)

(defun tabnine--send-request (request)
  "Send REQUEST to tabnine."
  (when (null tabnine--process)
    (tabnine-start-process))
  (when tabnine--process
    (let ((encoded (concat
                    (json-serialize request
                                    :null-object nil
                                    :false-object json-false)
                    "\n")))
      (when tabnine-log-messages
        (with-current-buffer (get-buffer-create tabnine--trace-message-buffer-name)
          (goto-char (point-max))
          (insert encoded)))
      (setq tabnine--response nil)
      (process-send-string tabnine--process encoded)
      (accept-process-output tabnine--process tabnine-wait))))

;;; end tabnine binary management

;;; minor mode stuff

;;;###autoload
(define-minor-mode tabnine-mode
  "Minor mode for Tabnine."
  :init-value nil
  :lighter " Tabnine"
  (tabnine-clear-overlay)
  (tabnine--prefetch)
  (if tabnine-mode
      (progn
        (add-hook 'post-command-hook #'tabnine--post-command nil 'local)
        (add-hook 'before-change-functions #'tabnine--on-change nil 'local))
    (progn
      (remove-hook 'post-command-hook #'tabnine--post-command 'local)
      (remove-hook 'before-change-functions #'tabnine--on-change 'local))))

;;;###autoload
(define-globalized-minor-mode global-tabnine-mode
  tabnine-mode tabnine-mode)

(defun tabnine--on-change (&rest _args)
  "Handle `before-change-functions' hook."
  (cl-incf tabnine--buffer-version))

(defun tabnine--post-command ()
  "Complete in `post-command-hook' hook."
  (when (and this-command
             (not (and (symbolp this-command)
                       (or
                        (s-starts-with-p "tabnine-" (symbol-name this-command))
                        (member this-command tabnine-clear-overlay-ignore-commands)
                        (tabnine--self-insert this-command)))))
    (tabnine-clear-overlay)
    (when tabnine--post-command-timer
      (cancel-timer tabnine--post-command-timer))
    (when (tabnine--buffer-changed)
      (setq tabnine--post-command-timer
            (run-with-idle-timer tabnine-idle-timeout
                                 nil
                                 #'tabnine--post-command-debounce
                                 (current-buffer))))
    ))

(defun tabnine--self-insert (command)
  "Handle the case where the char just inserted is the start of the completion.
If so, update the overlays and continue. COMMAND is the
command that triggered `post-command-hook'."
  (when (and (eq command 'self-insert-command)
             (tabnine--overlay-visible))
    ;; (tabnine--satisfy-display-predicates))
    (let* ((ov tabnine--overlay)
           (completion (overlay-get ov 'completion)))
      ;; The char just inserted is the next char of completion
      (when (eq last-command-event (elt completion 0))
        (if (= (length completion) 1)
            ;; If there is only one char in the completion, accept it
            (tabnine-accept-completion)
          (tabnine--set-overlay-text ov))))))

(defun tabnine--post-command-debounce (buffer)
  "Complete in BUFFER."
  (when (and (buffer-live-p buffer)
             (equal (current-buffer) buffer)
             tabnine-mode
             (tabnine--satisfy-trigger-predicates))
    (tabnine-complete)))

(defun tabnine-who-am-i()
  "Print user name if known to messages buffer."
  (interactive)
  (if tabnine-process-status
      (message (cdr (assoc 'user_name tabnine-process-status)))
    (message "Tabnine process status not known")))

(defun tabnine-config-hub()
  "Launch TabNine config hub."
  (interactive)
  (let ((request (list :Configuration (list :quiet nil))))
    (tabnine-send-request request)))

(defun tabnine-config-hub-url()
  "Launch TabNine config hub and print url to messages."
  (interactive)
  (let* ((request (list :Configuration (list :quiet 't)))
         (response (tabnine-send-request request)))
    (message (cdr (assoc 'message response)))))

;;; end minor mode stuff

(provide 'tabnine)
;;; tabnine.el ends here
