;;; init-build.el --- Build, Run, Debug System -*- lexical-binding: t -*-

(use-package cmake-mode
  :mode ("CMakeLists\\.txt\\'" "\\.cmake\\'")
  :config
  (setq cmake-tab-width 2))

(defun my/cmake-configure (&optional build-dir)
  "Configure CMake project."
  (interactive)
  (let ((build-dir (or build-dir "build")))
    (unless (file-exists-p "CMakeLists.txt")
      (error "No CMakeLists.txt found"))
    (unless (file-directory-p build-dir)
      (make-directory build-dir t))
    (async-shell-command (format "cd %s && cmake .." build-dir)
                         (format "*cmake-configure-%s*" build-dir))))

(defun my/cmake-build (&optional build-dir target)
  "Build CMake project."
  (interactive)
  (let* ((build-dir (or build-dir "build"))
         (target (or target "all")))
    (async-shell-command (format "cmake --build %s --target %s" build-dir target)
                         (format "*cmake-build-%s*" build-dir))))

(defun my/cmake-run (&optional build-dir target)
  "Run CMake target."
  (interactive)
  (let* ((build-dir (or build-dir "build"))
         (target (or target (read-string "Target: ")))
         (exe-path (concat build-dir "/" target)))
    (unless (file-exists-p exe-path)
      (error "Executable not found: %s" exe-path))
    (async-shell-command exe-path (format "*run-%s*" target))))

(defun my/gdb-run ()
  "Run GDB on current file or CMake target."
  (interactive)
  (cond
   ((string-suffix-p "CMakeLists.txt" (buffer-file-name))
    (let ((target (read-string "Target: "))
          (build-dir "build"))
      (gdb (format "gdb --annotate=3 %s/%s" build-dir target))))
   ((member major-mode '(c-ts-mode c++-ts-mode c-mode c++-mode))
    (let* ((file (buffer-file-name))
           (exe (read-string "Executable: " (file-name-base file))))
      (gdb (format "gdb --annotate=3 %s" exe))))
   (t
    (error "Not a C/C++ file"))))

(defconst my/run-commands
  '((c-ts-mode . my/run-c-cpp)
    (c++-ts-mode . my/run-c-cpp)
    (c-mode . my/run-c-cpp)
    (c++-mode . my/run-c-cpp)
    (python-ts-mode . my/run-python)
    (python-mode . my/run-python)
    (rust-ts-mode . my/run-rust)
    (rust-mode . my/run-rust))
  "Mapping of major modes to run functions.")

(defconst my/build-commands
  '((c-ts-mode . my/build-c-cpp)
    (c++-ts-mode . my/build-c-cpp)
    (c-mode . my/build-c-cpp)
    (c++-mode . my/build-c-cpp)
    (rust-ts-mode . my/build-rust)
    (rust-mode . my/build-rust))
  "Mapping of major modes to build functions.")

(defconst my/debug-commands
  '((c-ts-mode . my/gdb-run)
    (c++-ts-mode . my/gdb-run)
    (c-mode . my/gdb-run)
    (c++-mode . my/gdb-run)
    (rust-ts-mode . my/run-rust-lldb)
    (rust-mode . my/run-rust-lldb)
    (python-ts-mode . my/debug-python)
    (python-mode . my/debug-python))
  "Mapping of major modes to debug functions.")

(defun my/get-command (type)
  "Get command function for current buffer."
  (let ((cmd-alist (pcase type
                     ('run my/run-commands)
                     ('build my/build-commands)
                     ('debug my/debug-commands)
                     (_ nil))))
    (cdr (assq major-mode cmd-alist))))

(defun my/run-current-file ()
  "Run file based on language."
  (interactive)
  (let ((cmd (my/get-command 'run)))
    (if cmd
        (funcall cmd)
      (message "No run command defined for %s" major-mode))))

(defun my/build-current-file ()
  "Build file based on language."
  (interactive)
  (let ((cmd (my/get-command 'build)))
    (if cmd
        (funcall cmd)
      (message "No build command defined for %s" major-mode))))

(defun my/debug-current-file ()
  "Debug file based on language."
  (interactive)
  (let ((cmd (my/get-command 'debug)))
    (if cmd
        (funcall cmd)
      (message "No debug command defined for %s" major-mode))))

(defun my/test-current-file ()
  "Run tests based on language."
  (interactive)
  (cond
   ((file-exists-p "CMakeLists.txt")
    (my/cmake-build "build" "test")
    (my/cmake-run "build" "test"))
   ((file-exists-p "Cargo.toml")
    (async-shell-command "cargo test" "*cargo-test*"))
   ((member major-mode '(python-ts-mode python-mode))
    (async-shell-command "pytest" "*pytest*"))
   (t
    (message "No test command defined"))))

(defun my/run-c-cpp ()
  "Run C/C++ file (detect CMake)."
  (interactive)
  (cond
   ((file-exists-p "CMakeLists.txt")
    (my/cmake-run "build" (read-string "Target: ")))
   (t
    (let* ((file (buffer-file-name))
           (exe (read-string "Executable: " (file-name-base file))))
      (if (file-exists-p exe)
          (async-shell-command exe (format "*run-%s*" exe))
        (error "Executable not found: %s" exe))))))

(defun my/build-c-cpp ()
  "Build C/C++ file (detect CMake)."
  (interactive)
  (cond
   ((file-exists-p "CMakeLists.txt")
    (my/cmake-build "build" "all"))
   (t
    (let* ((file (buffer-file-name))
           (exe (file-name-base file)))
      (async-shell-command (format "gcc -o %s %s" exe file)
                           (format "*gcc-build-%s*" exe))))))

(defun my/run-python ()
  "Run Python file."
  (interactive)
  (async-shell-command (format "python %s" (buffer-file-name))
                       (format "*python-run-%s*" (buffer-name))))

(defun my/debug-python ()
  "Debug Python file with pdb."
  (interactive)
  (let ((file (buffer-file-name)))
    (async-shell-command (format "python -m pdb %s" file)
                         (format "*pdb-%s*" (buffer-name)))))

(defun my/run-rust ()
  "Run Rust file."
  (interactive)
  (cond
   ((file-exists-p "Cargo.toml")
    (async-shell-command "cargo run" "*cargo-run*"))
   (t
    (let ((file (buffer-file-name)))
      (async-shell-command (format "rustc %s && ./%s" file (file-name-base file))
                           (format "*rustc-run-%s*" (buffer-name)))))))

(defun my/build-rust ()
  "Build Rust file."
  (interactive)
  (cond
   ((file-exists-p "Cargo.toml")
    (async-shell-command "cargo build" "*cargo-build*"))
   (t
    (let ((file (buffer-file-name)))
      (async-shell-command (format "rustc %s" file)
                           (format "*rustc-build-%s*" (buffer-name)))))))

(defun my/run-rust-lldb ()
  "Debug Rust file with lldb."
  (interactive)
  (cond
   ((file-exists-p "Cargo.toml")
    (async-shell-command "lldb target/debug/$(basename $(pwd))" "*rust-lldb*"))
   (t
    (let ((file (buffer-file-name)))
      (async-shell-command (format "lldb %s" (file-name-base file))
                           (format "*rust-lldb-%s*" (buffer-name)))))))

(provide 'init-build)
