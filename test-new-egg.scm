(module test-new-egg ()

(import chicken scheme)
(use data-structures extras files posix setup-api utils)
(use salmonella-log-parser)

(define (usage #!optional exit-code)
  (let ((port (if (and exit-code (not (zero? exit-code)))
                  (current-error-port)
                  (current-output-port)))
        (this (pathname-strip-directory (program-name))))
    (fprintf port "Usage: ~a <.release-info URI>\n" this)
    (when exit-code
      (exit exit-code))))

(define (die! fmt . args)
  (apply fprintf (append (list (current-error-port)
                               (string-append fmt "\n"))
                         args))
  (exit 1))

(define (info fmt . args)
  (apply printf (cons (string-append fmt "\n") args)))

(define (salmonella-error message)
  (abort (make-property-condition 'exn 'message message)))

(define (test-egg egg-location-uri tmp-dir)
  (let ((egg-locations (make-pathname tmp-dir "egg-locations"))
        (egg-name (pathname-file egg-location-uri)))

    (info "Writing egg-locations for ~a to ~a..." egg-name egg-locations)
    (with-output-to-file egg-locations
      (cut pp `(,(string->symbol egg-name) ,egg-location-uri)))

    (info "Running henrietta-cache...")
    (system* "henrietta-cache -c ~a -e ~a" tmp-dir egg-locations)

    (info "Finding out the latest version for ~a..." egg-name)
    (let ((versions (sort (directory (make-pathname tmp-dir egg-name))
                          version>=?)))
      (when (null? versions)
        (die! "Could not find any version for ~a." egg-name))

      (let ((latest-version (car versions)))
        (info "Running salmonella on ~a version ~a..." egg-name latest-version)
        (change-directory (make-pathname (list tmp-dir egg-name)
                                         latest-version))
        (system* "salmonella")
        (unless (file-exists? "salmonella.log")
          (salmonella-error "salmonella.log not found"))
        (let ((salmonella-log (read-log-file "salmonella.log"))
              (egg (string->symbol egg-name))
              (status-zero? (lambda (status) ;; not available in old salmonellas
                              (and status (zero? status)))))
          (unless (status-zero? (install-status egg salmonella-log))
            (salmonella-error "Installation failed."))
          (when (and (has-test? egg salmonella-log)
                     (not (status-zero? (test-status egg salmonella-log))))
            (salmonella-error "Tests failed.")))))))

(define (main args)
  (when (null? args)
    (usage 1))
  (let ((egg-location-uri (car args))
        (tmp-dir (create-temporary-directory)))
    (handle-exceptions exn
      (info
       (string-append
        "Leaving ~a for you to manually remove, as you probably want to "
        "debug something, since something went wrong while installing/testing "
        "the egg.")
       tmp-dir)
      (test-egg egg-location-uri tmp-dir)
      (info "Removing ~a" tmp-dir)
      (delete-directory tmp-dir 'recursively)
      (info "Egg looks ok!"))))

(main (command-line-arguments))

) ;; end module
