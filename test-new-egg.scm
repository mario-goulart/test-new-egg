(module test-new-egg ()

(import scheme)
(cond-expand
 (chicken-4
  (import chicken foreign)
  (use data-structures extras files posix setup-api utils srfi-1)
  (use salmonella-log-parser)
  (define chicken-major-version 4)
  (define egg-file-extension ".setup"))
 (chicken-5
  (import (chicken base)
          (chicken condition)
          (chicken file)
          (chicken foreign)
          (chicken format)
          (chicken irregex)
          (chicken pathname)
          (chicken pretty-print)
          (chicken process)
          (chicken process-context)
          (chicken sort)
          (chicken string))
  (import salmonella-log-parser srfi-1)

  ;; From setup-api (chicken-4.13.0)
  (define (version>=? v1 v2)
    (define (version->list v)
      (map (lambda (x) (or (string->number x) x))
           (irregex-split "[-\\._]" (->string v))))
    (let loop ((p1 (version->list v1))
               (p2 (version->list v2)))
      (cond ((null? p1) (null? p2))
            ((null? p2))
            ((number? (car p1))
             (and (number? (car p2))
                  (or (> (car p1) (car p2))
                      (and (= (car p1) (car p2))
                           (loop (cdr p1) (cdr p2))))))
            ((number? (car p2)))
            ((string>? (car p1) (car p2)))
            (else
             (and (string=? (car p1) (car p2))
                  (loop (cdr p1) (cdr p2)))))))

  (define chicken-major-version 5)
  (define egg-file-extension ".egg"))
 (else
  (error "Unsupported CHICKEN version.")))

(define version "1.0.4")

(define (usage #!optional exit-code)
  (let ((port (if (and exit-code (not (zero? exit-code)))
                  (current-error-port)
                  (current-output-port)))
        (this (pathname-strip-directory (program-name))))
    (fprintf port "Usage: ~a <egg name> <.release-info URI>\n" this)
    (fprintf port "       ~a [-h|-help|--help|--version]\n" this)
    (when exit-code
      (exit exit-code))))

(define (info fmt . args)
  (apply printf (cons (string-append fmt "\n") args)))

(define (raise-error message)
  (abort (make-property-condition 'exn 'message message)))

(define (test-egg egg-name egg-location-uri tmp-dir)
  (let* ((egg-locations (make-pathname tmp-dir "egg-locations"))
         (bin-prefix (foreign-value "C_TARGET_BIN_HOME" c-string))
         (henrietta-cache
          (or (get-environment-variable "TEST_NEW_EGG_HENRIETTA_CACHE")
              (make-pathname bin-prefix "henrietta-cache"))))

    (info "Writing egg-locations for ~a to ~a..." egg-name egg-locations)
    (with-output-to-file egg-locations
      (cut pp `(,(string->symbol egg-name) ,egg-location-uri)))

    (info "Running ~a..." henrietta-cache)
    (system* (sprintf "~a -c ~a -e ~a -r ~a"
                      henrietta-cache
                      tmp-dir
                      egg-locations
                      chicken-major-version))

    (info "Finding out the latest version for ~a..." egg-name)
    (let ((versions (sort (directory (make-pathname tmp-dir egg-name))
                          version>=?)))
      (when (null? versions)
        (raise-error (sprintf "Could not find any version for ~a." egg-name)))

      (let ((latest-version (car versions))
            (salmonella
             (or (get-environment-variable "TEST_NEW_EGG_SALMONELLA")
                 (make-pathname bin-prefix "salmonella")))
            (egg-filename (string-append egg-name egg-file-extension)))
        (info "Running ~a on ~a version ~a..." salmonella egg-name latest-version)
        (change-directory (make-pathname (list tmp-dir egg-name)
                                         latest-version))
        (unless (file-exists? egg-filename)
          (raise-error (sprintf "Could not find ~a" egg-filename)))
        (system* salmonella)
        (unless (file-exists? "salmonella.log")
          (raise-error "salmonella.log not found"))
        (let ((salmonella-log (read-log-file "salmonella.log"))
              (egg (string->symbol egg-name))
              (status-zero? (lambda (status) ;; not available in old salmonellas
                              (and status (zero? status)))))
          (unless (status-zero? (install-status egg salmonella-log))
            (raise-error "Installation failed."))
          (when (and (has-test? egg salmonella-log)
                     (not (status-zero? (test-status egg salmonella-log))))
            (raise-error "Tests failed.")))))))

;; From henrietta.  If you modify this code, please also change it in
;; henrietta.
(define illegal-name?
  (let ((legal-chars
         (string->list
          "0123456789abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ+-_")))
    (lambda (name)
      (not (every (cut member <> legal-chars)
                  (string->list name))))))

(define (main args)
  (when (null? args)
    (usage 1))

  (when (member (car args) '("-h" "-help" "--help"))
    (usage 0))

  (when (equal? (car args) "--version")
    (print version)
    (exit 0))

  (when (null? (cdr args))
    (usage 1))

  (let ((egg-name (car args))
        (egg-location-uri (cadr args))
        (tmp-dir (create-temporary-directory)))

    (when (illegal-name? egg-name)
      (fprintf
       (current-error-port)
       (string-append
        "~a: invalid egg name.  See "
        "http://wiki.call-cc.org/eggs%20tutorial#naming-your-extension "
        "for egg naming rules.\n")
       egg-name)
      (exit 1))

    (handle-exceptions exn
      (info
       (string-append
        "Leaving ~a for you to manually remove, as you probably want to "
        "debug something, since something went wrong while installing/testing "
        "the egg.")
       tmp-dir)
      (test-egg egg-name egg-location-uri tmp-dir)
      (info "Removing ~a" tmp-dir)
      (delete-directory tmp-dir 'recursively)
      (info "Egg looks ok!"))))

(main (command-line-arguments))

) ;; end module
