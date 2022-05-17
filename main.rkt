#lang racket/base

(require db
         xml
         net/mime-type
         net/url
         racket/bool
         racket/cmdline
         racket/date
         racket/exn
         racket/format
         racket/function
         racket/list
         racket/logging
         racket/match
         racket/math
         racket/port
         racket/string
         web-server/http/request-structs
         web-server/dispatch
         web-server/http
         web-server/safety-limits
         web-server/servlet-env
         web-server/templates
         web-server/servlet/web
         "data.rkt"
         "helpers.rkt"
         "logger.rkt")

(define *comments* (make-parameter empty))

(define site-title "platitude")

(define-values (dispatch rev-url)
  (dispatch-rules
   [((string-arg)) #:method "get" get-iframe]
   [((string-arg)) #:method "post" post-comment]))

(define ((headize start) req)
  (match (request-method req)
    [#"HEAD"
     (define get-response
       (start (struct-copy request req [method #"GET"])))
     (struct-copy response get-response
                  [output void])]
    [_ (start req)]))

(define ((log-request start) req)
  (define res (start req))
  (log-platitude-info "~a ~a -> ~a"
                   (request-method req)
                   (url->string (request-uri req))
                   (response-code res))
  res)

;; XXX is there a better way to do this?  This feels not very user friendly.
(define (sql-timestamp->string sql-ts)
  (match-define (struct sql-timestamp (year month day hour minute second nanosecond tz))
    sql-ts)
  (define (f v)
    (~r v #:min-width 2 #:pad-string "0"))
  (format "~a-~a-~a ~a:~a" (f year) month day (f hour) (f minute)))

(define (epoch->rfc2822 epoch)
  (parameterize ([date-display-format 'rfc2822])
    (date->string (seconds->date epoch #f) #t)))

(define (site-baseurl req)
  (define h (request-headers/raw req))
  (define s
    (cond
      [(headers-assq #"Forwarded" h) => (compose1 bytes->string/utf-8 header-value)]
      [(headers-assq #"X-Forwarded-For" h) => (compose1 bytes->string/utf-8 header-value)]
      [(getenv "APP_URL") => identity]
      [(headers-assq #"Host" h) => (compose1 bytes->string/utf-8 header-value)]
      [else (format "~a:~a" (request-host-ip req) (request-host-port req))]))
  (match (string-append (string-trim s "/" #:repeat? #t) "/")
    [(regexp "https?://.*" (list m)) m]
    [no-scheme (string-append "http://" no-scheme)]))

(define (get-iframe req locator)
  (parameterize ([*comments* (db:comments-for-article #:locator locator)])
    (response/200
     #:mime-type TEXT/HTML-MIME-TYPE
     (include-template "templates/iframe.html"))))

(define (post-comment req locator)
  (define binds (request-bindings/raw req))
  (log-platitude-info "Bindings ~a" binds)
  (define author-b (bindings-assq #"author" binds))
  (define commentary-b (bindings-assq #"commentary" binds))
  (cond [(and author-b commentary-b)
         (define author (bytes->string/utf-8 (binding:form-value author-b)))
         (define commentary (bytes->string/utf-8 (binding:form-value commentary-b)))
         (db:create-comment locator (db:comment #f author #f commentary))
         (redirect-to (url->string (request-uri req)))]
        [else
         (response/400 "Bad request\n")]))

(define (not-found req)
  (response/404 "Not found\n"))

(define (fun)
  (define listen-port (make-parameter 8080))
  (define listen-ip (make-parameter #f))
  (command-line
   #:once-each
   [("-p" "--port") port-string "Port to listen on"
                    (listen-port (string->number port-string))]
   [("--ip") ip-string "IP to listen on"
             (listen-ip ip-string)])
  (db:setup-connection)
  (log-platitude-info "Connected and schema created.  Visit http://~a:~a/" (or (listen-ip) "0.0.0.0") (listen-port))
  (define max-waiting 511)
  (define safety-limits
    (make-safety-limits #:max-waiting max-waiting
                        #:max-form-data-field-length (sqr 1024)))
  (serve/servlet (log-request (headize dispatch))
                 #:stateless? #t
                 #:listen-ip (listen-ip)
                 #:port (listen-port)
                 #:servlet-path "/"
                 #:servlet-regexp #rx""
                 #:command-line? #t
                 #:server-root-path "."
                 #:safety-limits safety-limits
                 #:file-not-found-responder (log-request (headize not-found))))

(module+ main
  (with-logging-to-port (current-error-port)
    fun
    #:logger platitude-logger 'debug 'platitude))
