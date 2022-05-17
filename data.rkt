#lang racket/base

(require db
         racket/function
         racket/match
         racket/contract
         "logger.rkt")

(provide (prefix-out db: (all-defined-out)))

(struct comment (id author timestamp commentary) #:transparent)
(struct article (id locator comments) #:transparent)

(define db-conn (make-parameter #f))

(define (comments-for-article #:locator locator)
  (define Q
    #<<END
SELECT
  comment_id,
  author,
  timestamp,
  commentary
FROM
  comments_by_article
WHERE
  locator = $1
END
)
  (define rows
    (query-rows (db-conn) Q locator))
  (for/list ([r rows])
    (match-define (vector comment-id author timestamp commentary) r)
    (comment comment-id author timestamp commentary)))

(define (create-comment locator the-comment)
  (define article-id
    (query-value (db-conn) #<<END
WITH sel AS (
  SELECT id
  FROM articles
  WHERE locator = $1
), ins AS (
  INSERT INTO articles (locator)
  SELECT $1
  WHERE NOT EXISTS (TABLE sel)
  RETURNING id
)
SELECT id
FROM sel
UNION
SELECT id FROM ins
END
                 locator ))
  (define comment-id (query-value (db-conn) #<<END
INSERT INTO comments
  (article, author, commentary)
VALUES ($1, $2, $3)
RETURNING id
END
                                  article-id (comment-author the-comment) (comment-commentary the-comment)))
  (struct-copy comment the-comment [id comment-id]))

(define (setup-connection)
  (db-conn
   (virtual-connection
    (connection-pool
     (thunk (postgresql-connect
             #:user (getenv "POSTGRES_USER")
             #:port (match (getenv "POSTGRES_PORT")
                      [#f 5432]
                      [s (string->number s)])
             #:database (getenv "POSTGRES_DB")
             #:server (getenv "POSTGRES_HOST")
             #:password (getenv "POSTGRES_PASSWORD")
             #:ssl 'optional)))))
  (log-platitude-info "Creating schema...")
  (query-exec (db-conn) #<<END
CREATE TABLE IF NOT EXISTS articles
  ("id" SERIAL PRIMARY KEY,
   "locator" TEXT NOT NULL)
END
              )
  (query-exec (db-conn) #<<END
CREATE TABLE IF NOT EXISTS comments
  ("id" SERIAL PRIMARY KEY,
   "article" integer NOT NULL,
   "commentary" TEXT NOT NULL,
   "author" TEXT NOT NULL,
   "timestamp" TIMESTAMPTZ NOT NULL DEFAULT now()::timestamp,
   FOREIGN KEY ("article") REFERENCES articles ("id")
     ON DELETE CASCADE
     ON UPDATE NO ACTION)
END
              )
  (query-exec (db-conn) #<<END
CREATE OR REPLACE VIEW comments_by_article AS
  SELECT
    comments.id AS comment_id,
    articles.id AS article_id,
    articles.locator AS locator,
    comments.author AS author,
    comments.commentary AS commentary,
    comments.timestamp AS timestamp
  FROM
    comments
  INNER JOIN
    articles
  ON
    articles.id = comments.article
  ORDER BY
    articles.id,
    comments.id
  DESC
END
              )
  )
