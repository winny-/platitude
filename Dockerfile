FROM racket/racket:8.3

WORKDIR /app

COPY info.rkt .
RUN raco pkg install --auto --no-docs --name platitude

COPY . .

RUN raco setup --no-docs platitude

ENTRYPOINT /app/entrypoint.sh
