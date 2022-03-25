       SELECT tlistini
           ASSIGN       TO  "tlistini"
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-tlistini
           RECORD KEY   IS tls-chiave
           ALTERNATE RECORD KEY IS tls-k-tipo = tls-tipo, 
           tls-codice-tipo, tls-ini-val, tls-chiave
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS tls-k-descrizione = tls-descrizione, 
           tls-codice
           WITH DUPLICATES .
