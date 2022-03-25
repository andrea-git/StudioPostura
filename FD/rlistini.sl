       SELECT rlistini
           ASSIGN       TO  "rlistini"
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-rlistini
           RECORD KEY   IS rls-chiave
           ALTERNATE RECORD KEY IS rls-k-ricerca = rls-tipo, 
           rls-codice-tipo, rls-articolo, rls-ini-val
           WITH DUPLICATES .
