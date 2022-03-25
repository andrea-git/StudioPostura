       SELECT clienti
           ASSIGN       TO  "clienti"
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-clienti
           RECORD KEY   IS cli-chiave
           ALTERNATE RECORD KEY IS cli-k-ragsoc = cli-ragsoc, cli-chiave
           WITH DUPLICATES .
