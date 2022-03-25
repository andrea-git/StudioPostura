       SELECT tschede
           ASSIGN       TO  "tschede"
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-tschede
           RECORD KEY   IS tsc-chiave
           ALTERNATE RECORD KEY IS tsc-k-data = tsc-data-creazione, 
           tsc-cliente
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS tsc-k-cliente = tsc-cliente.
