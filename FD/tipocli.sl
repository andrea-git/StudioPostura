       SELECT tipocli
           ASSIGN       TO  "tipocli"
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-tipocli
           RECORD KEY   IS tcl-chiave
           ALTERNATE RECORD KEY IS tcl-k-descrizione = tcl-descrizione, 
           tcl-chiave
           WITH DUPLICATES .
