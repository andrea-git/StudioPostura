       SELECT articoli
           ASSIGN       TO  "articoli"
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-articoli
           RECORD KEY   IS art-chiave
           ALTERNATE RECORD KEY IS art-k-descrizione = art-descrizione, 
           art-chiave
           WITH DUPLICATES .
