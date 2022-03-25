       SELECT tcorrisp
           ASSIGN       TO  "tcorrisp"
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-tcorrisp
           RECORD KEY   IS tco-chiave
           ALTERNATE RECORD KEY IS tco-k-data = tco-data-doc, tco-chiave
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS tco-k-cliente = tco-cliente, 
           tco-data-doc, tco-numero
           WITH DUPLICATES .
