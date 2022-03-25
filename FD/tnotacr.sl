       SELECT tnotacr
           ASSIGN       TO  "tnotacr"
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-tnotacr
           RECORD KEY   IS tno-chiave
           ALTERNATE RECORD KEY IS tno-k-data = tno-data-doc, tno-chiave
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS tno-k-cliente = tno-cliente, 
           tno-data-doc, tno-numero
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS tno-k-fattura = tno-fattura, 
           tno-chiave
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS tno-k-corrisp = tno-corrisp, 
           tno-chiave
           WITH DUPLICATES .
