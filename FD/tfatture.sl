       SELECT tfatture
           ASSIGN       TO  "tfatture"
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-tfatture
           RECORD KEY   IS tfa-chiave
           ALTERNATE RECORD KEY IS tfa-k-data = tfa-data-doc, tfa-chiave
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS tfa-k-cliente = tfa-cliente, 
           tfa-data-doc, tfa-numero
           WITH DUPLICATES .
