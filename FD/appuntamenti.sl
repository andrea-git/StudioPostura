       SELECT appuntamenti
           ASSIGN       TO  "appuntamenti"
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-appuntamenti
           RECORD KEY   IS app-chiave
           ALTERNATE RECORD KEY IS k-clidata = app-cliente, app-data, 
           app-ora, app-stato
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS k-statocli = app-stato, app-cliente, 
           app-data, app-ora
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS k-ricerca = app-cliente, app-data, 
           app-ora, app-stato, app-pagato
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS k-pagato = app-pagato, app-cliente, 
           app-data, app-ora
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS k-recupero = app-recup, app-cliente, 
           app-data, app-ora
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS k-statodata = app-stato, app-data, 
           app-cliente, app-ora
           WITH DUPLICATES .
