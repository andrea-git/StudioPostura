      ** rsc-riga = Nel caso ci siano più impegni o appuntamenti nello stesso giorno per lo stesso trattamento, oppure come semplice contatore per le righe di note (default dal cliente) dato che l'articolo sarà vuoto e la data la stessa
       SELECT rschede
           ASSIGN       TO  "rschede"
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           FILE STATUS  IS STATUS-rschede
           RECORD KEY   IS rsc-chiave
           ALTERNATE RECORD KEY IS rsc-k-art = rsc-articolo, 
           rsc-cliente, rsc-data
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS rsc-k-cli = rsc-cliente, 
           rsc-articolo, rsc-data
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS rsc-k-fatart = rsc-fattura, 
           rsc-articolo
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS rsc-k-fatcli = rsc-fattura, 
           rsc-cliente
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS rsc-k-corart = rsc-corrisp, 
           rsc-articolo
           WITH DUPLICATES 
           ALTERNATE RECORD KEY IS rsc-k-corcli = rsc-corrisp, 
           rsc-cliente
           WITH DUPLICATES .
