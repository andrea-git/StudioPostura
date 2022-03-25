      * Paragrafo per la struttura dei controlli sulla screen Form1
           evaluate control-id
           |78-ID-ef-codice è l'ID del control ef-codice
           when 78-ID-ef-codice
                inquire ef-codice, value in ef-codice-buf

           |78-ID-ef-descrizione è l'ID del control ef-descrizione
           when 78-ID-ef-descrizione
                inquire ef-descrizione, value in ef-descrizione-buf

           |78-ID-ef-ini-val è l'ID del control ef-ini-val
           when 78-ID-ef-ini-val
                inquire ef-ini-val, value in ef-ini-val-buf

           |78-ID-ef-tipo è l'ID del control ef-tipo
           when 78-ID-ef-tipo
                inquire ef-tipo, value in ef-tipo-buf

           end-evaluate.

