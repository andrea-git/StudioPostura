      * Paragrafo per la struttura dei controlli sulla screen Form1
           evaluate control-id
           |78-ID-ef-codice � l'ID del control ef-codice
           when 78-ID-ef-codice
                inquire ef-codice, value in ef-codice-buf

           |78-ID-ef-descr � l'ID del control ef-descr
           when 78-ID-ef-descr
                inquire ef-descr, value in ef-descr-buf

           end-evaluate.

