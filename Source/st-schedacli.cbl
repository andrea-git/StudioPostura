       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      st-schedacli.
       AUTHOR.                          Andrea.
      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.                                            
       FILE-CONTROL.
           copy "clienti.sl".
           copy "tschede.sl".
           copy "rschede.sl".
           copy "param.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "clienti.fd".
           copy "tschede.fd".
           copy "rschede.fd".
           copy "param.fd".

       WORKING-STORAGE SECTION.
      * COPY   
       copy "acugui.def".
       copy "spooler.def".
       copy "fonts.def".
       copy "selprint.lks".

       77  status-clienti        pic xx.
       77  status-tschede        pic xx.
       77  status-rschede        pic xx.
       77  status-param          pic xx.

      * COSTANTI
       78  titolo                value "Studio - Scheda Cliente".
       78  78-passo-intestazione value 0,3.
       78  78-max-righe          value 30.
       78  78-passo              value 0,7.
       78  78-margine-basso      value 27,4.

      * RIGHE PER LA STAMPA
       01  r-titolo              pic x(100).

       01  r-riga.
           05 r-data             pic x(10).
           05 r-articolo         pic x(6).
           05 r-descrizione      pic x(50).
           05 r-qta              pic z.zz9.
           05 r-prz              PIC z.zz9,99.
           05 filler             pic x(2) value " €".

      * FLAGS
       77  controlli             pic xx.
           88  tutto-ok          value "OK".
           88  errori            value "ER".
       77  filler                pic 9.
           88 RecLocked          value 1 false 0.
       77  filler                pic 9.
           88 trovato            value 1 false 0.
       77  filler                pic 9.
           88 record-ok          value 1 false 0.

      * VARIABILI
       
       77  messaggio             pic x(150) value spaces.
       77  font-size-dply        pic z(5).      
       77  WFONT-STATUS          pic s9(5)  value zero.

       77  Calibri20BI           handle of font.
       77  Calibri16B            handle of font.
       77  Calibri12B            handle of font.
       77  Calibri11             handle of font.
       77  Calibri8              handle of font.
       77  save-riga             pic 9(7)v99.
       77  save-altezza-pagina   pic 9(7)v99.
       77  como-data             pic 9(8).
       77  como-ora              pic 9(8).

       77  num-righe             pic 9(3).
       77  pagina                pic 9(3) value 0.
       77  tot-pagine            pic 9(3). 
       77  resto                 pic 9(3). 
       
       77  pagina-z              pic z(3).
       77  tot-pagine-z          pic z(3). 

       LINKAGE SECTION.
       77  link-scheda           pic 9(6).
       77  link-stampante        pic x(200).

      ******************************************************************
       PROCEDURE DIVISION using link-scheda link-stampante.

      ***---
       MAIN-PRG.
           perform INIT.
           perform OPEN-FILES.
           if tutto-ok
              perform ELABORAZIONE
              perform CLOSE-FILES
           end-if.
           perform EXIT-PGM.

      ***---
       INIT.
           set environment "PRINTER" to "-P SPOOLER".
           set tutto-ok  to true.
           set RecLocked to false.
           set trovato   to false.

      ***---
       OPEN-FILES.
           open input tschede rschede clienti param.

      ***---
       ELABORAZIONE.
           move link-scheda to tsc-codice.
           read tschede no lock 
                invalid set errori to true
           end-read.
           if tutto-ok
              move low-value  to rsc-rec
              move tsc-codice to rsc-codice
              start rschede key >= rsc-chiave
                    invalid continue
                not invalid
                    move tsc-cliente to cli-codice
                    read clienti no lock invalid continue end-read
                    move spaces to prm-chiave
                    read param no lock
                    inspect prm-ragsoc
                            replacing trailing spaces by low-value
                    move 0 to num-righe
                    perform until 1 = 2
                       read rschede next at end exit perform end-read
                       if rsc-codice not = tsc-codice
                          exit perform
                       end-if
                       add 1 to num-righe
                    end-perform
                    if num-righe <= 78-max-righe
                       move 1 to tot-pagine
                    else
                       move 0 to resto
                       divide num-righe by 78-max-righe 
                                 giving tot-pagine
                              remainder resto
                       if resto > 0
                          add 1 to tot-pagine
                       end-if
                    end-if
                    perform STAMPA
              end-start
           end-if.

      ***---
       STAMPA.
           if link-stampante = spaces
              initialize spooler-link
              call   "selprint" using selprint-linkage
              cancel "selprint"
           else
              move link-stampante to selprint-stampante
           end-if. 

           if selprint-stampante not = space
              move selprint-num-copie to SPL-NUM-COPIE
              move selprint-stampante to SPL-NOME-STAMPANTE

              move titolo to spl-nome-job
              set spl-apertura to true
              set spl-vertical to true
              set WFDEVICE-WIN-PRINTER    to true
              call "spooler" using spooler-link
              if spl-sta-annu
                 set errori to true
              else
                 move spl-altezza to save-altezza-pagina
                 perform CARICA-FONT
              end-if
           else
              set spl-sta-annu to true
              set errori to true
           end-if.

           if tutto-ok
             
              perform INTESTAZIONE
              
              move low-value  to rsc-rec
              move tsc-codice to rsc-codice
              start rschede key >= rsc-chiave
              perform until 1 = 2
                 read rschede next at end exit perform end-read
                 if rsc-codice not = tsc-codice
                    exit perform
                 end-if
                 add 1 to num-righe
                 if num-righe > 78-max-righe
                    perform SALTO-PAGINA
                    perform INTESTAZIONE
                 end-if
                 move Calibri11 to spl-hfont
                 initialize spl-riga-stampa
                 string rsc-data(7:2) delimited size
                        "/"            delimited size
                        rsc-data(5:2) delimited size
                        "/"            delimited size
                        rsc-data(1:4) delimited size
                        into r-data
                 end-string
                 move rsc-articolo    to r-articolo
                 move rsc-descrizione to r-descrizione
                 move rsc-qta         to r-qta
                 move rsc-netto       to r-prz
                 move r-riga          to spl-riga-stampa
                 move 2               to spl-tipo-colonna
                 perform SCRIVI
                 move 6 to spl-pen-width
                 move 1,5                to spl-colonna
                 move 19,0               to spl-colonna-fine
                 add  0,6 to save-riga giving spl-riga
                 move spl-riga to spl-riga-fine
                 set  spl-oggetto        to true
                 set  spl-linea          to true
                 set  spl-pen-solid      to true
                 set  spl-nero           to true
                 call "spooler"       using spooler-link
              end-perform

              set spl-chiusura to true
              call   "spooler" using spooler-link

           end-if.

      ***---
       INTESTAZIONE.
           move 8                  to spl-colonna.
           move 0,2                to save-riga.
           move "SCHEDA CLIENTE"   to spl-riga-stampa.
           move Calibri20BI        to spl-hfont.
           move 1                  to spl-tipo-colonna
           perform SCRIVI.
           move 0                  to spl-tipo-colonna.

           perform STAMPA-FRAMES.

           perform STAMPA-LINEA.
           perform STAMPA-PIE-DI-PAGINA.
                
           move 1,7        to spl-colonna.
           move 1,8        to save-riga.
           move Calibri16B to spl-hfont.
           
           initialize spl-riga-stampa.
           inspect cli-ragsoc replacing trailing spaces by low-value.
           string  cli-ragsoc delimited low-value
                   " "        delimited size
                   cli-nome   delimited size
                   into spl-riga-stampa
           end-string.
           perform SCRIVI.
           add 78-passo-intestazione to save-riga.
           add 0,1                   to save-riga.
                                        
           move Calibri12B to spl-hfont.
           initialize spl-riga-stampa
           inspect cli-indirizzo replacing trailing spaces by low-value.
           inspect cli-localita  replacing trailing spaces by low-value.

           string  "INDIRIZZO: " delimited size
                   cli-indirizzo delimited low-value
                   "  -  "       delimited size
                   "LOCALITA': " delimited size
                   cli-localita  delimited low-value
                   " ("          delimited size
                   cli-prov      delimited size
                   ")"           delimited size
                   into spl-riga-stampa
           end-string.
           perform SCRIVI.
           add 78-passo-intestazione to   save-riga.
           subtract 0,1              from save-riga.

           initialize spl-riga-stampa.
           inspect cli-email     replacing trailing spaces by low-value.
           inspect cli-tel       replacing trailing spaces by low-value.
           inspect cli-cell      replacing trailing spaces by low-value.
           inspect cli-fax       replacing trailing spaces by low-value.

           string  "EMAIL: "     delimited size
                   cli-email     delimited low-value
                   "  -  "       delimited size
                   "TEL: "       delimited size
                   cli-tel       delimited low-value
                   "  -  "       delimited size
                   "MOBILE: "    delimited size
                   cli-cell      delimited low-value
                   "  -  "       delimited size
                   "FAX: "       delimited size
                   cli-fax       delimited low-value
                   into spl-riga-stampa
           end-string.
           perform SCRIVI.
           
           move 5,15   to save-riga.
           move "DATA" to spl-riga-stampa.
           move 2,12   to spl-colonna.
           perform SCRIVI.
           subtract 78-passo from save-riga.
                                             
           move "ART."     to spl-riga-stampa.
           move 4,23       to spl-colonna.
           perform SCRIVI.
           subtract 78-passo from save-riga.
                                     
           move "DESCRIZIONE" to spl-riga-stampa.
           move 9,6           to spl-colonna.
           perform SCRIVI.
           subtract 78-passo from save-riga.
                                     
           move "QTA"  to spl-riga-stampa.
           move 16,22  to spl-colonna.
           perform SCRIVI.
           subtract 78-passo from save-riga.
           
           move "PREZZO" to spl-riga-stampa.
           move 17,4     to spl-colonna.
           perform SCRIVI.
           subtract 78-passo from save-riga.
           
           move 0   to num-righe.
           move 5,8 to save-riga.

      ***---
       STAMPA-FRAMES.
           move 9     to spl-pen-width.
           move 2,2   to save-riga.
           
           move save-riga to spl-riga.
           add 3,1 to spl-riga giving spl-riga-fine.

           move 1,5   to spl-colonna.
           move 19,0  to spl-colonna-fine.

           set  spl-oggetto       to true.
           set  spl-rettangolo    to true.
           set  spl-brush-null    to true.
           set  spl-nero          to true.
           call "spooler"         using spooler-link.

           set  spl-brush-ltgray  to true.
           call "spooler"         using spooler-link.

           add 0,5 to spl-riga-fine giving spl-riga.
           add 0,6 to spl-riga      giving spl-riga-fine.

           set  spl-oggetto       to true.
           set  spl-rettangolo    to true.
           set  spl-brush-null    to true.
           set  spl-nero          to true.
           call "spooler"         using spooler-link.
           
           set  spl-brush-ltgray  to true.
           call "spooler"         using spooler-link.

           move 1,5                to spl-colonna spl-colonna-fine.
           move 78-margine-basso   to spl-riga-fine.
           perform STAMPA-LINEA-VERTICALE.

           move 3,8                to spl-colonna spl-colonna-fine.
           move 78-margine-basso   to spl-riga-fine.
           perform STAMPA-LINEA-VERTICALE.
           
           move 5,5                to spl-colonna spl-colonna-fine.
           move 78-margine-basso   to spl-riga-fine.
           perform STAMPA-LINEA-VERTICALE.
           
           move 16,1               to spl-colonna spl-colonna-fine.
           move 78-margine-basso   to spl-riga-fine.
           perform STAMPA-LINEA-VERTICALE.
           
           move 17,1               to spl-colonna spl-colonna-fine.
           move 78-margine-basso   to spl-riga-fine.
           perform STAMPA-LINEA-VERTICALE.
           
           move 19,0               to spl-colonna spl-colonna-fine.
           move 78-margine-basso   to spl-riga-fine.
           perform STAMPA-LINEA-VERTICALE.

           perform STAMPA-LINEA.
            
      ***---
       STAMPA-LINEA.
           move 1,5                to spl-colonna.
           move 19,0               to spl-colonna-fine.
           move 78-margine-basso   to spl-riga spl-riga-fine.
           set  spl-oggetto        to true.
           set  spl-linea          to true.
           set  spl-pen-solid      to true.
           set  spl-nero           to true.
           call "spooler"       using spooler-link.
            
      ***---
       STAMPA-LINEA-VERTICALE.
           set  spl-oggetto        to true.
           set  spl-linea          to true.
           set  spl-pen-solid      to true.
           set  spl-nero           to true.
           call "spooler"       using spooler-link.

      ***---
       STAMPA-PIE-DI-PAGINA.
           subtract 0,6 from 78-margine-basso giving save-riga.
           add 1 to pagina.
           move Calibri8 to spl-hfont.
           accept como-data from century-date.
           move 1 to spl-tipo-colonna.
           initialize spl-riga-stampa.
           string "Rilasciata da  - " delimited size
                  prm-ragsoc          delimited low-value
                  " - il "            delimited size
                  como-data(7:2)      delimited size
                  "/"                 delimited size
                  como-data(5:2)      delimited size
                  "/"                 delimited size
                  como-data(1:4)      delimited size
                  into spl-riga-stampa
           end-string.
           perform SCRIVI.
           move 0 to spl-tipo-colonna.
           subtract 78-passo from save-riga.
                               
           move pagina     to pagina-z.
           move tot-pagine to tot-pagine-z.
           initialize spl-riga-stampa.
           string "Pag. "      delimited size
                  pagina-z     delimited size
                  "  di "      delimited size
                  tot-pagine-z delimited size
                  into spl-riga-stampa
           end-string.
           move 17,5 to spl-colonna.
           perform SCRIVI.

      ***---
       SCRIVI.
           add  78-passo      to save-riga.
           move save-riga     to spl-riga.
           set  spl-stringa   to true.
           call "spooler"  using spooler-link.

      ***---
       SALTO-PAGINA.
           set spl-salto-pagina to true.
           call "spooler" using spooler-link.

      ***---
       CARICA-FONT.
      * Calibri 20BI
           initialize wfont-data Calibri20BI.
           move 20 to wfont-size.
           move "Calibri"            to wfont-name.
           set  wfcharset-dont-care  to true.
           set  wfont-bold           to true.
           set  wfont-italic         to true.
           set  wfont-underline      to false.
           set  wfont-strikeout      to false.
           set  wfont-fixed-pitch    to false.
           move 0                    to wfont-char-set.
           set  wfdevice-win-printer to true. |E' un carattere per la stampante
           call "W$FONT" using wfont-get-font, Calibri20BI, wfont-data
                        giving wfont-status.
      
      * ISACCO (QUESTI TEST CONTROLLANO L'ESISTENZA DEL FONT)
           if wfont-status not = 1
              set errori to true
              perform MESSAGGIO-ERR-FONT
              exit paragraph
           end-if. 

      * Calibri 16B
           initialize wfont-data Calibri16B.
           move 16 to wfont-size.
           move "Calibri"            to wfont-name.
           set  wfcharset-dont-care  to true.
           set  wfont-bold           to true.
           set  wfont-italic         to false.
           set  wfont-underline      to false.
           set  wfont-strikeout      to false.
           set  wfont-fixed-pitch    to false.
           move 0                    to wfont-char-set.
           set  wfdevice-win-printer to true. |E' un carattere per la stampante
           call "W$FONT" using wfont-get-font, Calibri16B, wfont-data
                        giving wfont-status.

      * ISACCO (QUESTI TEST CONTROLLANO L'ESISTENZA DEL FONT)
           if wfont-status not = 1
              set errori to true
              perform MESSAGGIO-ERR-FONT
              exit paragraph
           end-if.

      * Calibri 11
           initialize wfont-data Calibri11.
           move 11 to wfont-size.
           move "Calibri"            to wfont-name.
           set  wfcharset-dont-care  to true.
           set  wfont-bold           to false.
           set  wfont-italic         to false.
           set  wfont-underline      to false.
           set  wfont-strikeout      to false.
           set  wfont-fixed-pitch    to false.
           move 0                    to wfont-char-set.
           set  wfdevice-win-printer to true. |E' un carattere per la stampante
           call "W$FONT" using wfont-get-font, Calibri11, wfont-data
                        giving wfont-status.
      
      * ISACCO (QUESTI TEST CONTROLLANO L'ESISTENZA DEL FONT)
           if wfont-status not = 1
              set errori to true
              perform MESSAGGIO-ERR-FONT
              exit paragraph
           end-if. 

      * Calibri 12B
           initialize wfont-data Calibri12B.
           move 12 to wfont-size.
           move "Calibri"            to wfont-name.
           set  wfcharset-dont-care  to true.
           set  wfont-bold           to true.
           set  wfont-italic         to false.
           set  wfont-underline      to false.
           set  wfont-strikeout      to false.
           set  wfont-fixed-pitch    to false.
           move 0                    to wfont-char-set.
           set  wfdevice-win-printer to true. |E' un carattere per la stampante
           call "W$FONT" using wfont-get-font, Calibri12B, wfont-data
                        giving wfont-status.
      
      * ISACCO (QUESTI TEST CONTROLLANO L'ESISTENZA DEL FONT)
           if wfont-status not = 1
              set errori to true
              perform MESSAGGIO-ERR-FONT
              exit paragraph
           end-if. 

      * Calibri 8B
           initialize wfont-data Calibri8.
           move 8 to wfont-size.
           move "Calibri"            to wfont-name.
           set  wfcharset-dont-care  to true.
           set  wfont-bold           to false.
           set  wfont-italic         to false.
           set  wfont-underline      to false.
           set  wfont-strikeout      to false.
           set  wfont-fixed-pitch    to false.
           move 0                    to wfont-char-set.
           set  wfdevice-win-printer to true. |E' un carattere per la stampante
           call "W$FONT" using wfont-get-font, Calibri8, wfont-data
                        giving wfont-status.
      
      * ISACCO (QUESTI TEST CONTROLLANO L'ESISTENZA DEL FONT)
           if wfont-status not = 1
              set errori to true
              perform MESSAGGIO-ERR-FONT
              exit paragraph
           end-if. 

      ***---
       MESSAGGIO-ERR-FONT.
      * ISACCO (MESSAGGIO DI ERRORE ED USCITA SE NON TROVA UN FONT)
           initialize messaggio.

           inspect wfont-name replacing trailing space by low-value.
           move wfont-size    to font-size-dply.

           string  "Font: "         delimited size
                   WFONT-NAME       delimited low-value
                   X"0D0A"          delimited size
                   "Dimensione: ",  delimited size 
                   FONT-SIZE-DPLY,  delimited size
                   X"0D0A"          delimited size
                   "Non installato. La stampa verrà abortita!"
                                    delimited size
              into messaggio.

           inspect messaggio replacing trailing space by low-value.

           display message messaggio.

      ***---
       CLOSE-FILES.
           close clienti tschede rschede param.

      ***---
       EXIT-PGM.
           set environment "PRINTER" to "-P SPOOLER-DIRECT".

           destroy Calibri20BI.
           destroy Calibri16B.
           destroy Calibri11.
           destroy Calibri12B.
           destroy Calibri8.

           cancel "spooler".
           initialize spooler-link.
           goback.
