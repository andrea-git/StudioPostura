       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      st-cli-det.
       AUTHOR.                          Andrea.
      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.                                            
       FILE-CONTROL.
           copy "clienti.sl".
           copy "tipocli.sl".
           copy "codiva.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "clienti.fd".
           copy "tipocli.fd".
           copy "codiva.fd".

       WORKING-STORAGE SECTION.
      * COPY   
       copy "acugui.def".
       copy "spooler.def".
       copy "fonts.def".
       copy "selprint.lks".

       77  status-clienti        pic xx.
       77  status-tipocli        pic xx.
       77  status-codiva         pic xx.

      * COSTANTI
       78  titolo                value "Studio - Dettaglio cliente".

      * RIGHE PER LA STAMPA
       01  r-titolo              pic x(100).

       01  r1-fissa.
           05 filler             pic x(13) value "Provincia".
           05 filler             pic x(8)  value "C.A.P.".
           05 filler             pic x(8)  value "Nazione".

       01  r1.
           05 r1-prov            pic x(13).
           05 r1-cap             pic x(8).
           05 r1-naz             pic x(8).

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
       77  valore-z              pic zzz.zzz.zz9,99.
       77  valore-x              pic x(14).
       
       77  BitmapClienteHandle   pic S9(9) comp-4.

       77  messaggio             pic x(150) value spaces.
       77  font-size-dply        pic z(5).      
       77  WFONT-STATUS          pic s9(5)  value zero.

       77  Calibri20BI           handle of font.
       77  Calibri14B            handle of font.
       77  Calibri12BI           handle of font.
       77  Calibri12B            handle of font.
       77  Calibri8              handle of font.
       77  passo                 pic 99v99.
       77  save-riga             pic 9(7)v99.
       77  save-altezza-pagina   pic 9(7)v99.
       77  como-data             pic 9(8).
       77  como-ora              pic 9(8).

       77  sconto1-ed            pic zz9,99.
      ***** 77  sconto2-ed            pic zz9,99.

       LINKAGE SECTION.
       77  link-cliente          pic x(6).
       77  link-stampante        pic x(200).

      ******************************************************************
       PROCEDURE DIVISION using link-cliente link-stampante.

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
           move 0,7  to passo.
           set environment "PRINTER" to "-P SPOOLER".
           set tutto-ok  to true.
           set RecLocked to false.
           set trovato   to false.

      ***---
       OPEN-FILES.
           open input clienti tipocli codiva.

      ***---
       ELABORAZIONE.
           move link-cliente to cli-codice.
           read clienti no lock 
                invalid set errori to true
           end-read.
           if tutto-ok
              move cli-tipologia to tcl-codice
              read tipocli no lock invalid continue end-read
                                                             
              move cli-iva-esente to iva-codice
              read codiva no lock invalid continue end-read

              perform STAMPA
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

              perform STAMPA-BITMAP
            
              move 0,5              to save-riga
              move "** DETTAGLIO CLIENTE **" to spl-riga-stampa
              move Calibri20BI      to spl-hfont
              move 58               to spl-tipo-colonna
              perform SCRIVI

              perform STAMPA-FRAMES

              perform STAMPA-LINEA

              initialize r-titolo
              string "CODICE: "   delimited size
                     link-cliente delimited size
                     into r-titolo
              end-string

              move 1,7  to save-riga

              move Calibri14B to spl-hfont
              move r-titolo to spl-riga-stampa
              perform SCRIVI

              move 2,6 to save-riga
              move Calibri12B to spl-hfont
              
              initialize spl-riga-stampa
              inspect cli-ragsoc replacing trailing spaces by low-value
              string  cli-ragsoc delimited low-value
                      " "           delimited size
                      cli-nome   delimited size
                      into spl-riga-stampa
              end-string
              perform SCRIVI
              
              move 8,8         to save-riga
              move Calibri12BI to spl-hfont
              move "INDIRIZZO" to spl-riga-stampa
              perform SCRIVI

              move 9,3           to save-riga
              move Calibri12B    to spl-hfont
              move cli-indirizzo to spl-riga-stampa
              perform SCRIVI

              move 10,3        to save-riga
              move Calibri12BI to spl-hfont
              move "LOCALITA'" to spl-riga-stampa
              perform SCRIVI

              move 10,8         to save-riga
              move Calibri12B   to spl-hfont
              move cli-localita to spl-riga-stampa
              perform SCRIVI

              move 11,8        to save-riga
              move Calibri12BI to spl-hfont
              move r1-fissa    to spl-riga-stampa
              move 75          to spl-tipo-colonna
              perform SCRIVI

              move 12,3             to save-riga
              move Calibri12B       to spl-hfont
              move cli-cap          to r1-cap
              move cli-prov         to r1-prov
              move cli-cittadinanza to r1-naz
              move r1               to spl-riga-stampa
              move 75               to spl-tipo-colonna
              perform SCRIVI

              move 58          to spl-tipo-colonna
              move 13,3        to save-riga
              move Calibri12BI to spl-hfont
              move "TIPOLOGIA CLIENTE" to spl-riga-stampa
              perform SCRIVI

              move 13,8       to save-riga
              move Calibri12B to spl-hfont
              initialize r-titolo
              string tcl-codice      delimited size
                     " - "           delimited size
                     tcl-descrizione delimited size
                     into r-titolo
              end-string
              move r-titolo to spl-riga-stampa
              perform SCRIVI

              move 14,8        to save-riga
              move Calibri12BI to spl-hfont
              move "DATI NASCITA" to spl-riga-stampa
              perform SCRIVI

              move 15,3       to save-riga
              move Calibri12B to spl-hfont
              inspect cli-luogo-n replacing trailing spaces by low-value
              initialize r-titolo
              string cli-luogo-n     delimited low-value
                     " ("            delimited size
                     cli-prov-n      delimited size
                     ")   -  "       delimited size
                     cli-data-n(7:2) delimited size
                     "/"             delimited size
                     cli-data-n(5:2) delimited size
                     "/"             delimited size
                     cli-data-n(1:4) delimited size
                     into r-titolo
              end-string
              move r-titolo   to spl-riga-stampa
              perform SCRIVI

              move 16,3        to save-riga
              move Calibri12BI to spl-hfont
              move "CONTATTI"  to spl-riga-stampa
              perform SCRIVI

              move 16,8       to save-riga
              move Calibri12B to spl-hfont
              inspect cli-email replacing trailing spaces by low-value
              inspect cli-tel   replacing trailing spaces by low-value
              inspect cli-cell  replacing trailing spaces by low-value

              initialize r-titolo
              string "E-MAIL: "      delimited size
                     cli-email       delimited low-value
                     "  -  TEL: "    delimited size
                     cli-tel         delimited low-value
                     "  -  MOBILE: " delimited size
                     cli-cell        delimited low-value
                     into r-titolo
              end-string
              move r-titolo to spl-riga-stampa
              perform SCRIVI

              move 17,8           to save-riga
              move Calibri12BI    to spl-hfont
              move "FATTURAZIONE" to spl-riga-stampa
              perform SCRIVI

              move 18,3       to save-riga
              move Calibri12B to spl-hfont
              initialize r-titolo
              inspect cli-piva 
                      replacing trailing spaces by low-value
              inspect cli-codfis   
                      replacing trailing spaces by low-value
              inspect cli-piva-cee 
                      replacing trailing spaces by low-value

              string "P.IVA: "          delimited size
                     cli-piva           delimited low-value
                     "  -  P.IVA CEE: " delimited size
                     cli-piva-cee       delimited low-value
                     "  -  C.F.: "      delimited size
                     cli-codfis         delimited low-value
                     into r-titolo
              end-string
              move r-titolo to spl-riga-stampa
              perform SCRIVI

              move 19,3         to save-riga
              move Calibri12BI  to spl-hfont
              move "IVA ESENTE" to spl-riga-stampa
              perform SCRIVI

              move 19,8       to save-riga
              move Calibri12B to spl-hfont
              initialize r-titolo
              string cli-iva-esente     delimited size
                     " - "              delimited size
                     iva-descrizione    delimited size
                     into r-titolo
              end-string
              move r-titolo to spl-riga-stampa
              perform SCRIVI

              move 20,8        to save-riga
              move Calibri12BI to spl-hfont
              move "SCONTI %"  to spl-riga-stampa
              perform SCRIVI

              move cli-sconto1 to sconto1-ed
      *****        move cli-sconto2 to sconto2-ed
              move 21,3        to save-riga
              move Calibri12B  to spl-hfont
              initialize r-titolo
              string sconto1-ed    delimited size
      *****               "   + "       delimited size
      *****               sconto2-ed    delimited size
                     into r-titolo
              end-string
              move r-titolo to spl-riga-stampa
              perform SCRIVI

              set spl-chiusura to true
              call   "spooler" using spooler-link

           end-if.

      ***---
       STAMPA-BITMAP.
           |IMMAGINE
           move 0 to BitmapClienteHandle.
           call "W$BITMAP" using WBITMAP-LOAD, cli-fotografia,
                          giving BitmapClienteHandle.
           if BitmapClienteHandle <= 0
              accept cli-fotografia from environment "BMP_CONTACT"
              CALL "w$bitmap" USING WBITMAP-LOAD, cli-fotografia, 
                             GIVING BitmapClienteHandle
           end-if.

           initialize spooler-link.
           set spl-bitmap to true.
           move 37,2 to spl-colonna.
           move 14,0 to spl-riga.
           move BitmapClienteHandle   to spl-hbitmap.

           move 4,2 to spl-bitmap-height.
           move 4,8 to spl-bitmap-width.

           call "spooler" using spooler-link.

           |FRAME
           move 6     to spl-pen-width.

           move 4,50 to spl-riga.
           add  4,35 to spl-riga giving spl-riga-fine.

           move 7,55   to spl-colonna.
           move 12,55  to spl-colonna-fine.

           set  spl-oggetto       to true.
           set  spl-rettangolo    to true.
           set  spl-brush-null    to true.
           set  spl-nero          to true.
           call "spooler"         using spooler-link.

      ***---
       STAMPA-FRAMES.
           move 3,3   to save-riga.
           
           move save-riga to spl-riga.
           add 0,5 to spl-riga giving spl-riga-fine.

           move 1,5   to spl-colonna.
           move 18,5  to spl-colonna-fine.

           set  spl-oggetto       to true.
           set  spl-rettangolo    to true.
           set  spl-brush-null    to true.
           set  spl-nero          to true.
           call "spooler"         using spooler-link.

           add 5,7 to spl-riga-fine giving spl-riga.
           add 0,5 to spl-riga      giving spl-riga-fine.

           move 3,0   to spl-colonna.
           move 17,0  to spl-colonna-fine.

           set  spl-oggetto       to true.
           set  spl-rettangolo    to true.
           set  spl-brush-null    to true.
           set  spl-nero          to true.
           call "spooler"         using spooler-link.

           add 0,0 to spl-riga-fine giving spl-riga.
           add 0,5 to spl-riga      giving spl-riga-fine.

           move 3,0   to spl-colonna.
           move 17,0  to spl-colonna-fine.

           set  spl-oggetto       to true.
           set  spl-rettangolo    to true.
           set  spl-brush-null    to true.
           set  spl-nero          to true.
           call "spooler"         using spooler-link.

           add 0,5 to spl-riga-fine giving spl-riga.
           add 0,5 to spl-riga      giving spl-riga-fine.

           move 3,0   to spl-colonna.
           move 17,0  to spl-colonna-fine.

           set  spl-oggetto       to true.
           set  spl-rettangolo    to true.
           set  spl-brush-null    to true.
           set  spl-nero          to true.
           call "spooler"         using spooler-link.

           add 0,0 to spl-riga-fine giving spl-riga.
           add 0,5 to spl-riga      giving spl-riga-fine.

           move 3,0   to spl-colonna.
           move 17,0  to spl-colonna-fine.

           set  spl-oggetto       to true.
           set  spl-rettangolo    to true.
           set  spl-brush-null    to true.
           set  spl-nero          to true.
           call "spooler"         using spooler-link.

           add 0,5 to spl-riga-fine giving spl-riga.
           add 0,5 to spl-riga      giving spl-riga-fine.

           move  3,0  to spl-colonna.
           move  7,0  to spl-colonna-fine.

           set  spl-oggetto       to true.
           set  spl-rettangolo    to true.
           set  spl-brush-null    to true.
           set  spl-nero          to true.
           call "spooler"         using spooler-link.

           move   8,0  to spl-colonna.
           move  12,0  to spl-colonna-fine.

           set  spl-oggetto       to true.
           set  spl-rettangolo    to true.
           set  spl-brush-null    to true.
           set  spl-nero          to true.
           call "spooler"         using spooler-link.

           move 13,0  to spl-colonna.
           move 17,0  to spl-colonna-fine.

           set  spl-oggetto       to true.
           set  spl-rettangolo    to true.
           set  spl-brush-null    to true.
           set  spl-nero          to true.
           call "spooler"         using spooler-link.

           add 0,0 to spl-riga-fine giving spl-riga.
           add 0,5 to spl-riga      giving spl-riga-fine.

           move  3,0  to spl-colonna.
           move  7,0  to spl-colonna-fine.

           set  spl-oggetto       to true.
           set  spl-rettangolo    to true.
           set  spl-brush-null    to true.
           set  spl-nero          to true.
           call "spooler"         using spooler-link.

           move   8,0  to spl-colonna.
           move  12,0  to spl-colonna-fine.

           set  spl-oggetto       to true.
           set  spl-rettangolo    to true.
           set  spl-brush-null    to true.
           set  spl-nero          to true.
           call "spooler"         using spooler-link.

           move 13,0  to spl-colonna.
           move 17,0  to spl-colonna-fine.

           set  spl-oggetto       to true.
           set  spl-rettangolo    to true.
           set  spl-brush-null    to true.
           set  spl-nero          to true.
           call "spooler"         using spooler-link.

           add 0,5 to spl-riga-fine giving spl-riga.
           add 0,5 to spl-riga      giving spl-riga-fine.

           move 3,0   to spl-colonna.
           move 17,0  to spl-colonna-fine.

           set  spl-oggetto       to true.
           set  spl-rettangolo    to true.
           set  spl-brush-null    to true.
           set  spl-nero          to true.
           call "spooler"         using spooler-link.

           add 0,0 to spl-riga-fine giving spl-riga.
           add 0,5 to spl-riga      giving spl-riga-fine.

           move 3,0   to spl-colonna.
           move 17,0  to spl-colonna-fine.

           set  spl-oggetto       to true.
           set  spl-rettangolo    to true.
           set  spl-brush-null    to true.
           set  spl-nero          to true.
           call "spooler"         using spooler-link.

           add 0,5 to spl-riga-fine giving spl-riga.
           add 0,5 to spl-riga      giving spl-riga-fine.

           move 3,0   to spl-colonna.
           move 17,0  to spl-colonna-fine.

           set  spl-oggetto       to true.
           set  spl-rettangolo    to true.
           set  spl-brush-null    to true.
           set  spl-nero          to true.
           call "spooler"         using spooler-link.

           add 0,0 to spl-riga-fine giving spl-riga.
           add 0,5 to spl-riga      giving spl-riga-fine.

           move 3,0   to spl-colonna.
           move 17,0  to spl-colonna-fine.

           set  spl-oggetto       to true.
           set  spl-rettangolo    to true.
           set  spl-brush-null    to true.
           set  spl-nero          to true.
           call "spooler"         using spooler-link.

           add 0,5 to spl-riga-fine giving spl-riga.
           add 0,5 to spl-riga      giving spl-riga-fine.

           move 3,0   to spl-colonna.
           move 17,0  to spl-colonna-fine.

           set  spl-oggetto       to true.
           set  spl-rettangolo    to true.
           set  spl-brush-null    to true.
           set  spl-nero          to true.
           call "spooler"         using spooler-link.

           add 0,0 to spl-riga-fine giving spl-riga.
           add 0,5 to spl-riga      giving spl-riga-fine.

           move 3,0   to spl-colonna.
           move 17,0  to spl-colonna-fine.

           set  spl-oggetto       to true.
           set  spl-rettangolo    to true.
           set  spl-brush-null    to true.
           set  spl-nero          to true.
           call "spooler"         using spooler-link.

           add 0,5 to spl-riga-fine giving spl-riga.
           add 0,5 to spl-riga      giving spl-riga-fine.

           move 3,0   to spl-colonna.
           move 17,0  to spl-colonna-fine.

           set  spl-oggetto       to true.
           set  spl-rettangolo    to true.
           set  spl-brush-null    to true.
           set  spl-nero          to true.
           call "spooler"         using spooler-link.

           add 0,0 to spl-riga-fine giving spl-riga.
           add 0,5 to spl-riga      giving spl-riga-fine.

           move 3,0   to spl-colonna.
           move 17,0  to spl-colonna-fine.

           set  spl-oggetto       to true.
           set  spl-rettangolo    to true.
           set  spl-brush-null    to true.
           set  spl-nero          to true.
           call "spooler"         using spooler-link.

           add 0,5 to spl-riga-fine giving spl-riga.
           add 0,5 to spl-riga      giving spl-riga-fine.

           move 3,0   to spl-colonna.
           move 17,0  to spl-colonna-fine.

           set  spl-oggetto       to true.
           set  spl-rettangolo    to true.
           set  spl-brush-null    to true.
           set  spl-nero          to true.
           call "spooler"         using spooler-link.

           add 0,0 to spl-riga-fine giving spl-riga.
           add 0,5 to spl-riga      giving spl-riga-fine.

           move 3,0   to spl-colonna.
           move 17,0  to spl-colonna-fine.

           set  spl-oggetto       to true.
           set  spl-rettangolo    to true.
           set  spl-brush-null    to true.
           set  spl-nero          to true.
           call "spooler"         using spooler-link.

           add 0,5 to spl-riga-fine giving spl-riga.
           add 0,5 to spl-riga      giving spl-riga-fine.

           move 3,0   to spl-colonna.
           move 17,0  to spl-colonna-fine.

           set  spl-oggetto       to true.
           set  spl-rettangolo    to true.
           set  spl-brush-null    to true.
           set  spl-nero          to true.
           call "spooler"         using spooler-link.

           add 0,0 to spl-riga-fine giving spl-riga.
           add 0,5 to spl-riga      giving spl-riga-fine.

           move 3,0   to spl-colonna.
           move 17,0  to spl-colonna-fine.

           set  spl-oggetto       to true.
           set  spl-rettangolo    to true.
           set  spl-brush-null    to true.
           set  spl-nero          to true.
           call "spooler"         using spooler-link.

      * QUADRATI RIEMPITIVI
           move 9,52 to save-riga.

           add 0,0  to save-riga giving spl-riga.
           add 0,46 to spl-riga  giving spl-riga-fine.
      
           move 3,02  to spl-colonna.
           move 16,98 to spl-colonna-fine.
      
           set  spl-oggetto       to true.
           set  spl-rettangolo    to true.
           set  spl-brush-ltgray  to true.
           set  spl-nero          to true.
           call "spooler"         using spooler-link. 

           add 1,5  to save-riga giving spl-riga.
           add 0,46 to spl-riga  giving spl-riga-fine.
      
           move 3,02  to spl-colonna.
           move 16,98 to spl-colonna-fine.
      
           set  spl-oggetto       to true.
           set  spl-rettangolo    to true.
           set  spl-brush-ltgray  to true.
           set  spl-nero          to true.
           call "spooler"         using spooler-link.

           add 1,04 to spl-riga-fine giving spl-riga.
           add 0,46 to spl-riga      giving spl-riga-fine.
      
           move  3,02 to spl-colonna.
           move  6,98 to spl-colonna-fine.
      
           set  spl-oggetto       to true.
           set  spl-rettangolo    to true.
           set  spl-brush-ltgray  to true.
           set  spl-nero          to true.
           call "spooler"         using spooler-link.
      
           move  8,02 to spl-colonna.
           move 11,98 to spl-colonna-fine.
      
           set  spl-oggetto       to true.
           set  spl-rettangolo    to true.
           set  spl-brush-ltgray  to true.
           set  spl-nero          to true.
           call "spooler"         using spooler-link.
      
           move 13,02 to spl-colonna.
           move 16,98 to spl-colonna-fine.
      
           set  spl-oggetto       to true.
           set  spl-rettangolo    to true.
           set  spl-brush-ltgray  to true.
           set  spl-nero          to true.
           call "spooler"         using spooler-link.

           add 1,04 to spl-riga-fine giving spl-riga.
           add 0,46 to spl-riga      giving spl-riga-fine.
      
           move 3,02  to spl-colonna.
           move 16,98 to spl-colonna-fine.
      
           set  spl-oggetto       to true.
           set  spl-rettangolo    to true.
           set  spl-brush-ltgray  to true.
           set  spl-nero          to true.
           call "spooler"         using spooler-link. 

           add 1,04 to spl-riga-fine giving spl-riga.
           add 0,46 to spl-riga      giving spl-riga-fine.
      
           move 3,02  to spl-colonna.
           move 16,98 to spl-colonna-fine.
      
           set  spl-oggetto       to true.
           set  spl-rettangolo    to true.
           set  spl-brush-ltgray  to true.
           set  spl-nero          to true.
           call "spooler"         using spooler-link.

           add 1,04 to spl-riga-fine giving spl-riga.
           add 0,46 to spl-riga      giving spl-riga-fine.
      
           move 3,02  to spl-colonna.
           move 16,98 to spl-colonna-fine.
      
           set  spl-oggetto       to true.
           set  spl-rettangolo    to true.
           set  spl-brush-ltgray  to true.
           set  spl-nero          to true.
           call "spooler"         using spooler-link.

           add 1,04 to spl-riga-fine giving spl-riga.
           add 0,46 to spl-riga      giving spl-riga-fine.
      
           move 3,02  to spl-colonna.
           move 16,98 to spl-colonna-fine.
      
           set  spl-oggetto       to true.
           set  spl-rettangolo    to true.
           set  spl-brush-ltgray  to true.
           set  spl-nero          to true.
           call "spooler"         using spooler-link.

           add 1,04 to spl-riga-fine giving spl-riga.
           add 0,46 to spl-riga      giving spl-riga-fine.
      
           move 3,02  to spl-colonna.
           move 16,98 to spl-colonna-fine.
      
           set  spl-oggetto       to true.
           set  spl-rettangolo    to true.
           set  spl-brush-ltgray  to true.
           set  spl-nero          to true.
           call "spooler"         using spooler-link.

           add 1,04 to spl-riga-fine giving spl-riga.
           add 0,46 to spl-riga      giving spl-riga-fine.
      
           move 3,02  to spl-colonna.
           move 16,98 to spl-colonna-fine.
      
           set  spl-oggetto       to true.
           set  spl-rettangolo    to true.
           set  spl-brush-ltgray  to true.
           set  spl-nero          to true.
           call "spooler"         using spooler-link.
            
      ***---
       STAMPA-LINEA.
           move 8                  to spl-pen-width.
           move 1,5                to spl-colonna.
           move 18,5               to spl-colonna-fine.
           move 23,0               to spl-riga spl-riga-fine.
           set  spl-oggetto        to true.
           set  spl-linea          to true.
           set  spl-pen-solid      to true.
           set  spl-nero           to true.
           call "spooler"       using spooler-link.

      ***---
       SCRIVI.
           add  passo         to save-riga.
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

      * Calibri 14B
           initialize wfont-data Calibri14B.
           move 14 to wfont-size.
           move "Calibri"            to wfont-name.
           set  wfcharset-dont-care  to true.
           set  wfont-bold           to true.
           set  wfont-italic         to false.
           set  wfont-underline      to false.
           set  wfont-strikeout      to false.
           set  wfont-fixed-pitch    to false.
           move 0                    to wfont-char-set.
           set  wfdevice-win-printer to true. |E' un carattere per la stampante
           call "W$FONT" using wfont-get-font, Calibri14B, wfont-data
                        giving wfont-status.

      * ISACCO (QUESTI TEST CONTROLLANO L'ESISTENZA DEL FONT)
           if wfont-status not = 1
              set errori to true
              perform MESSAGGIO-ERR-FONT
              exit paragraph
           end-if.

      * Calibri 12BI
           initialize wfont-data Calibri12BI.
           move 12 to wfont-size.
           move "Calibri"            to wfont-name.
           set  wfcharset-dont-care  to true.
           set  wfont-bold           to true.
           set  wfont-italic         to true.
           set  wfont-underline      to false.
           set  wfont-strikeout      to false.
           set  wfont-fixed-pitch    to false.
           move 0                    to wfont-char-set.
           set  wfdevice-win-printer to true. |E' un carattere per la stampante
           call "W$FONT" using wfont-get-font, Calibri12BI, wfont-data
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
           close clienti tipocli codiva.

      ***---
       EXIT-PGM.
           set environment "PRINTER" to "-P SPOOLER-DIRECT".

           destroy Calibri20BI.
           destroy Calibri14B.
           destroy Calibri12BI.
           destroy Calibri12B.
           destroy Calibri8.

           cancel "spooler".
           initialize spooler-link.
           goback.
