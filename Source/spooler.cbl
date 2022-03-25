       program-id.     spooler.
       remarks. Questa versione di spooler è un pò particolare in quanto
                utilizza la proprietà TRANSPARENT per la jpg la quale
                però, basandosi sul settaggio delle colonne, mi varia la
                logica del programma che mi permetterà di stampare solo
                righe intere e lineari.
                'IMPORTANTE!!!' Il nome del job di stampa passato in 
                linkage nella variabile "spl-nome-job" può contenere
                al massimo 29 caratteri (non uno di più) altrimenti in
                fase di apertura con WINPRINT-SET-PRINTER va in crash 
                (Questo si verifica con Run-Time 6.0).

       SPECIAL-NAMES.
           DECIMAL-POINT IS COMMA.

       file-control.
           COPY "STAMPA.SL".
       file section.
           COPY "STAMPA.FD".
      *************************
       working-storage section.
      *************************
           copy "acugui.def".
           copy "acucobol.def".
           copy "crtvars.def".
           copy "winprint.def".

       01  extend-stat.
           03 primary-error    pic x(2).
           03 secondary-error  pic x(10).
       01  text-message        pic x(40).

       01  stato-stampa pic 9.
           88 stampa-annullata value zero.
           88 stampa-ok        value 1.

       01  status-stampa pic x(2).

       01  result signed-short.

       01  como-altezza   pic 9(3)v99.
       01  como-larghezza pic 9(3)v99.
       01  dim-crt        pic 9(3)v99.
       01  nr-crt         pic 9(3).
       01  cont           pic 9(5).
       01  ind            pic 9(5).

       01  como-area      pic x(1000).

       01  passaggio      pic 9.
           88 primo-passaggio value 0.
           88 altro-passaggio value 1.

       77  calling-program          pic x(20).
       77  como-data                pic 9(8).
       77  mese                     pic 99.

       78  std-margine-inf          value 2.
       78  std-margine-destro       value 1.
       78  std-margine-sinistro     value 1.

       01  controlli         pic xx.
           88 errori         value "ER".
           88 tutto-ok       value "OK".

       LINKAGE SECTION.
           COPY "SPOOLER.DEF".
         
      ************************************************************************
       procedure division using SPOOLER-LINK.
      ************************************************************************
       DECLARATIVES.
       STAMPA-ERR SECTION.
           use after error procedure on STAMPA.
           set tutto-ok  to true.
           evaluate status-stampa
           when "30" |Permanent Error!!!
                call "C$RERR" using extend-stat, text-message
                display message "Trasmission Error on Windows Spooler."
                      x"0d0a""Riavviare il sistema GESLUX e riprovare."
                      x"0d0a""Codice d'errore: " extend-stat
                          title "spooler"
                           icon 2
                set spl-sta-annu  to true
                set errori to true
           end-evaluate.       
       END DECLARATIVES.
       
       MAIN-LOGIC.
           set tutto-ok to true.
           evaluate true
           when spl-apertura
           when spl-apertura-anteprima   perform APERTURA
           when spl-stringa              perform STAMPA-STRINGA
           when spl-oggetto              perform STAMPA-OGGETTO
           when spl-bitmap               perform STAMPA-BITMAP
           when spl-salto-pagina         perform SALTO-PAGINA
           when spl-chiusura             close   stampa
           end-evaluate.

           goback.

      ***---
       APERTURA.
           call "C$CALLEDBY"  using calling-program.
      *    SELEZIONO LA STAMPANTE
           if spl-apertura
              if spl-nome-stampante = space
                 call "WIN$PRINTER" using winprint-setup-old,
                                          winprint-selection
                                   giving result

                 if result = 1
                    call "WIN$PRINTER" using winprint-get-current-info, 
                                             winprint-selection
                                      giving result

                    perform ORIENTAMENTO-FOGLIO
                 end-if

              else

                 initialize winprint-selection            
                 move spl-nome-stampante to winprint-name
                 if SPL-NUM-COPIE = zero
                    move 1   to SPL-NUM-COPIE
                 end-if
                 move SPL-NUM-COPIE   to WINPRINT-COPIES
                 move SPL-NUM-COPIE   to winprint-curr-copies
                 call "WIN$PRINTER" using winprint-set-printer,
                                          winprint-selection
                                   giving result

                 if result = 1
                    call "WIN$PRINTER" using winprint-get-current-info, 
                                             winprint-selection
                                      giving result

                    perform ORIENTAMENTO-FOGLIO
                 end-if

              end-if

              if result not = 1
                 if spl-titolo-msgbox = spaces
                    move "spooler" to spl-titolo-msgbox
                 end-if
                 if spl-nome-stampante not = space
                    display message "Stampante non disponibile!"
                              title spl-titolo-msgbox 
                               icon mb-warning-icon
                 end-if
                 set spl-sta-annu to true
                 exit paragraph
              end-if

           else 
              accept spl-nome-stampante 
                          from environment "STAMPANTE_ANTEPRIMA"

              initialize winprint-selection            
              move spl-nome-stampante to winprint-name
              call "WIN$PRINTER" using winprint-set-printer,
                                       winprint-selection
                                giving result

              if result = 1
                 call "WIN$PRINTER" using winprint-get-current-info,
                                          winprint-selection
                                   giving result
                 perform ORIENTAMENTO-FOGLIO
              end-if
                                         
              if result not = 1
                 if spl-titolo-msgbox = spaces
                    move "spooler" to spl-titolo-msgbox
                 end-if
                 display message "Anteprima non disponibile"
                           title spl-titolo-msgbox 
                            icon mb-warning-icon
                 set spl-sta-annu to true
                 exit paragraph
              end-if
           end-if.

      *    recupero le dimensioni del foglio
           perform CALCOLA-DIMENSIONI.
      *    setto il nome del job di stampa
           move spl-nome-job to winprint-job-title.
           call "WIN$PRINTER" using winprint-set-printer-ex,  
                                    winprint-selection
                             giving result.

           open output stampa.
           if tutto-ok
      *       setto la posizione in modo da avere sempre il posizionamento in centimetri
              initialize wprtdata-draw
              move 1                        to wprtdata-draw-start-y
              move 1                        to wprtdata-draw-start-x
              move zero                     to wprtdata-draw-stop-x
              move zero                     to wprtdata-draw-stop-y
              move wprtunits-centimeters    to wprtdata-draw-units
              move zero                     to wprtdata-draw-shape
              call "WIN$PRINTER" using winprint-set-cursor, 
                                       winprint-data
                                giving result
           end-if.

      ***---
       ORIENTAMENTO-FOGLIO.
           if spl-horizontal
              move wprtsel-orient-landscape  
                to winprint-curr-orientation  
           else              
              move wprtsel-orient-portrait
                to winprint-curr-orientation  
           end-if.
           call "WIN$PRINTER" using winprint-set-printer,
                                    winprint-selection
                             giving result.

      ***---
       STAMPA-STRINGA.
           evaluate calling-program 
           when "st-cli-det"
                perform SETTA-COLONNE-DETTAGLIO-CLIENTE
           when "st-schedacli"
                perform SETTA-COLONNE-SCHEDA-CLIENTE
           when "st-docum"
                perform SETTA-COLONNE-DOCUM
           end-evaluate.

      *    SETTO IL FONT
           initialize winprint-data.
           move spl-hfont        to wprtdata-font.
           call "WIN$PRINTER" using winprint-set-font, 
                                    winprint-data
                             giving result.
      *
           perform CALCOLA-CRT-RIGA.

      *    SETTO LA POSIZIONE 
           initialize wprtdata-draw.
           move spl-colonna              to wprtdata-draw-start-x.
           move spl-riga                 to wprtdata-draw-start-y.
           move zero                     to wprtdata-draw-stop-x.
           move zero                     to wprtdata-draw-stop-y.
           move wprtunits-centimeters to wprtdata-draw-units.
           move zero                     to wprtdata-draw-shape.

           call "WIN$PRINTER" using winprint-set-cursor, 
                                    winprint-data
                             giving result.

      *    setto il colore
      *    Formula per usare un colore particolare:
      *    compute colorref            =
      *            (rgb-red)           +
      *            (rgb-green * 256)   +
      *            (rgb-blue  * 65536).

           move spl-pen-color   to WPRTDATA-TEXT-COLOR

           CALL "WIN$PRINTER" USING  WINPRINT-SET-TEXT-COLOR, 
                                     WINPRINT-DATA
                              GIVING RESULT

      *     INSPECT SPL-RIGA-STAMPA REPLACING TRAILING SPACE BY LOW-VALUE.
      *
      *     MOVE ZERO TO CONT.
      *     INSPECT SPL-RIGA-STAMPA TALLYING CONT FOR CHARACTERS
      *          BEFORE LOW-VALUE.
      *
      *     INSPECT SPL-RIGA-STAMPA REPLACING TRAILING LOW-VALUE BY SPACE.
      *
      **    SCRIVO LA STRINGA
      *     SET PRIMO-PASSAGGIO TO TRUE.
      *     IF CONT < NR-CRT
              write stampa-rigo from spl-riga-stampa after 0
      *     ELSE 
      *        MOVE 1 TO IND
      *        PERFORM UNTIL IND > CONT
      *           IF PRIMO-PASSAGGIO
      *              SET ALTRO-PASSAGGIO TO TRUE               
      *           ELSE 
      *              ADD SPL-PASSO       TO SPL-RIGA     
      *              initialize wprtdata-draw
      *              MOVE SPL-COLONNA    TO WPRTDATA-DRAW-START-X
      *              MOVE SPL-RIGA       TO WPRTDATA-DRAW-START-Y
      *              MOVE ZERO           TO WPRTDATA-DRAW-STOP-X
      *              MOVE ZERO           TO WPRTDATA-DRAW-STOP-Y
      *              MOVE WPRTUNITS-CENTIMETERS
      *                                  TO WPRTDATA-DRAW-UNITS
      *              MOVE ZERO           TO WPRTDATA-DRAW-SHAPE
      *
      *              CALL "WIN$PRINTER" USING  WINPRINT-SET-CURSOR, 
      *                                        WINPRINT-DATA
      *                                 GIVING RESULT
      *           END-IF
      *           INITIALIZE COMO-AREA
      *           MOVE SPL-RIGA-STAMPA(IND:NR-CRT)   TO COMO-AREA
      *           WRITE STAMPA-RIGO 
      *                       FROM COMO-AREA(1:NR-CRT) AFTER 0
      *           ADD NR-CRT     TO IND
      *        END-PERFORM
      *     END-IF.
           call "WIN$PRINTER" using winprint-clear-page-columns.
           call "WIN$PRINTER" using winprint-clear-data-columns.

      ***---
       STAMPA-OGGETTO.
           move spl-pen        to wprtdata-pen-style.
           move spl-pen-width  to wprtdata-pen-width.
           move spl-pen-color  to wprtdata-pen-color.

           call "WIN$PRINTER" using winprint-graph-pen, 
                                    winprint-data
                             giving result.

      *    SETTO LO STILE DELL'OGGETTO
           move spl-stile to wprtdata-brush-style.

           move spl-pen-color  to WPRTDATA-BRUSH-COLOR


           call "WIN$PRINTER" using winprint-graph-brush, 
                                    winprint-data
                             giving result.



      *    STAMPO L'OGGETTO.
           initialize wprtdata-draw.
           move spl-colonna           to wprtdata-draw-start-x.
           move spl-riga              to wprtdata-draw-start-y.
           move spl-colonna-fine      to wprtdata-draw-stop-x.
           move spl-riga-fine         to wprtdata-draw-stop-y.
           move wprtunits-centimeters to wprtdata-draw-units.
           move spl-tipo-oggetto      to wprtdata-draw-shape.

           call "WIN$PRINTER" using winprint-graph-draw, 
                                    winprint-data
                             giving result.

      ***---
       STAMPA-BITMAP.
           initialize     wprtdata-print-bitmap.
      *    setto la bitmap
           move spl-hbitmap                   to wprtdata-bitmap.
           move spl-riga                      to wprtdata-bitmap-row.
           move spl-colonna                   to wprtdata-bitmap-col.
           move spl-bitmap-height             to wprtdata-bitmap-height.
           move spl-bitmap-width              to wprtdata-bitmap-width.
           move wprtbitmap-scale-centimeters  to wprtdata-bitmap-flags.
      *    stampo la bitmap
           call "WIN$PRINTER" using winprint-print-bitmap, 
                                    winprint-data
                             giving result.

      ***---
       SALTO-PAGINA.
           write stampa-rigo from spaces after page.

      ***---
       CALCOLA-DIMENSIONI.
           if spl-margine-inf = zero
              move std-margine-inf     to spl-margine-inf
           end-if.
           if spl-margine-destro = zero
              move std-margine-destro  to spl-margine-destro
           end-if.
           if spl-margine-sinistro = zero
              move std-margine-sinistro     to spl-margine-sinistro
           end-if.

           call "WIN$PRINTER" using winprint-get-current-info-ex, 
                                    winprint-selection
                             giving result.

           evaluate winprint-curr-papersize
           when 8 |formato a3
                if winprint-curr-orientation = 1|verticale
                   move 42   to como-altezza
                   move 29,7 to como-larghezza
                else 
                   move 29,7 to como-altezza
                   move 42   to como-larghezza
                end-if
           when 9 |formato a4
           when other
                if winprint-curr-orientation = 1|verticale
                   move 29,7 to como-altezza
                   move 21   to como-larghezza
                else
                   move 21   to como-altezza
                   move 29,7 to como-larghezza
                end-if
           end-evaluate.

           subtract spl-margine-inf from como-altezza.
           compute como-larghezza = como-larghezza - spl-margine-destro 
                                    - spl-margine-sinistro

           move como-larghezza to spl-larghezza.
           move como-altezza   to spl-altezza.

      ***---
       CALCOLA-CRT-RIGA.
           call "WIN$PRINTER" using winprint-get-page-layout, 
                                    winprint-data,
                             giving result.
           |WPRTDATA-COLUMNS-PER-PAGE

           compute dim-crt = como-larghezza / wprtdata-columns-per-page.

           compute nr-crt = spl-colonna / dim-crt.

           compute nr-crt = wprtdata-columns-per-page - nr-crt.

      ***--- 
       PULISCI-COLONNE.
           call "WIN$PRINTER" using winprint-clear-data-columns
                             giving result.

           call "WIN$PRINTER" using winprint-clear-page-columns,
                             giving result.

      ***---
       SETTA-COLONNE.
           perform PULISCI-COLONNE
           initialize winprint-column
           move spl-hfont             to winprint-col-font
           move wprtunits-centimeters to winprint-col-units     
           move wprtalign-none        to winprint-col-alignment
           set  winprint-transparent  to true
           evaluate spl-tipo-colonna
           when 1
                call "WIN$PRINTER"  using winprint-set-data-columns,
                                          41
                                   giving return-code
                                   
                move 0,2               to winprint-col-start
                call "WIN$PRINTER"  using winprint-set-page-column, 
                                          winprint-column             
                                   giving return-code
                                   
                move 10,2              to winprint-col-start
                call "WIN$PRINTER"  using winprint-set-page-column, 
                                          winprint-column
                                   giving return-code

           when 2                 
                call "WIN$PRINTER"  using winprint-set-data-columns,
                                     |5, 30,  2,  5, 30,  2
                                          6, 36, 38, 43, 73
                                   giving return-code
                                   
                move 0,2               to winprint-col-start
                call "WIN$PRINTER"  using winprint-set-page-column, 
                                          winprint-column             
                                   giving return-code
                                   
                move 1,5            to    winprint-col-start
                call "WIN$PRINTER"  using winprint-set-page-column, 
                                          winprint-column
                                   giving return-code
                                   
                move 9              to    winprint-col-start
                call "WIN$PRINTER"  using winprint-set-page-column, 
                                          winprint-column
                                   giving return-code
                                   
                move 10,2           to    winprint-col-start
                call "WIN$PRINTER"  using winprint-set-page-column, 
                                          winprint-column
                                   giving return-code
                                   
                move 11,5           to    winprint-col-start
                call "WIN$PRINTER"  using winprint-set-page-column, 
                                          winprint-column
                                   giving return-code
                                   
                move 19             to    winprint-col-start
                call "WIN$PRINTER"  using winprint-set-page-column, 
                                          winprint-column
                                   giving return-code
           when 3                 
                call "WIN$PRINTER"  using winprint-set-data-columns,
                                     | 8, 8,  30,  5,  5
                                          9, 17, 44, 49
                                   giving return-code 
                                   
                move 0,5             to    winprint-col-start
                call "WIN$PRINTER"   using winprint-set-page-column, 
                                           winprint-column
                                    giving return-code
                                   
                move 3,7             to    winprint-col-start
                call "WIN$PRINTER"   using winprint-set-page-column, 
                                           winprint-column
                                    giving return-code
                                   
                move 6,1             to    winprint-col-start
                call "WIN$PRINTER"   using winprint-set-page-column, 
                                           winprint-column
                                    giving return-code
                                   
                move 14              to    winprint-col-start
                call "WIN$PRINTER"   using winprint-set-page-column, 
                                           winprint-column
                                    giving return-code
                                   
                move 16,5            to    winprint-col-start
                call "WIN$PRINTER"   using winprint-set-page-column, 
                                           winprint-column
                                    giving return-code
           when 4      
                call "win$printer"  using winprint-set-data-columns,
                                     | 10, 8,  8,  8, 11, 16
                                          11, 19, 27, 35, 46
                                   giving return-code
                                   
                move 0,5             to    winprint-col-start
                call "WIN$PRINTER"   using winprint-set-page-column, 
                                           winprint-column
                                    giving return-code
                                   
                move 3,7              to   winprint-col-start
                call "WIN$PRINTER"   using winprint-set-page-column, 
                                           winprint-column
                                    giving return-code
                                   
                move 6,3              to   winprint-col-start
                call "WIN$PRINTER"   using winprint-set-page-column, 
                                           winprint-column
                                    giving return-code
                                   
                move 9,2                 to  winprint-col-start
                call "WIN$PRINTER"   using winprint-set-page-column, 
                                           winprint-column
                                    giving return-code
                                   
                move 11,8            to   winprint-col-start
                call "WIN$PRINTER"   using winprint-set-page-column, 
                                           winprint-column
                                    giving return-code
                                   
                move 14,5            to    winprint-col-start
                call "WIN$PRINTER"   using winprint-set-page-column, 
                                           winprint-column
                                    giving return-code
           when 5       
                call "win$printer"  using winprint-set-data-columns,
                                      |40, 5
                                          41              
                                   giving return-code
                                   
                move 0,5             to    winprint-col-start
                call "WIN$PRINTER"   using winprint-set-page-column, 
                                           winprint-column
                                    giving return-code
                                   
                move 18              to    winprint-col-start
                call "WIN$PRINTER"   using winprint-set-page-column, 
                                           winprint-column
                                    giving return-code

           when 6                                                       
                call "WIN$PRINTER"  using winprint-set-data-columns,
                                     |35, 35
                                          36              
                                    giving return-code
                                   
                move 0,5             to    winprint-col-start
                call "WIN$PRINTER"   using winprint-set-page-column, 
                                           winprint-column
                                    giving return-code
                                   
                move 10              to    winprint-col-start
                call "WIN$PRINTER"   using winprint-set-page-column, 
                                           winprint-column
                                    giving return-code
           end-evaluate.

      ***---
       SETTA-COLONNE-DOCUM.
           perform PULISCI-COLONNE.
           initialize winprint-column.
           move spl-hfont             to winprint-col-font.
           move wprtunits-centimeters to winprint-col-units.
           move wprtalign-none        to winprint-col-alignment.
           set  winprint-transparent  to true.
           evaluate spl-tipo-colonna
           when 1    

                call "WIN$PRINTER"  using winprint-set-data-columns,
                                           13, 23, 43, 73
                                    giving return-code
                move 0,8               to winprint-col-start
                move wprtalign-center  to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code
                move 3,85              to winprint-col-start
                move wprtalign-center  to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code
                move 6,5               to winprint-col-start
                move wprtalign-center  to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code
                move 12,9              to winprint-col-start
                move wprtalign-center  to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code
                move 20,0              to winprint-col-start
                move wprtalign-center  to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code
           when 1,5

                call "WIN$PRINTER"  using winprint-set-data-columns,
                                           50
                                    giving return-code
                move 13,1              to winprint-col-start
                move wprtalign-left    to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code
           when 2
                call "WIN$PRINTER"  using winprint-set-data-columns,
                                          7, 57 
                                    giving return-code
                move 0,75              to winprint-col-start
                move wprtalign-left    to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code 
                move 2,80              to winprint-col-start
                move wprtalign-left    to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code
                move 10,95             to winprint-col-start
                move wprtalign-right   to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code |TAPPO

           when 2,5                         
                call "WIN$PRINTER"  using winprint-set-data-columns,  
                                          6, 15, 17, 23, 25, 34, 36, 39
                                    |10, 12, 17, 26, 28, 37, 39, 41, 43
                |QTA
                move 12,0              to winprint-col-start
                move wprtalign-right   to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code               
                |PREZZO
                move 12,7              to winprint-col-start
                move wprtalign-right   to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code               
                |€
                move 14,6              to winprint-col-start
                move wprtalign-right   to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code      
                |SCONTO
                move 14,85             to winprint-col-start
                move wprtalign-right   to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code
                |%
                move 16,30             to winprint-col-start
                move wprtalign-right   to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code
                |IMPONIBILE
                move 16,65             to winprint-col-start
                move wprtalign-right   to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code
                |€
                move 18,7              to winprint-col-start
                move wprtalign-right   to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code
                |IVA
                move 18,95             to winprint-col-start
                move wprtalign-right   to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code
                |TAPPO
                move 19,93             to winprint-col-start
                move wprtalign-right   to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code
           when 3 
                call "WIN$PRINTER"  using winprint-set-data-columns,
                                     11, 13, 23, 25, 35, 37, 47, 49
                                    giving return-code
                move 1,4               to winprint-col-start
                move wprtalign-center  to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code 
                move 4,6               to winprint-col-start
                move wprtalign-left    to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code 
                move 6,0               to winprint-col-start
                move wprtalign-center  to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code
                move 8,25              to winprint-col-start
                move wprtalign-left    to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code
                move 9,6               to winprint-col-start
                move wprtalign-center  to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code
                move 12,0              to winprint-col-start
                move wprtalign-left    to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code
                move 15,0              to winprint-col-start
                move wprtalign-center  to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code
                move 18,1              to winprint-col-start
                move wprtalign-left    to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code

           end-evaluate.
            
      ***---
       SETTA-COLONNE-SCHEDA-CLIENTE.
           perform PULISCI-COLONNE.
           initialize winprint-column.
           move spl-hfont             to winprint-col-font.
           move wprtunits-centimeters to winprint-col-units.
           move wprtalign-none        to winprint-col-alignment.
           set  winprint-transparent  to true.
           evaluate spl-tipo-colonna
           when 1
                call "WIN$PRINTER"  using winprint-set-data-columns,
                                           101
                                    giving return-code
                move 1,5               to winprint-col-start
                move wprtalign-center  to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code
                move 19,0              to winprint-col-start
                move wprtalign-right   to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code
           when 2
                call "WIN$PRINTER"  using winprint-set-data-columns,
                                           11, 17, 67, 72, 80, 82
                                    giving return-code
                move 1,5               to winprint-col-start
                move wprtalign-center  to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code 
                move 3,8               to winprint-col-start
                move 0,1               to winprint-col-indent
                move wprtalign-left    to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code
                move 5,6               to winprint-col-start
                move wprtalign-left    to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code
                move 16,1              to winprint-col-start
                move wprtalign-right   to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code
                move 16,9              to winprint-col-start
                move wprtalign-right   to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code
                move 18,6              to winprint-col-start
                move wprtalign-right   to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code
                move 18,9              to winprint-col-start
                move wprtalign-right   to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code
           end-evaluate.
      ***---
       SETTA-COLONNE-DETTAGLIO-CLIENTE.
           perform PULISCI-COLONNE.
           initialize winprint-column.
           move spl-hfont             to winprint-col-font.
           move wprtunits-centimeters to winprint-col-units.
           move wprtalign-none        to winprint-col-alignment.
           set  winprint-transparent  to true.
           evaluate spl-tipo-colonna
           when 1
                call "WIN$PRINTER"  using winprint-set-data-columns,
                                          41
                                   giving return-code
                                   
                move 0,2               to winprint-col-start
                call "WIN$PRINTER"  using winprint-set-page-column, 
                                          winprint-column             
                                   giving return-code
                                   
                move 10,2              to winprint-col-start
                call "WIN$PRINTER"  using winprint-set-page-column, 
                                          winprint-column
                                   giving return-code

           when 2                 
                call "WIN$PRINTER"  using winprint-set-data-columns,
                                     |5, 30,  2,  5, 30,  2
                                          6, 36, 38, 43, 73
                                   giving return-code
                                   
                move 0,2               to winprint-col-start
                call "WIN$PRINTER"  using winprint-set-page-column, 
                                          winprint-column             
                                   giving return-code
                                   
                move 1,5            to    winprint-col-start
                call "WIN$PRINTER"  using winprint-set-page-column, 
                                          winprint-column
                                   giving return-code
                                   
                move 9              to    winprint-col-start
                call "WIN$PRINTER"  using winprint-set-page-column, 
                                          winprint-column
                                   giving return-code
                                   
                move 10,2           to    winprint-col-start
                call "WIN$PRINTER"  using winprint-set-page-column, 
                                          winprint-column
                                   giving return-code
                                   
                move 11,5           to    winprint-col-start
                call "WIN$PRINTER"  using winprint-set-page-column, 
                                          winprint-column
                                   giving return-code
                                   
                move 19             to    winprint-col-start
                call "WIN$PRINTER"  using winprint-set-page-column, 
                                          winprint-column
                                   giving return-code
           when 3                 
                call "WIN$PRINTER"  using winprint-set-data-columns,
                                     | 8, 8,  30,  5,  5
                                          9, 17, 44, 49
                                   giving return-code 
                                   
                move 0,5             to    winprint-col-start
                call "WIN$PRINTER"   using winprint-set-page-column, 
                                           winprint-column
                                    giving return-code
                                   
                move 3,7             to    winprint-col-start
                call "WIN$PRINTER"   using winprint-set-page-column, 
                                           winprint-column
                                    giving return-code
                                   
                move 6,1             to    winprint-col-start
                call "WIN$PRINTER"   using winprint-set-page-column, 
                                           winprint-column
                                    giving return-code
                                   
                move 14              to    winprint-col-start
                call "WIN$PRINTER"   using winprint-set-page-column, 
                                           winprint-column
                                    giving return-code
                                   
                move 16,5            to    winprint-col-start
                call "WIN$PRINTER"   using winprint-set-page-column, 
                                           winprint-column
                                    giving return-code
           when 4      
                call "win$printer"  using winprint-set-data-columns,
                                     | 10, 8,  8,  8, 11, 16
                                          11, 19, 27, 35, 46
                                   giving return-code
                                   
                move 0,5             to    winprint-col-start
                call "WIN$PRINTER"   using winprint-set-page-column, 
                                           winprint-column
                                    giving return-code
                                   
                move 3,7              to   winprint-col-start
                call "WIN$PRINTER"   using winprint-set-page-column, 
                                           winprint-column
                                    giving return-code
                                   
                move 6,3              to   winprint-col-start
                call "WIN$PRINTER"   using winprint-set-page-column, 
                                           winprint-column
                                    giving return-code
                                   
                move 9,2                 to  winprint-col-start
                call "WIN$PRINTER"   using winprint-set-page-column, 
                                           winprint-column
                                    giving return-code
                                   
                move 11,8            to   winprint-col-start
                call "WIN$PRINTER"   using winprint-set-page-column, 
                                           winprint-column
                                    giving return-code
                                   
                move 14,5            to    winprint-col-start
                call "WIN$PRINTER"   using winprint-set-page-column, 
                                           winprint-column
                                    giving return-code
           when 5       
                call "win$printer"  using winprint-set-data-columns,
                                      |40, 5
                                          41              
                                   giving return-code
                                   
                move 0,5             to    winprint-col-start
                call "WIN$PRINTER"   using winprint-set-page-column, 
                                           winprint-column
                                    giving return-code
                                   
                move 18              to    winprint-col-start
                call "WIN$PRINTER"   using winprint-set-page-column, 
                                           winprint-column
                                    giving return-code

           when 6                                                       
                call "WIN$PRINTER"  using winprint-set-data-columns,
                                     |35, 35
                                          36              
                                    giving return-code
                                   
                move 0,5             to    winprint-col-start
                call "WIN$PRINTER"   using winprint-set-page-column, 
                                           winprint-column
                                    giving return-code
                                   
                move 10              to    winprint-col-start
                call "WIN$PRINTER"   using winprint-set-page-column, 
                                           winprint-column
                                    giving return-code
           when 7 
                call "WIN$PRINTER"  using winprint-set-data-columns,
                             |6, 50,  2, 10, 14,  8,  8, 14, 2
                                  7, 57, 59, 69, 83, 91, 99, 113
                                    giving return-code
                                   
                move 0,2               to winprint-col-start
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code
                                   
                move 1,7                to winprint-col-start
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code
                                   
                move 7,7                to winprint-col-start
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code
                                  
                move 8,1                to winprint-col-start
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code
                                  
                move 9,6               to winprint-col-start
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code
                                  
                move 12,7               to winprint-col-start
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code
                                  
                move 14,5               to winprint-col-start
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code
                                  
                move 16,0               to winprint-col-start
                call "WIN$PRINTER"   using winprint-set-page-column, 
                                           winprint-column
                                    giving return-code
                                  
                move 19                 to winprint-col-start
                call "WIN$PRINTER"   using winprint-set-page-column, 
                                           winprint-column
                                    giving return-code
           when 7,5 |* T O T A L E   P I O M B O *...
                call "WIN$PRINTER"  using winprint-set-data-columns,
                                          51
                                    giving return-code
                                   
                move 1,7                to winprint-col-start
                move wprtalign-left     to winprint-col-alignment
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code
                                   
                move 7,7                to winprint-col-start
                move wprtalign-right    to winprint-col-alignment
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code
                                  
                move 16,0               to winprint-col-start
                call "WIN$PRINTER"   using winprint-set-page-column, 
                                           winprint-column
                                    giving return-code

           when 8
                call "WIN$PRINTER"  using winprint-set-data-columns,
                                     |14,  2, 14, 14, 14
                                          15, 18, 32, 49
                                    giving return-code
                                   
                move 0,7               to winprint-col-start
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code
                                   
                move 4,95               to winprint-col-start
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code
                                   
                move 6,7                to winprint-col-start
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code
                                                            
                move 10,4               to winprint-col-start
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code
                                  
                move 15,7               to winprint-col-start
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code

           when 8,5 |Pie di pagina bozza (imponibile a SX)
                call "WIN$PRINTER"  using winprint-set-data-columns,
                                     |14,  2, 14, 14, 14
                                          15, 18, 32, 49
                                    giving return-code
                                   
                move 0,7               to winprint-col-start
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code
                                   
                move 4,95               to winprint-col-start
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code
                                   
                move 4,7                to winprint-col-start
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code
                                                            
                move 10,4               to winprint-col-start
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code
                                  
                move 15,7               to winprint-col-start
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code

           when 9 
                call "WIN$PRINTER"  using winprint-set-data-columns,
                                           2, 73
                                    giving return-code
                                                      
                move 0,2                to winprint-col-start
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code

                move 6,7                to winprint-col-start       
                move wprtalign-right    to winprint-col-alignment
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code

                move 19,4               to winprint-col-start   
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code

           when 9,5
                call "WIN$PRINTER"  using winprint-set-data-columns,
                                           21, 26
                                    giving return-code
                                                      
                move 1,3                to winprint-col-start
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code

                move 4,3                to winprint-col-start
                move wprtalign-right    to winprint-col-alignment       
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code

                move 6,2                to winprint-col-start
                move wprtalign-left     to winprint-col-alignment
                move 0,3                to winprint-col-indent
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code
           when 10
                call "WIN$PRINTER"  using winprint-set-data-columns,
                                           2
                                    giving return-code
                                                      
                move 0,2                to winprint-col-start
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code

                move 16,3               to winprint-col-start
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code
           when 11
                call "WIN$PRINTER"  using winprint-set-data-columns,
                                           100
                                    giving return-code
                                                      
                move 3,0                to winprint-col-start
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code
           when 11,1
                call "WIN$PRINTER"  using winprint-set-data-columns,
                                           100
                                    giving return-code
                                                      
                move 7,7                to winprint-col-start
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code
           when 11,2
                call "WIN$PRINTER"  using winprint-set-data-columns,
                                           100
                                    giving return-code
                                                      
                move 12,5               to winprint-col-start
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code
           when 11,3
                call "WIN$PRINTER"  using winprint-set-data-columns,
                                           100
                                    giving return-code
                                                      
                move 17,3               to winprint-col-start
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code
           when 12
                call "WIN$PRINTER"  using winprint-set-data-columns,
                                           100
                                    giving return-code
                                                      
                move 0,2                to winprint-col-start
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code
           |DA QUI IN POI SONO I SETTAGGI DA STATMARGINI
           when 13|TITOLO
                call "WIN$PRINTER"  using winprint-set-data-columns,
                                          42, 120 
                                    giving return-code
                                   
                move 9,7               to winprint-col-start
                move wprtalign-left    to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code

                move 21,8              to winprint-col-start
                move wprtalign-left    to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code
                |tappo
                move 26,5               to winprint-col-start       
                move wprtalign-left     to winprint-col-alignment
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code
 
           when 14|INTESTAZIONE
                call "WIN$PRINTER"   using winprint-set-data-columns,
                                     |14,  2, 14, 14, 14
                                          4, 9, 17, 25, 32, 38
                                    giving return-code
                                   
                move 0,7                to winprint-col-start
                call "WIN$PRINTER"   using winprint-set-page-column,
                                          winprint-column
                                    giving return-code
                                   
                move 1,9                to winprint-col-start
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code

                move  8,8               to winprint-col-start

                move wprtalign-right    to winprint-col-alignment
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code

                move 12,7               to winprint-col-start
                move wprtalign-right    to winprint-col-alignment
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code

                move 16,6               to winprint-col-start
                move wprtalign-right    to winprint-col-alignment
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code
                                  
                move 20,5               to winprint-col-start
                move wprtalign-right    to winprint-col-alignment
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code
                                  
                move 24,4               to winprint-col-start
                move wprtalign-right    to winprint-col-alignment
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column

                                    giving return-code
                
                |tappo
                move 28,3               to winprint-col-start
                move wprtalign-right    to winprint-col-alignment
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code
 
           when 15|RIGA
                call "WIN$PRINTER"  using winprint-set-data-columns,
                                       4, 8, 35, 53, 71, 89, 107, 125
                                    giving return-code
                                   
                move 0,7               to winprint-col-start
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code
                                   
                move 0,8                to winprint-col-start
                move wprtalign-right    to winprint-col-alignment
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column



                                    giving return-code
                                   
                move 2,3                to winprint-col-start       
                move 0,3                to winprint-col-indent
                move wprtalign-left     to winprint-col-alignment
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code

                move  8,8               to winprint-col-start
                move wprtalign-right    to winprint-col-alignment
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code

                move 12,7               to winprint-col-start       
                move wprtalign-right    to winprint-col-alignment
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code

                move 16,6               to winprint-col-start       
                move wprtalign-right    to winprint-col-alignment
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code
                                  
                move 20,5               to winprint-col-start       
                move wprtalign-right    to winprint-col-alignment
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code
                                  
                move 24,4               to winprint-col-start
                move wprtalign-right    to winprint-col-alignment
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code
                
                |tappo
                move 28,3               to winprint-col-start       
                move wprtalign-right    to winprint-col-alignment
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code
 
           when 16|TOTALI
                call "WIN$PRINTER"  using winprint-set-data-columns,
                                           23, 41, 59, 77, 95, 113
                                    giving return-code
                                   
                move 0,7               to winprint-col-start
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code

                move 8,8                to winprint-col-start
                move wprtalign-right    to winprint-col-alignment
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code

                move 12,7               to winprint-col-start
                move wprtalign-right    to winprint-col-alignment
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code

                move 16,6               to winprint-col-start
                move wprtalign-right    to winprint-col-alignment
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code
                                  
                move 20,5               to winprint-col-start
                move wprtalign-right    to winprint-col-alignment
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code
                                  
                move 24,4               to winprint-col-start
                move wprtalign-right    to winprint-col-alignment
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code
                                  
                |tappo
                move 28,3               to winprint-col-start
                move wprtalign-right    to winprint-col-alignment
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code

           |DA QUI IN POI SONO I SETTAGGI DA STARTICOLI
           when 17|INTESTAZIONE
                call "WIN$PRINTER"  using winprint-set-data-columns,
                                     |14,  2, 14, 14, 14
                                           47
                                    giving return-code
                                   
                move 0,7               to winprint-col-start
                move wprtalign-center  to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code  
           when 17,5 |Contatore di Pagina
                call "WIN$PRINTER"  using winprint-set-data-columns,
                                     |14,  2, 14, 14, 14
                                           10
                                    giving return-code
                                   
                move 0,2               to winprint-col-start
                move wprtalign-right   to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code

                move 19,5              to winprint-col-start
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code

           when 18|TITOLO
                call "WIN$PRINTER"  using winprint-set-data-columns,
                                          7, 23, 27, 33, 40, 43
                                    giving return-code
                |"Codice"
                move 0,7               to winprint-col-start
                move wprtalign-right   to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code
                |"PRODOTTI NORMALI"
                move 2,1                to winprint-col-start       
                move 0,2                to winprint-col-indent
                move wprtalign-left     to winprint-col-alignment
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code
                |"Sc.%"
                move 8,78               to winprint-col-start
                move wprtalign-right    to winprint-col-alignment
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code
                |"Prezzo"
                move 10,05              to winprint-col-start
                move wprtalign-right    to winprint-col-alignment
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code
                |"Imposta"
                move 12,40              to winprint-col-start       
                move wprtalign-right    to winprint-col-alignment
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code
                |"COU"
                move 14,80              to winprint-col-start       
                move wprtalign-right    to winprint-col-alignment
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code
                |"TOTALE"
                move 17,10              to winprint-col-start       
                move wprtalign-right    to winprint-col-alignment
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code
                |Tappo                
                move 19,5               to winprint-col-start       
                move wprtalign-right    to winprint-col-alignment
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code
 
           when 19|RIGA
                call "WIN$PRINTER"  using winprint-set-data-columns,
                                          7, 34, 39, 49, 59, 69
                                    giving return-code
                |ARTICOLO                   
                move 0,7               to winprint-col-start
                move wprtalign-right   to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code
                |DESCRIZIONE                   
                move 2,1                to winprint-col-start       
                move 0,2                to winprint-col-indent
                move wprtalign-left     to winprint-col-alignment
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code
                |SCONTO
                move 8,6                to winprint-col-start
                move wprtalign-right    to winprint-col-alignment
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code
                |PREZZO
                move 10                 to winprint-col-start
                move wprtalign-right    to winprint-col-alignment
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code
                |IMPOSTA
                move 12,4               to winprint-col-start       
                move wprtalign-right    to winprint-col-alignment
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code
                |COU
                move 14,8               to winprint-col-start       
                move wprtalign-right    to winprint-col-alignment
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code
                |TOTALE                  
                move 17,15              to winprint-col-start       
                move wprtalign-right    to winprint-col-alignment
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code
                |TAPPO                  
                move 19,5               to winprint-col-start       
                move wprtalign-right    to winprint-col-alignment
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code

           |DA QUI IN POI SONO I SETTAGGI DA STP-PERAGE
           when 20|TITOLO
                call "WIN$PRINTER"  using winprint-set-data-columns,
                                           91
                                    giving return-code
                                   
                move 0,7               to winprint-col-start
                move wprtalign-center  to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code
 
           when 21|INTESTAZIONE
                call "WIN$PRINTER"  using winprint-set-data-columns,
                              5, 10, 12, 19, 26, 34, 36, 40, 46, 52
                                    giving return-code
                |data
                move 0,9                to winprint-col-start       
                move wprtalign-left     to winprint-col-alignment
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code
                |fatt.                   
                move 3                  to winprint-col-start
                move wprtalign-left     to winprint-col-alignment
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code
                |rg
                move 4,5                to winprint-col-start
                move wprtalign-left     to winprint-col-alignment
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code
                |cliente
                move 5,3                to winprint-col-start
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code
                |listino
                move 11,9               to winprint-col-start
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code
                |prodotto                  
                move 13,8               to winprint-col-start
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code
                |um                  
                move 20,8               to winprint-col-start
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code
                |qta
                move 22,4               to winprint-col-start
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code
                |prezzo
                move 24,35              to winprint-col-start
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code
                |provv.
                move 26,8               to winprint-col-start
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code

           when 22|RIGA
                call "WIN$PRINTER"  using winprint-set-data-columns,
                               9, 15, 17, 47, 56, 86, 88, 95, 105
                                   giving return-code
                |data
                move 0,7               to winprint-col-start
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code
                |num-fatt                   
                move 3                  to winprint-col-start
                move wprtalign-right    to winprint-col-alignment
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code
                |riga-fatt                    
                move 4                  to winprint-col-start
                move wprtalign-right    to winprint-col-alignment
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code
                |des-cliente
                move 5                  to winprint-col-start
                move wprtalign-left     to winprint-col-alignment
                move 0,3                to winprint-col-indent       
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code
                |prezzo-netto-agente
                move 11,5               to winprint-col-start       
                move wprtalign-right    to winprint-col-alignment
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code
                |des-art
                move 13,5               to winprint-col-start       
                move wprtalign-left     to winprint-col-alignment
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code
                |r-um
                move 20,8               to winprint-col-start       
                move wprtalign-left     to winprint-col-alignment
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code
                |qta-vend                                     
                move 21,6               to winprint-col-start
                move wprtalign-right    to winprint-col-alignment
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code
                |prezzo-unit-vend
                move 23,3               to winprint-col-start       
                move wprtalign-right    to winprint-col-alignment
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code
                |val-provvig
                move 25,9               to winprint-col-start       
                move wprtalign-right    to winprint-col-alignment
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code
                |tappo
                move 28,3               to winprint-col-start       
                move wprtalign-right    to winprint-col-alignment
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code
 
           when 23|TOTALI
                call "WIN$PRINTER"  using winprint-set-data-columns,
                                         17, 26
                                    giving return-code
                |label
                move 13                to winprint-col-start
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code
                |tot-qta
                move 21,7               to winprint-col-start
                move wprtalign-right    to winprint-col-alignment
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code
                |tot-prov
                move 23,3               to winprint-col-start
                move wprtalign-right    to winprint-col-alignment
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code
                |tappo
                move 28,3               to winprint-col-start       
                move wprtalign-right    to winprint-col-alignment
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code
           |DA QUI IN POI SONO I SETTAGGI DA STP-BROG
           when 24|TITOLO
                call "WIN$PRINTER"  using winprint-set-data-columns,
                                          42, 120
                                    giving return-code
                                   
                move 9,7               to winprint-col-start
                move wprtalign-left    to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code

                move 21,8              to winprint-col-start
                move wprtalign-left    to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code 

                move 26,4               to winprint-col-start       
                move wprtalign-left     to winprint-col-alignment
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code
                |tappo
                move 28,3               to winprint-col-start       
                move wprtalign-right    to winprint-col-alignment
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code

           when 25|RIGA AGENTE
                call "WIN$PRINTER"  using winprint-set-data-columns,
                                          6, 46
                                    giving return-code
                |codice agente
                move 2                 to winprint-col-start
                move wprtalign-right   to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code
                |descrizione agente
                move 5                  to winprint-col-start
                move wprtalign-left     to winprint-col-alignment
                move 0,2                to winprint-col-indent
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code

           when 26|INTESTAZIONE
                call "WIN$PRINTER"  using winprint-set-data-columns,
                   6, 8, 15, 22, 29, 33, 38, 42, 50, 52, 56, 62, 68
                                    giving return-code
                |fatt
                move 0,3                to winprint-col-start       
                move wprtalign-right    to winprint-col-alignment
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code
                |rg                   
                move 1,9                to winprint-col-start
                move 0,1                to winprint-col-indent
                move wprtalign-right    to winprint-col-alignment
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code
                |cliente
                move 2,5                to winprint-col-start
                move 0,2                to winprint-col-indent
                move wprtalign-left     to winprint-col-alignment
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code
                |num-listino
                move 7,3                to winprint-col-start
                move wprtalign-right    to winprint-col-alignment
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code
                |listino
                move 8,4                to winprint-col-start
                move wprtalign-right    to winprint-col-alignment
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code
                |sconto                  
                move 10,0               to winprint-col-start
                move wprtalign-right    to winprint-col-alignment
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code
                |peso-um                  
                move 11,1               to winprint-col-start
                move wprtalign-right    to winprint-col-alignment
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code
                |cod-articolo                  
                move 12,3               to winprint-col-start
                move wprtalign-right    to winprint-col-alignment
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code
                |desc-articolo                  
                move 14,1               to winprint-col-start
                move wprtalign-left     to winprint-col-alignment
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code
                |um                  
                move 22,05              to winprint-col-start
                move wprtalign-left     to winprint-col-alignment
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code
                |qta
                move 22,65              to winprint-col-start
                move wprtalign-right    to winprint-col-alignment
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code
                |prezzo
                move 23,9               to winprint-col-start
                move wprtalign-right    to winprint-col-alignment
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code
                |provv.
                move 25,75              to winprint-col-start
                move wprtalign-right    to winprint-col-alignment
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code
                |tappo
                move 27,5               to winprint-col-start       
                move wprtalign-right    to winprint-col-alignment
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code

           when 27|RIGA
                call "WIN$PRINTER"  using winprint-set-data-columns,
                 7, 9, 34, 38, 48, 53, 59, 60, 66, 111, 113, 120, 130
                                   giving return-code
                |num-fatt
                move 0,3               to winprint-col-start
                move wprtalign-right   to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code
                |riga-fatt                   
                move 1,9                to winprint-col-start
                move wprtalign-right    to winprint-col-alignment
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code
                |des-cliente                    
                move 2,5                to winprint-col-start
                move wprtalign-left     to winprint-col-alignment
                move 0,2                to winprint-col-indent       
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code
                |num. listino
                move 7,5                to winprint-col-start
                move wprtalign-right    to winprint-col-alignment
                move 0,15               to winprint-col-indent       
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code
                |prezzo-netto-agente
                move 8,4                to winprint-col-start       
                move wprtalign-right    to winprint-col-alignment
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code
                |sconto listino
                move 10,0               to winprint-col-start       
                move wprtalign-right    to winprint-col-alignment
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code
                |peso-um
                move 11,1               to winprint-col-start
                move wprtalign-right    to winprint-col-alignment
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code
                |tipo-vendita
                move 12,3                to winprint-col-start       
                move wprtalign-right     to winprint-col-alignment
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code
                |cod-articolo
                move 13,0               to winprint-col-start       
                move wprtalign-right    to winprint-col-alignment
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code
                |des-art
                move 14,10              to winprint-col-start       
                move  0,2               to winprint-col-indent
                move wprtalign-left     to winprint-col-alignment
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code
                |r-um
                move 22,1               to winprint-col-start       
                move wprtalign-left     to winprint-col-alignment
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code
                |qta-vend
                move 22,6               to winprint-col-start
                move wprtalign-right    to winprint-col-alignment
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code
                |prezzo-unit-vend
                move 23,9               to winprint-col-start
                move wprtalign-right    to winprint-col-alignment
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code
                |val-provvig
                move 25,75              to winprint-col-start
                move wprtalign-right    to winprint-col-alignment
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code
                |tappo
                move 27,5               to winprint-col-start
                move wprtalign-right    to winprint-col-alignment
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code
           |DA QUI IN POI SONO I SETTAGGI DA STP-MARCA
           when 28|TITOLO
                call "WIN$PRINTER"  using winprint-set-data-columns,
                                           55
                                    giving return-code
                                   
                move 0,2               to winprint-col-start
                move wprtalign-center  to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code
 
           when 29|INTESTAZIONE
                call "WIN$PRINTER"  using winprint-set-data-columns,
                                          6, 9, 17, 26
                                    giving return-code
                |Marca
                move 2                  to winprint-col-start       
                move wprtalign-left     to winprint-col-alignment
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code
                |Kg.                   
                move 8                  to winprint-col-start
                move wprtalign-right    to winprint-col-alignment
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code
                |Provvig.
                move 10,7               to winprint-col-start
                move wprtalign-right    to winprint-col-alignment
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code
                |Media/Kg.
                move 13,7               to winprint-col-start
                move wprtalign-right    to winprint-col-alignment
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code
                |Fatt./Euro
                move 16,6               to winprint-col-start
                move wprtalign-right    to winprint-col-alignment
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code
                |tappo
                move 19,5               to winprint-col-start       
                move wprtalign-right    to winprint-col-alignment
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code

           when 30|RIGA
                call "WIN$PRINTER"  using winprint-set-data-columns,
                                          5, 35, 45, 59, 73
                                   giving return-code
                |cod-marca
                move 0,7               to winprint-col-start
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code
                |des-marca                   
                move 2                  to winprint-col-start
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code
                |Kg.                    
                move 8                  to winprint-col-start
                move wprtalign-right    to winprint-col-alignment
                move 0,5                to winprint-col-indent       
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code
                |Provvig.
                move 10,7               to winprint-col-start
                move wprtalign-right    to winprint-col-alignment
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code
                |Media/Kg.
                move 13,7               to winprint-col-start       
                move wprtalign-right    to winprint-col-alignment
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code
                |Fatt./Euro
                move 16,6               to winprint-col-start       
                move wprtalign-right    to winprint-col-alignment
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code
                |tappo
                move 19,5               to winprint-col-start       
                move wprtalign-right    to winprint-col-alignment
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code
 
           when 31|TOTALI
                call "WIN$PRINTER"  using winprint-set-data-columns,
                                          7, 18, 34, 50
                                    giving return-code
                |label Totali
                move 3                 to winprint-col-start
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code
                |tot-kg
                move 8                  to winprint-col-start
                move wprtalign-right    to winprint-col-alignment
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code
                |tot-provvig
                move 10,7               to winprint-col-start
                move wprtalign-right    to winprint-col-alignment
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code
                |tot-media-kg
                move 13,7               to winprint-col-start
                move wprtalign-right    to winprint-col-alignment
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code
                |tot-fatt
                move 16,6               to winprint-col-start
                move wprtalign-right    to winprint-col-alignment
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code
                |tappo
                move 19,5               to winprint-col-start       
                move wprtalign-right    to winprint-col-alignment
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code
           |DA QUI IN POI SONO I SETTAGGI DA STP-INCSOS-P
           when 32|TITOLO
                call "WIN$PRINTER"  using winprint-set-data-columns,
                                           55
                                    giving return-code
                                   
                move 0,7               to winprint-col-start
                move wprtalign-center  to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code
           when 33|TITOLO1
                call "WIN$PRINTER"  using winprint-set-data-columns,
                                           100
                                    giving return-code
                                   
                move 0,7               to winprint-col-start
                move wprtalign-center  to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code
 
           when 34|INTESTAZIONE
                call "WIN$PRINTER"  using winprint-set-data-columns,
                                   5, 20, 26, 30, 32, 40, 42, 46, 52
                                    giving return-code
                |Cod
                move 0,7                  to winprint-col-start       
                move wprtalign-left     to winprint-col-alignment
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code
                |Ragione sociale                   
                move 3,5                to winprint-col-start
                move wprtalign-left     to winprint-col-alignment
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code
                |n.fat.
                move 8,7                to winprint-col-start
                move wprtalign-left     to winprint-col-alignment
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code
                |data
                move 10,6               to winprint-col-start
                move wprtalign-left     to winprint-col-alignment
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code
                |rg
                move 12,5               to winprint-col-start
                move wprtalign-left     to winprint-col-alignment
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code
                |prodotto
                move 13,5               to winprint-col-start
                move wprtalign-left     to winprint-col-alignment
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code
                |UM
                move 20                 to winprint-col-start
                move wprtalign-left     to winprint-col-alignment
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code
                |Q.ta
                move 22,5               to winprint-col-start
                move wprtalign-left     to winprint-col-alignment
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code
                |Prezzo
                move 24,5               to winprint-col-start
                move wprtalign-left     to winprint-col-alignment
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code
                |Importo
                move 26,6               to winprint-col-start
                move wprtalign-left     to winprint-col-alignment
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code

           when 35|RIGA
              call "WIN$PRINTER"  using winprint-set-data-columns,
                               6, 36, 42, 50, 52, 82, 84, 91, 101
                                 giving return-code
                |cod-cliente
                move 0,7               to winprint-col-start
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code
                |des-cliente                   
                move 2                  to winprint-col-start
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code
                |n.fatt.                    
                move 8                  to winprint-col-start
                move wprtalign-right    to winprint-col-alignment
                move 0,5                to winprint-col-indent       
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code
                |data
                move 10                 to winprint-col-start
                move wprtalign-right    to winprint-col-alignment
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code
                |riga
                move 12                 to winprint-col-start
                move wprtalign-right    to winprint-col-alignment
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code
                |prodotto
                move 13                 to winprint-col-start       
                move wprtalign-left     to winprint-col-alignment
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code
                |r-um
                move 19,7               to winprint-col-start       
                move wprtalign-left     to winprint-col-alignment
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code
                |qta-vend                                     
                move 21,5               to winprint-col-start
                move wprtalign-right    to winprint-col-alignment
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code
                |prezzo-unit-vend
                move 23,5                 to winprint-col-start       
                move wprtalign-right    to winprint-col-alignment
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code
                |val-provvig (importo)
                move 26                 to winprint-col-start       
                move wprtalign-right    to winprint-col-alignment
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code
                |tappo
                move 28,5               to winprint-col-start       
                move wprtalign-right    to winprint-col-alignment
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code
 
           when 36|TOTALI
                call "WIN$PRINTER"  using winprint-set-data-columns,
                                          17
                                    giving return-code
                |label Totali
                move 13                to winprint-col-start
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code
                |tot-prov
                move 26               to winprint-col-start
                move wprtalign-right    to winprint-col-alignment
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code
                |tappo
                move 28,5               to winprint-col-start       
                move wprtalign-right    to winprint-col-alignment
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code
           |DA QUI IN POI SONO I SETTAGGI DA STP-OMAG-P
           when 37|TITOLO
                call "WIN$PRINTER"  using winprint-set-data-columns,
                                           85
                                    giving return-code
                                   
                move 0,7               to winprint-col-start
                move wprtalign-center  to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code
 
           when 38|INTESTAZIONE
                call "WIN$PRINTER"  using winprint-set-data-columns,
                              5, 10, 12, 19, 25, 33, 35, 39, 45
                                    giving return-code
                |data
                move 0,9                to winprint-col-start       
                move wprtalign-left     to winprint-col-alignment
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code
                |fatt.                   
                move 3                  to winprint-col-start
                move wprtalign-left     to winprint-col-alignment
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code
                |rg
                move 4,5                to winprint-col-start
                move wprtalign-left     to winprint-col-alignment
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code
                |cliente
                move 5,5                to winprint-col-start
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code
                |codice
                move 12,0               to winprint-col-start
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code
                |prodotto                  
                move 14,0               to winprint-col-start
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code
                |um                  
                move 20,55              to winprint-col-start
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code
                |qta
                move 22,2               to winprint-col-start
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code
                |prezzo
                move 24,2               to winprint-col-start
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code
                |addebito
                move 26,2               to winprint-col-start
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code

           when 39|RIGA
                call "WIN$PRINTER"  using winprint-set-data-columns,
                               9, 15, 17, 47, 53, 78, 80, 87, 97
                                   giving return-code
                |data
                move 0,7               to winprint-col-start
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code
                |num-fatt                   
                move 3                  to winprint-col-start
                move wprtalign-right    to winprint-col-alignment
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code
                |riga-fatt                    
                move 4                  to winprint-col-start
                move wprtalign-right    to winprint-col-alignment
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code
                |des-cliente
                move 5                  to winprint-col-start
                move wprtalign-left     to winprint-col-alignment
                move 0,5                to winprint-col-indent       
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code
                |cod-art
                move 12                 to winprint-col-start       
                move wprtalign-right    to winprint-col-alignment
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code
                |des-art
                move 13,5               to winprint-col-start       
                move wprtalign-left     to winprint-col-alignment
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code
                |r-um
                move 20,2               to winprint-col-start       
                move wprtalign-left     to winprint-col-alignment
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code
                |qta-vend                                     
                move 21,3               to winprint-col-start
                move wprtalign-right    to winprint-col-alignment
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code
                |prezzo-unit-vend
                move 23,1               to winprint-col-start       
                move wprtalign-right    to winprint-col-alignment
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code
                |val-provvig (addebito)
                move 25,7               to winprint-col-start       
                move wprtalign-right    to winprint-col-alignment
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code
                |tappo
                move 28,3               to winprint-col-start       
                move wprtalign-right    to winprint-col-alignment
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code
 
           when 40|TOTALI
                call "WIN$PRINTER"  using winprint-set-data-columns,
                                         17, 26
                                    giving return-code
                |label
                move 13                to winprint-col-start
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code
                |tot-qta
                move 21,2               to winprint-col-start
                move wprtalign-right    to winprint-col-alignment
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code
                |tot-prov
                move 23,2               to winprint-col-start
                move wprtalign-right    to winprint-col-alignment
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code
                |tappo
                move 28,3               to winprint-col-start       
                move wprtalign-right    to winprint-col-alignment
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code
           |DA QUI IN POI SONO I SETTAGGI DA INVENTORY-P
           when 41|TITOLO
                call "WIN$PRINTER"  using winprint-set-data-columns,
                                     |14,  2, 14, 14, 14
                                           50, 120
                                    giving return-code
                                   
                move 8,9               to winprint-col-start
                move wprtalign-center  to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code
 
                move 21,7              to winprint-col-start
                move wprtalign-left    to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code
                |tappo
                move 26,4              to winprint-col-start       
                move wprtalign-left    to winprint-col-alignment
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code
           when 42|INTESTAZIONE
                call "WIN$PRINTER"  using winprint-set-data-columns,
                                     |14,  2, 14, 14, 14
                                          7, 10, 21, 23, 27, 33, 39
                                    giving return-code
                                   
                move 0,7               to winprint-col-start
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code
                                   
                move 2,8                to winprint-col-start
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code

                move 4,2               to winprint-col-start
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code

                move 14,75              to winprint-col-start
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code

                move 16,85              to winprint-col-start
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code
                                  
                move 19,6               to winprint-col-start
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code
                                  
                move 23,15               to winprint-col-start
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code
 
                move 27,5               to winprint-col-start
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code

           when 43|RIGA
                call "WIN$PRINTER"  using winprint-set-data-columns,
                                     |14,  2, 14, 14, 14
                                   7, 8, 11, 59, 61, 72, 86, 101, 115
                                    giving return-code
                
                |Codice articolo
                move 0,5                to winprint-col-start
                move wprtalign-right    to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code
                          
                |* UTF
                move 2,2                to winprint-col-start       
                move 0,1                to winprint-col-indent
                move wprtalign-center   to winprint-col-alignment
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code
                                   
                |Magazzino
                move 2,55               to winprint-col-start       
                move 0,3                to winprint-col-indent
                move wprtalign-left     to winprint-col-alignment
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code

                |Descrizione
                move 3,9                to winprint-col-start
                move wprtalign-left     to winprint-col-alignment
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code

                |UM
                move 14,5               to winprint-col-start       
                move wprtalign-left     to winprint-col-alignment
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code

                |Q.tà
                move 15,65              to winprint-col-start
                move wprtalign-right    to winprint-col-alignment
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code
                                  
                |Prezzo
                move 17,8               to winprint-col-start
                move wprtalign-right    to winprint-col-alignment
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code
                                  
                |Valore
                move 21,2               to winprint-col-start
                move wprtalign-right    to winprint-col-alignment
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code
 
                |Kg.
                move 24,7               to winprint-col-start
                move wprtalign-right    to winprint-col-alignment
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code
 
                |tappo
                move 28,3               to winprint-col-start
                move wprtalign-right    to winprint-col-alignment
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code

           when 44|TOTALI
                call "WIN$PRINTER"  using winprint-set-data-columns,
                                     |14,  2, 14, 14, 14
                                          46, 61, 77
                                    giving return-code
                                   
                move 2,8               to winprint-col-start
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code

                move 20,0               to winprint-col-start
                move wprtalign-right    to winprint-col-alignment
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code

                move 24,7               to winprint-col-start
                move wprtalign-right    to winprint-col-alignment
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code 

                |tappo
                move 28,3               to winprint-col-start
                move wprtalign-right    to winprint-col-alignment
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code

           when 45|TOTALI
                call "WIN$PRINTER"  using winprint-set-data-columns,
                                     |14,  2, 14, 14, 14
                                          18, 34
                                    giving return-code
                                   
                move 2,8               to winprint-col-start
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code

                move 24,3               to winprint-col-start
                move wprtalign-right    to winprint-col-alignment
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code

                |tappo
                move 28,3               to winprint-col-start
                move wprtalign-right    to winprint-col-alignment
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code

           when 46|TOTALI GENERALI
                call "WIN$PRINTER"  using winprint-set-data-columns,
                                     |14,  2, 14, 14, 14
                                          25, 41
                                    giving return-code
                                   
                move 2,8               to winprint-col-start
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code

                move 24,9               to winprint-col-start
                move wprtalign-right    to winprint-col-alignment
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code

                move 28,3               to winprint-col-start
                move wprtalign-right    to winprint-col-alignment
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code
           when 47|TOTALE MAGAZZINO
                call "WIN$PRINTER"  using winprint-set-data-columns,
                                     |14,  2, 14, 14, 14
                                          23, 38, 54
                                    giving return-code
                                   
                move 2,8               to winprint-col-start
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code

                move 21,2               to winprint-col-start
                move wprtalign-right    to winprint-col-alignment
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code

                move 24,7               to winprint-col-start
                move wprtalign-right    to winprint-col-alignment
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code

                |tappo
                move 28,3               to winprint-col-start
                move wprtalign-right    to winprint-col-alignment
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code
           |DA QUI IN POI SONO I SETTAGGI DA STATGRUPPI
           when 48|TITOLO
                call "WIN$PRINTER"  using winprint-set-data-columns,
                                     |14,  2, 14, 14, 14
                                           50, 120
                                    giving return-code
                                   
                move 8,9               to winprint-col-start
                move wprtalign-center  to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code
 
                move 21,7              to winprint-col-start
                move wprtalign-left    to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code
                |tappo                             move 26,2              to winprint-col-start       
                move wprtalign-left    to winprint-col-alignment
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code
           when 49|INTESTAZIONE
                call "WIN$PRINTER"  using winprint-set-data-columns,
                                       4, 20, 30, 40, 51, 61, 70, 81
                                    giving return-code
                |Mag
                move 0,7               to winprint-col-start
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code
                                   
                |Marca Statistica
                move 1,85               to winprint-col-start
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code

                |Vendite Kg.
                move  9,4               to winprint-col-start
                move wprtalign-right    to winprint-col-alignment
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code

                |Val / 1000
                move 12,6               to winprint-col-start
                move wprtalign-right    to winprint-col-alignment
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code

                |Acquisti Kg.
                move 15,6               to winprint-col-start
                move wprtalign-right    to winprint-col-alignment
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code
                                  
                |Val / 1000
                move 18,9               to winprint-col-start
                move wprtalign-right    to winprint-col-alignment
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code
                                  
                |Giac. UTF
                move 21,9               to winprint-col-start
                move wprtalign-right    to winprint-col-alignment
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code
 
                |Giac. Altre
                move 25,1               to winprint-col-start
                move wprtalign-right    to winprint-col-alignment
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code
 
                |tappo
                move 28,3               to winprint-col-start
                move wprtalign-right    to winprint-col-alignment
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code

           when 50|RIGA
                call "WIN$PRINTER"  using winprint-set-data-columns,
                                   4, 8, 38, 53, 67, 82, 96, 111, 126
                                    giving return-code
                
                |Magazzino
                move 0,7                to winprint-col-start
                move wprtalign-left    to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code
                          
                |Marca statistica
                move 1,85             to winprint-col-start       
                move wprtalign-left   to winprint-col-alignment
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code
                                   
                |Descrizione Marca
                move 2,75               to winprint-col-start       
                move 0,1                to winprint-col-indent
                move wprtalign-left     to winprint-col-alignment
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code

                |Vendite Kg.
                move 9,4                to winprint-col-start
                move wprtalign-right    to winprint-col-alignment
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code

                |Val / 1000
                move 12,6               to winprint-col-start       
                move wprtalign-right    to winprint-col-alignment
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code

                |Acquisti Kg.
                move 15,6               to winprint-col-start
                move wprtalign-right    to winprint-col-alignment
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code
                                  
                |Val / 1000
                move 18,9               to winprint-col-start
                move wprtalign-right    to winprint-col-alignment
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code
                                  
                |Giac. UTF
                move 21,9               to winprint-col-start
                move wprtalign-right    to winprint-col-alignment
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code
 
                |Giac. Altre
                move 25,1               to winprint-col-start
                move wprtalign-right    to winprint-col-alignment
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code
                |tappo
                move 28,3               to winprint-col-start
                move wprtalign-right    to winprint-col-alignment
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code

           when 51|TOTALI
                call "WIN$PRINTER"  using winprint-set-data-columns,
                                         28, 43, 57, 72, 86, 101, 116
                                    giving return-code
                                   
                |Totali ------>
                move 0,8               to winprint-col-start
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code

                |Totale Vendite Kg.
                move 9,4               to winprint-col-start
                move wprtalign-right    to winprint-col-alignment
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code

                |Totale Val / 1000
                move 12,6               to winprint-col-start
                move wprtalign-right    to winprint-col-alignment
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code 

                |Totale Acquisti Kg.
                move 15,6               to winprint-col-start
                move wprtalign-right    to winprint-col-alignment
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code 

                |Totale Val / 1000
                move 18,9               to winprint-col-start
                move wprtalign-right    to winprint-col-alignment
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code 

                |Totale Giac. UTF
                move 21,9               to winprint-col-start
                move wprtalign-right    to winprint-col-alignment
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code 

                |Totale Giac. Altre
                move 25,1               to winprint-col-start
                move wprtalign-right    to winprint-col-alignment
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code 

                |tappo
                move 28,3               to winprint-col-start
                move wprtalign-right    to winprint-col-alignment
                call "WIN$PRINTER"   using winprint-set-page-column,
                                           winprint-column
                                    giving return-code 

           when 52 |SETTAGGIO stordcp QTA IN GRASSETTO
                call "WIN$PRINTER"  using winprint-set-data-columns,
                                          6
                                   giving return-code
                |Quantità                   
                move 18,3              to winprint-col-start
                move wprtalign-right   to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code
                |TAPPO
                move 19,6              to winprint-col-start
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code

           |DA QUI IN POI SONO I SETTAGGI DA LAB-ORD-DA-FAT
           when 53|TITOLO
                call "WIN$PRINTER"  using winprint-set-data-columns,
                                           49
                                    giving return-code
                                   
                move 1                 to winprint-col-start
                move wprtalign-center  to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code
                |TAPPO
                move 19,5              to winprint-col-start
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code      
           when 54|INTESTAZIONE
                call "WIN$PRINTER"  using winprint-set-data-columns,
                                           08, 14
                                    giving return-code
                |CLIENTE                   
                move 3,2               to winprint-col-start
                move wprtalign-left    to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code
                |TOTALE
                move 15,8              to winprint-col-start
                move wprtalign-left    to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code      

           when 55|RIGA
                call "WIN$PRINTER"  using winprint-set-data-columns,
                                           36
                                    giving return-code
                |DESCRIZINE GDO                   
                move 3,2               to winprint-col-start
                move wprtalign-left    to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code
                |TOTALE
                move 14,5              to winprint-col-start
                move wprtalign-right   to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code
                |TAPPO
                move 17,2              to winprint-col-start
                move wprtalign-right   to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code      

           when 56|TOTALE
                call "WIN$PRINTER"  using winprint-set-data-columns,
                                           36
                                    giving return-code
                |DESCRIZINE GDO
                move 3,2               to winprint-col-start
                move wprtalign-left    to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code
                |TOTALE
                move 08,50             to winprint-col-start
                move wprtalign-right   to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code
                |TAPPO
                move 17,2              to winprint-col-start
                move wprtalign-right   to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code

           when 57|DATA DI STAMPA E PAGINA DI (PUO' ESSERE COMUNE 
                  |A TUTTI I PROGRAMMI CON PAGINA VERTICALE)
                call "WIN$PRINTER"  using winprint-set-data-columns,
                                           40
                                    giving return-code
                |DATA DI STAMPA
                move 2                 to winprint-col-start
                move wprtalign-left    to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code
                |TOTALE
                move 10                to winprint-col-start
                move wprtalign-right   to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code
                |TAPPO
                move 18,5              to winprint-col-start
                move wprtalign-right   to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code
           |DA QUI INIZIANO I SETTAGGI DI "RIORDINO"
           when 58
                |TITOLO "RIORDINO" "DATA DI PARTENZA" DataDiPartenza
                call "WIN$PRINTER"  using winprint-set-data-columns,
                                           101
                                    giving return-code
                |RIORDINO
                move 1,1               to winprint-col-start
                move wprtalign-center  to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code
                |TAPPO
                move 19                to winprint-col-start
                move wprtalign-right   to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code
           |"RAGIONE SOCIALE" RagioneSociale
           when 59
                call "WIN$PRINTER"  using winprint-set-data-columns,
                                           41
                                    giving return-code
                |RIORDINO
                move 1,1               to winprint-col-start
                move wprtalign-center  to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code
                |TAPPO
                move 9,8               to winprint-col-start
                move wprtalign-right   to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code
           |"DESTINO" Destinazione
           when 60
                call "WIN$PRINTER"  using winprint-set-data-columns,
                                           41
                                    giving return-code
                |RIORDINO
                move 10,2              to winprint-col-start
                move wprtalign-center  to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code
                |TAPPO
                move 19                to winprint-col-start
                move wprtalign-right   to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code
           |"Cod. Articolo" e "Descrizione"
           when 61
                call "WIN$PRINTER"  using winprint-set-data-columns,
                                           14
                                    giving return-code
                |"Cod. Articolo"
                move 1                 to winprint-col-start
                move wprtalign-left    to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code
                |"Descrizione"
                move 5,5               to winprint-col-start
                move wprtalign-left    to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code
           |Codice Articolo e Descrizione Articolo
           when 62
                call "WIN$PRINTER"  using winprint-set-data-columns,
                                           7
                                    giving return-code
                |Articolo
                move 1                 to winprint-col-start
                move wprtalign-left    to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code
                |Descrizione
                move 5,5               to winprint-col-start
                move wprtalign-left    to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code
           |Codice Articolo e Descrizione Articolo
           when 62,5
                call "WIN$PRINTER"  using winprint-set-data-columns,
                                          5
                                   giving return-code
                |===>
                move 3,2               to winprint-col-start
                move wprtalign-left    to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code
           |Riga fissa nera
           when 63
                call "WIN$PRINTER"  using winprint-set-data-columns,
                                           150
                                    giving return-code
                |riga
                move 8,3               to winprint-col-start
                move wprtalign-left    to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code
           |Riga variabile rossa
           when 64
                call "WIN$PRINTER"  using winprint-set-data-columns,
                                           8, 18, 26
                                    giving return-code
                |Qta
                move 4,5               to winprint-col-start
                move wprtalign-right   to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code 
                |Data
                move 11,2              to winprint-col-start
                move wprtalign-right   to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code 
                |Numero
                move 14,7              to winprint-col-start
                move wprtalign-right   to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code
                |TAPPO
                move 19                to winprint-col-start
                move wprtalign-left    to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code

           |DA QUI INIZIANO I SETTAGGI DI "lab-ritiri"
           when 65 |"Data Fatt""Tipo""Prezzo"
                call "WIN$PRINTER"  using winprint-set-data-columns,
                                           13, 17
                                    giving return-code
                |"Data Fattura"
                move 7,0               to winprint-col-start
                move wprtalign-center  to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code 
                |"Tipo"
                move 11,9              to winprint-col-start
                move wprtalign-center  to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code 
                |"Prezzo"
                move 16,8              to winprint-col-start
                move wprtalign-right   to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code 
                |TAPPO
                move 19                to winprint-col-start
                move wprtalign-right   to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code

           when 65,5 |"Num. Fattura" e "Quantità"
                call "WIN$PRINTER"  using winprint-set-data-columns,
                                           13
                                    giving return-code
                |"Num fattura"
                move 10,4              to winprint-col-start
                move wprtalign-center  to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code 
                |"Quantità"
                move 14,3              to winprint-col-start
                move wprtalign-right   to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code 
                |TAPPO
                move 16,7              to winprint-col-start
                move wprtalign-right   to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code

           when 66 |Data, Tipo, Prezzo
                call "WIN$PRINTER"  using winprint-set-data-columns,
                                           11, 12
                                    giving return-code
                |Data
                move 7,0               to winprint-col-start
                move wprtalign-center  to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code 
                |Tipo
                move 11,9              to winprint-col-start
                move wprtalign-center  to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code 
                |Prezzo
                move 16,8              to winprint-col-start
                move wprtalign-right   to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code 
                |TAPPO
                move 19                to winprint-col-start
                move wprtalign-right   to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code

           when 67 |Numero fattura e qta
                call "WIN$PRINTER"  using winprint-set-data-columns,
                                           9
                                    giving return-code
                |Num fattura
                move 11,4              to winprint-col-start
                move wprtalign-center  to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code 
                |Quantità
                move 13,3              to winprint-col-start
                move wprtalign-right   to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code 
                |TAPPO
                move 16,7              to winprint-col-start
                move wprtalign-right   to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code

           when 68 |TOTALE QUTANTITA' SALDO
                call "WIN$PRINTER"  using winprint-set-data-columns,
                                           23
                                    giving return-code
                |"TOTALE QUANTITA' SALDO"
                move 8,3               to winprint-col-start
                move wprtalign-left    to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code 
                |QUANTITA' TOTALE
                move 13,6              to winprint-col-start
                move wprtalign-right   to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code 
                |TAPPO
                move 16,7              to winprint-col-start
                move wprtalign-right   to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code

           when 69 |DA QUI INIZIANO I SETTAGGI DI LAB-INEVASO
                call "WIN$PRINTER"  using winprint-set-data-columns,
                                          8, 16, 27, 35, 41, 47
                                    giving return-code

                |"Cliente"
                move 1,1               to winprint-col-start
                move wprtalign-left    to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code 
                |"Articolo"
                move 5,5               to winprint-col-start
                move wprtalign-center  to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code 
                |"Descrizione"
                move 7,5               to winprint-col-start
                move wprtalign-left    to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code 
                |"Quantità"
                move 13                to winprint-col-start
                move wprtalign-center  to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code 
                |"Prezzo"
                move 14,8              to winprint-col-start
                move wprtalign-right   to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code 
                |"Totale"
                move 16,9              to winprint-col-start
                move wprtalign-right   to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code 
                |TAPPO
                move 19,3              to winprint-col-start
                move wprtalign-left    to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code

           when 69,5 |Scorta <> 2 (NIENTE CLIENTE)
                call "WIN$PRINTER"  using winprint-set-data-columns,
                                          9, 20, 28, 34, 40
                                    giving return-code

                |"Articolo"
                move 1,1               to winprint-col-start
                move wprtalign-left    to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code 
                |"Descrizione"
                move 3,0               to winprint-col-start
                move wprtalign-left    to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code 
                |"Quantità"
                move 13                to winprint-col-start
                move wprtalign-center  to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code 
                |"Prezzo"
                move 14,8              to winprint-col-start
                move wprtalign-right   to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code 
                |"Totale"
                move 16,9              to winprint-col-start
                move wprtalign-right   to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code 
                |TAPPO
                move 19,3              to winprint-col-start
                move wprtalign-left    to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code

           when 70|RIGA
                call "WIN$PRINTER"  using winprint-set-data-columns,
                                          36, 42, 92, 99, 113, 127
                                    giving return-code

                |Cliente
                move 1,1               to winprint-col-start
                move wprtalign-left    to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code 
                |Articolo
                move 5,5               to winprint-col-start
                move wprtalign-center  to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code 
                |Descrizione
                move 7,5               to winprint-col-start
                move wprtalign-left    to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code 
                |Quantità
                move 13                to winprint-col-start
                move wprtalign-center  to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code 
                |Prezzo
                move 14,8              to winprint-col-start
                move wprtalign-right   to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code 
                |Totale
                move 16,9              to winprint-col-start
                move wprtalign-right   to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code 
                |TAPPO
                move 19,3             to winprint-col-start
                move wprtalign-left    to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code

           when 70,1|RIGA scorta <> 2 (non va il cliente)
                call "WIN$PRINTER"  using winprint-set-data-columns,
                                          36, 42, 92, 99, 113, 127
                                    giving return-code

                |Cliente
                move 1,1               to winprint-col-start
                move wprtalign-left    to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code 
                |Articolo
                move 1,0               to winprint-col-start
                move wprtalign-center  to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code 
                |Descrizione
                move 3,0               to winprint-col-start
                move wprtalign-left    to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code 
                |Quantità
                move 13                to winprint-col-start
                move wprtalign-center  to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code 
                |Prezzo
                move 14,8              to winprint-col-start
                move wprtalign-right   to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code 
                |Totale
                move 16,9              to winprint-col-start
                move wprtalign-right   to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code 
                |TAPPO
                move 19,3             to winprint-col-start
                move wprtalign-left    to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code

           when 70,5 |TOTALE
                call "WIN$PRINTER"  using winprint-set-data-columns,
                                          20
                                    giving return-code

                |"Totale giornaliero:"
                move 7,5               to winprint-col-start
                move wprtalign-left    to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code 
                |Totale
                move 14                to winprint-col-start
                move wprtalign-right   to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code 
                |TAPPO
                move 19,3             to winprint-col-start
                move wprtalign-left    to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code

           when 71 |RIGA MARCA
                call "WIN$PRINTER"  using winprint-set-data-columns,
                                          35
                                    giving return-code

                |Marca
                move 6,0               to winprint-col-start
                move wprtalign-left    to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code 
                |Valore
                move 11                to winprint-col-start
                move wprtalign-right   to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code 
                |TAPPO
                move 14,2              to winprint-col-start
                move wprtalign-left    to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code

           when 72 |DA QUI INIZIANO I SETTAGGI DI "LAB-CUMULATO"
                |"Marca" "Inevaso" "Fatturato" "%"
                call "WIN$PRINTER"  using winprint-set-data-columns,
                                          6, 13, 22
                                    giving return-code

                |"Marca"
                move 2,0               to winprint-col-start
                move wprtalign-left    to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code 
                |"Inevaso"
                move 9                 to winprint-col-start
                move wprtalign-center  to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code 
                |"Fatturato"
                move 12,5              to winprint-col-start
                move wprtalign-center  to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code 
                |"%"
                move 16                to winprint-col-start
                move wprtalign-center  to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code 
                |TAPPO
                move 18,0              to winprint-col-start
                move wprtalign-left    to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code

           when 73 |Descrizione Marca   %
                call "WIN$PRINTER"  using winprint-set-data-columns,
                                          31
                                    giving return-code

                |Descrizione marca
                move 2,0               to winprint-col-start
                move wprtalign-left    to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code 
                |%
                move 16                to winprint-col-start
                move wprtalign-center  to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code 
                |TAPPO
                move 18,0              to winprint-col-start
                move wprtalign-left    to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code

           when 73,5 |"Inevaso" "Fatturato"
                call "WIN$PRINTER"  using winprint-set-data-columns,
                                          16
                                    giving return-code

                |"Inevaso"
                move 9                 to winprint-col-start
                move wprtalign-center  to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code 
                |"Fatturato"
                move 12,5              to winprint-col-start
                move wprtalign-center  to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code 
                |TAPPO
                move 16                to winprint-col-start
                move wprtalign-center  to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code

           when 74 |"TOT INEVASO:" "TOT FATTURATO:" "%"
                call "WIN$PRINTER"  using winprint-set-data-columns,
                                          13, 27
                                    giving return-code

                |"TOT INEVASO:"
                move 4                 to winprint-col-start
                move wprtalign-center  to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code 
                |"TOT FATTURATO:"
                move 8,5               to winprint-col-start
                move wprtalign-center  to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code 

                |"%"
                move 13                to winprint-col-start
                move wprtalign-center  to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code 

                |TAPPO
                move 15,9              to winprint-col-start
                move wprtalign-center  to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code

           when 74,5 |TOTALE INEVASO  TOTALE FATTURATO  %
                call "WIN$PRINTER"  using winprint-set-data-columns,
                                          16, 31
                                    giving return-code

                |"TOT INEVASO:"
                move 4                 to winprint-col-start
                move wprtalign-center  to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code 
                |"TOT FATTURATO:"
                move 8,5               to winprint-col-start
                move wprtalign-center  to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code 

                |"%"
                move 13                to winprint-col-start
                move wprtalign-center  to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code 

                |TAPPO
                move 15,9              to winprint-col-start
                move wprtalign-center  to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code

           when 75 |DA QUI INIZIANO I SETTAGGI DI ST-ART-DET
                |"UTF" "PESO UTF" "NON UTF" E SEGUENTI
                call "WIN$PRINTER"  using winprint-set-data-columns,
                                          14, 22
                                    giving return-code

                |"UTF"
                move 3                 to winprint-col-start
                move wprtalign-center  to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code 
                |"Peso UTF"
                move 7                 to winprint-col-start
                move 1                 to winprint-col-indent
                move wprtalign-center  to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code 

                |"Non UTF"
                move 12                to winprint-col-start
                move wprtalign-center  to winprint-col-alignment
                move 1                 to winprint-col-indent
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code 

                |TAPPO
                move 17                to winprint-col-start
                move wprtalign-center  to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code

           when 76 |DA QUI INIZIANO I SETTAGGI DI ST-CLI-DET
                |RIGA DESTINI
                call "WIN$PRINTER"  using winprint-set-data-columns,
                                          101
                                    giving return-code

                move 1,5               to winprint-col-start
                move wprtalign-left    to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code 

           when 77 |DA QUI INIZIANO I SETTAGGI DI LAB-PESI
                   |"Articolo" "Cod Cliente" "Descrizione"
                   |"Ean" "Peso (Kg.)"
                call "WIN$PRINTER"  using winprint-set-data-columns,
                                          9, 20, 31, 34, 44
                                    giving return-code
                |"Articolo"
                move 1,1               to winprint-col-start
                move wprtalign-left    to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code 
                |"Cod Cliente"
                move 3,0               to winprint-col-start
                move wprtalign-left    to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code 
                |"Descrizione"
                move 6,1               to winprint-col-start
                move wprtalign-left    to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code 
                |"Ean"
                move 13,6              to winprint-col-start
                move wprtalign-center  to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code 
                |"Peso (Kg.)"
                move 16,7              to winprint-col-start
                move wprtalign-right   to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code
                |TAPPO
                move 19,35             to winprint-col-start
                move wprtalign-right   to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code 

           when 78 |Cod. Articolo |Cod Art Cli |art-desrizione |Ean
                call "WIN$PRINTER"  using winprint-set-data-columns,
                                          7, 22, 72, 85,
                                    giving return-code
                |Cod. Articolo
                move 1,1               to winprint-col-start
                move wprtalign-right   to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code
                |Cod Art Cli
                move 2,6               to winprint-col-start
                move 0,4               to winprint-col-indent
                move wprtalign-left    to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code
                move 0,0               to winprint-col-indent
                |Descrizione articolo
                move 5,8               to winprint-col-start
                move 0,3               to winprint-col-indent
                move wprtalign-left    to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code
                move 0,0               to winprint-col-indent
                |Ean
                move 13,5              to winprint-col-start
                move  0,1              to winprint-col-indent
                move wprtalign-center  to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code 
                move 0,0               to winprint-col-indent
                |TAPPO
                move 16,7              to winprint-col-start
                move wprtalign-center  to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code 


           when 78,5 |Peso
                call "WIN$PRINTER"  using winprint-set-data-columns,
                                          8
                                    giving return-code

                |Peso (Kg.)
                move 17,3              to winprint-col-start
                move wprtalign-right   to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code
                |TAPPO
                move 19,3              to winprint-col-start
                move wprtalign-right   to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code 

           when 79 |DA QUI INIZIANO I SETTAGGI DI LAB-ST-LISTINI
                call "WIN$PRINTER"  using winprint-set-data-columns,
                                          151
                                    giving return-code
                move 2,7               to winprint-col-start
                move wprtalign-left    to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code 
                |TAPPO
                move 19,4              to winprint-col-start
                move wprtalign-left    to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code 
           when 80|Riga Intestazione
                call "WIN$PRINTER"  using winprint-set-data-columns,
                         3, 7, 15, 22, 33, 36, 39, 44, 49, 52, 58, 77
                                    giving return-code
                |"N."
                move 0,1               to winprint-col-start
                move wprtalign-right   to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code

                |"Art."
                move 0,6               to winprint-col-start
                move wprtalign-right   to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code

                |"Val. dal"
                move 1,45              to winprint-col-start
                move wprtalign-center  to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code 

                |"Cod Cli"
                move 2,7               to winprint-col-start
                move wprtalign-center  to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code 
                |"Descrizione"
                move 4,25              to winprint-col-start
                move wprtalign-center  to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code 
                |"Imb"
                move 8,35              to winprint-col-start
                move wprtalign-center  to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code 
                |"Ean"
                move 9,00               to winprint-col-start
                move wprtalign-center  to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code 
                |"Prod."
                move 10,65             to winprint-col-start
                move wprtalign-right   to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code
                |"Cons."
                move 11,85             to winprint-col-start
                move wprtalign-right   to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code
                |"Cou"
                move 12,67             to winprint-col-start
                move wprtalign-right   to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code
                |"Prezzo"
                move 13,45             to winprint-col-start
                move wprtalign-right   to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code
                |"Prezzo Promozionale"
                move 14,66             to winprint-col-start
                move wprtalign-center  to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code
                |"Faro"
                move 18,4              to winprint-col-start
                move wprtalign-right   to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code
                |TAPPO
                move 19,4              to winprint-col-start
                move wprtalign-right   to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code 
           when 81|Riga
                call "WIN$PRINTER"  using winprint-set-data-columns,
                   4, 10, 18, 33, 80, 83, 96, 105, 113, 121, 126
                                    giving return-code
                |Numero
                move 0,1               to winprint-col-start
                move wprtalign-right   to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code 
      *          move 0,1               to winprint-col-indent

                |art-codice
                move 0,55              to winprint-col-start
                move wprtalign-right   to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code 
                |VAl. dal
                move 1,45              to winprint-col-start
                move wprtalign-center  to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code 
                |cod-cli
                move 2,7               to winprint-col-start
                move wprtalign-center  to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code 
                |art-descrizione
                move 4,25              to winprint-col-start
                move 0,1               to winprint-col-indent
                move wprtalign-left    to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code 
                move 0,0               to winprint-col-indent
                |Imb
                move 8,35              to winprint-col-start
                move wprtalign-center  to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code 
                |art-codice-ean-1
                move 9,00              to winprint-col-start
                move wprtalign-center  to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code 
                |Prod
                move 10,80             to winprint-col-start
                move wprtalign-right   to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code
                |Consumo
                move 11,85             to winprint-col-start
                move wprtalign-right   to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code
                |Cou
                move 12,67             to winprint-col-start
                move wprtalign-right   to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code

                |FILLER
                move 13,47             to winprint-col-start
                move wprtalign-right   to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code
                |TAPPO
                move 14,66             to winprint-col-start
                move wprtalign-center  to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code 
           when 81,5 |Prezzo in grassetto
                call "WIN$PRINTER"  using winprint-set-data-columns,
                                          11
                                    giving return-code
                |lst-prezzo
                move 13,45             to winprint-col-start
                move wprtalign-right   to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code
                |TAPPO
                move 14,66             to winprint-col-start
                move wprtalign-center  to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code
           when 82|Prezzo promo e Faro in rosso
                call "WIN$PRINTER"  using winprint-set-data-columns,
                                          51
                                    giving return-code
                |Prezzo Promozionale
                move 14,66             to winprint-col-start
                move wprtalign-center  to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code
                |Faro
                move 18,40             to winprint-col-start
                move wprtalign-right   to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code
                |TAPPO
                move 19,45             to winprint-col-start
                move wprtalign-right   to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code
           when 83|DA QUI INIZIANO I SETTAGGI DI LAB-ST-PROMO-P
                call "WIN$PRINTER"  using winprint-set-data-columns,
                                          9, 59, 72, 85, 92
                                    giving return-code
                |"Articolo"
                move 1,2               to winprint-col-start
                move wprtalign-right   to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code
                |"Descrizione"
                move 0,4               to winprint-col-indent
                move 2,45              to winprint-col-start
                move wprtalign-left    to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code
                move 0                 to winprint-col-indent

                |"Prz. Acquisto"
                move 10,0               to winprint-col-start
                move wprtalign-center   to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code        

                |"Prz. Vendita"
                move 14,0               to winprint-col-start
                move wprtalign-center   to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code

                |"Q.ta"
                move 17,0               to winprint-col-start
                move wprtalign-center   to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code

                |TAPPO
                move 19,00             to winprint-col-start
                move wprtalign-right   to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code


           when 83,5|DA QUI INIZIANO I SETTAGGI DI LAB-ST-PROMO-ALL-P
                call "WIN$PRINTER"  using winprint-set-data-columns,
                                          9, 59, 66, 73, 80, 87, 92
                                    giving return-code
                |"Art."
                move 0,1               to winprint-col-start
                move wprtalign-right   to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code
                |"Descrizione"
                move 0,2               to winprint-col-indent
                move 1,3               to winprint-col-start
                move wprtalign-left    to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code
                move 0                 to winprint-col-indent

                |"Prz Ven"
                move 5,20               to winprint-col-start
                move wprtalign-center   to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code        

                |"Prz Acq"
                move 6,05               to winprint-col-start
                move wprtalign-center   to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code

                |"Q.ta"
                move 6,90              to winprint-col-start
                move wprtalign-center   to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code

                |"Q.ta B"
                move 7,75               to winprint-col-start
                move wprtalign-center   to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code

                |"Prz f"
                move 8,60               to winprint-col-start
                move wprtalign-center   to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code

                |TAPPO
                move 10,05              to winprint-col-start
                move wprtalign-center   to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code 


           when 83,6
                call "WIN$PRINTER"  using winprint-set-data-columns,
                                          9, 59, 66, 73, 80, 87, 92
                                    giving return-code

                |"Articolo"
                move 10,05             to winprint-col-start
                move wprtalign-right   to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code
                |"Descrizione"
                move 0,2               to winprint-col-indent
                move 10,9              to winprint-col-start
                move wprtalign-left    to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code
                move 0                 to winprint-col-indent

                |"Prz Acq"
                move 14,7               to winprint-col-start
                move wprtalign-center   to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code        

                |"Prz Ven"
                move 15,70              to winprint-col-start
                move wprtalign-center   to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code

                |"Q.ta"
                move 16,7               to winprint-col-start
                move wprtalign-center   to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code

                |"Q.ta B"
                move 17,6               to winprint-col-start
                move wprtalign-center   to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code

                |"Prz f"
                move 18,30              to winprint-col-start
                move wprtalign-center   to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code

                |TAPPO
                move 19,55             to winprint-col-start
                move wprtalign-right   to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code

           when 83,7
                call "WIN$PRINTER"  using winprint-set-data-columns,
                                          7, 37, 47, 57, 64, 71, 81, 
                                    87, 117, 127, 137, 144, 151, 161
                                    giving return-code

                |"Art."
                move 0,1               to winprint-col-start
                move wprtalign-right   to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code
                |"Descrizione"
                move 0,2               to winprint-col-indent
                move 1,3               to winprint-col-start
                move wprtalign-left    to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code
                move 0                 to winprint-col-indent

                |"Prz Acq"
                move 5,20               to winprint-col-start
                move wprtalign-center   to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code        

                |"Prz Ven"
                move 6,05               to winprint-col-start
                move wprtalign-center   to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code

                |"Q.ta"
                move 6,90               to winprint-col-start
                move wprtalign-center   to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code

                |"Q.ta B"
                move 7,75               to winprint-col-start
                move wprtalign-center   to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code

                |"Prz fat"
                move 8,60               to winprint-col-start
                move wprtalign-center   to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code

                |"Art."
                move 10,05             to winprint-col-start
                move 0,05              to winprint-col-indent
                move wprtalign-right   to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code
                |"Descrizione"
                move 0,2               to winprint-col-indent
                move 10,9              to winprint-col-start
                move wprtalign-left    to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code
                move 0                 to winprint-col-indent

                |"Prz Acq"
                move 14,7               to winprint-col-start
                move wprtalign-center   to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code        

                |"Prz Ven"
                move 15,70               to winprint-col-start
                move wprtalign-center   to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code

                |"Q.ta"
                move 16,70              to winprint-col-start
                move wprtalign-center   to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code

                |"Q.ta B"
                move 17,60              to winprint-col-start
                move wprtalign-center   to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code

                |"Prz fat"
                move 18,30              to winprint-col-start
                move wprtalign-center   to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code

                |TAPPO
                move 19,55             to winprint-col-start
                move wprtalign-right   to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code

           when 83,8 |promozioni dal... 
                call "WIN$PRINTER"  using winprint-set-data-columns,
                                          36, 71
                                    giving return-code
                |"promo dal..."
                move 0,4               to winprint-col-start
                move wprtalign-center  to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code
                |"promo dal..."
                move 10,0              to winprint-col-start
                move wprtalign-center  to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code 
                |TAPPO
                move 19,5              to winprint-col-start
                move wprtalign-left    to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code

           when 83,9 |gdo intestazione
                call "WIN$PRINTER"  using winprint-set-data-columns,
                                          41,
                                    giving return-code
                |
                move 8,0               to winprint-col-start
                move wprtalign-center  to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code 
                |TAPPO
                move 12                to winprint-col-start
                move wprtalign-left    to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code

           when 84 |DA QUI INIZIANO I SETTAGGI DI LAB-SELLOUT
                call "WIN$PRINTER"  using winprint-set-data-columns,
                                          9, 20, 23, 33, 47, 60
                                    giving return-code
                |"Articolo"
                move 1,6               to winprint-col-start
                move wprtalign-right   to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code
                |"Descrizione"
                move 0,20              to winprint-col-indent
                move 3,00              to winprint-col-start
                move wprtalign-left    to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code
                move 0                 to winprint-col-indent

                |"Imb"
                move 10,7               to winprint-col-start
                move wprtalign-center   to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code

                |"Qtà Totali"
                move 11,5                to winprint-col-start
                move wprtalign-center    to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code

                |"Qtà Bollettate"
                move 13,4                to winprint-col-start
                move wprtalign-center    to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code

                |"Qtà Rimanenti"
                move 16,0                to winprint-col-start
                move wprtalign-center    to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code

                |"TAPPO"
                move 18,5               to winprint-col-start
                move wprtalign-center   to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code

           when 85 |Riga
                call "WIN$PRINTER"  using winprint-set-data-columns,
                                          7, 57, 60, 67, 74
                                    giving return-code
                |Articolo
                move 1,6               to winprint-col-start
                move wprtalign-right   to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code
                |Descrizione
                move 0,20              to winprint-col-indent
                move 3,00              to winprint-col-start
                move wprtalign-left    to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code
                move 0                 to winprint-col-indent

                |Qta
                move 10,7               to winprint-col-start
                move wprtalign-center   to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code

                |Prezzo
                move 11,5                to winprint-col-start
                move wprtalign-center    to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code

                |Qtà Bollettate
                move 13,4                to winprint-col-start
                move wprtalign-center    to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code

                |TAPPO
                move 16,0                to winprint-col-start
                move wprtalign-center    to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code

           when 85,5|Solo quantità in rosso
                call "WIN$PRINTER"  using winprint-set-data-columns,
                                          9
                                    giving return-code

                |Qta rimanente
                move 16,0                to winprint-col-start
                move wprtalign-center    to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code

                |"TAPPO"
                move 18,5               to winprint-col-start
                move wprtalign-center   to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code

           when 86|DA QUI INIZIANO I SETTAGGI DI LAB-FATCON
                call "WIN$PRINTER"  using winprint-set-data-columns,
                                          9, 20, 24, 30
                                    giving return-code
                |"Articolo"
                move 1,3               to winprint-col-start
                move wprtalign-right   to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code
                |"Descrizione"
                move 0,2               to winprint-col-indent
                move 2,7               to winprint-col-start
                move wprtalign-left    to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code
                move 0                 to winprint-col-indent

                |"Q.tà"
                move 7,3                to winprint-col-start
                move wprtalign-center   to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code        

                |"Prezzo"
                move 8,0                to winprint-col-start
                move wprtalign-center   to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code

                |TAPPO
                move 9,85              to winprint-col-start
                move wprtalign-right   to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code

           when 87|Riga
                call "WIN$PRINTER"  using winprint-set-data-columns,
                                          7, 37, 43, 53
                                    giving return-code
                |Articolo
                move 1,8               to winprint-col-start
                move wprtalign-right   to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code
                |Descrizione
                move 0,2               to winprint-col-indent
                move 2,7               to winprint-col-start
                move wprtalign-left    to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code
                move 0                 to winprint-col-indent

                |Q.tà
                move 7,3               to winprint-col-start
                move wprtalign-center   to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code        

                |Prezzo
                move 8,0                to winprint-col-start
                move wprtalign-center   to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code

                |TAPPO
                move 9,85              to winprint-col-start
                move wprtalign-right   to winprint-col-alignment
                call "WIN$PRINTER"  using winprint-set-page-column,
                                          winprint-column
                                   giving return-code

           end-evaluate.
