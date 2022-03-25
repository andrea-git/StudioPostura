       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      st-docum.
       AUTHOR.                          Andrea.
      ******************************************************************

       SPECIAL-NAMES. decimal-point is comma.
       INPUT-OUTPUT SECTION.                                            
       FILE-CONTROL.
           copy "clienti.sl".
           copy "tfatture.sl".
           copy "rfatture.sl".
           copy "tcorrisp.sl".
           copy "rcorrisp.sl".
           copy "tnotacr.sl".
           copy "rnotacr.sl".
           copy "param.sl".
           copy "codiva.sl".

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "clienti.fd".
           copy "tfatture.fd".
           copy "rfatture.fd".
           copy "tcorrisp.fd".
           copy "rcorrisp.fd".
           copy "tnotacr.fd".
           copy "rnotacr.fd".
           copy "param.fd".
           copy "codiva.fd".

       WORKING-STORAGE SECTION.
      * COPY   
       copy "acugui.def".
       copy "spooler.def".
       copy "fonts.def".
       copy "selprint.lks".

       77  status-clienti        pic xx.
       77  status-tfatture       pic xx.
       77  status-rfatture       pic xx.
       77  status-tcorrisp       pic xx.
       77  status-rcorrisp       pic xx.
       77  status-tnotacr        pic xx.
       77  status-rnotacr        pic xx.
       77  status-param          pic xx.
       77  status-codiva         pic xx.
       
      * COSTANTI
       78  titolo             value "Studio - Stampa Documenti Fiscali".
       78  78-sfondo-fattura    
           value "F:\StudioPostura\Resource\STAMPE\SFONDO_FATTURE.BMP".
       78  78-sfondo-corrisp
           value "F:\StudioPostura\Resource\STAMPE\SFONDO_CORRISP.BMP".
       78  78-sfondo-notacr
           value "F:\StudioPostura\Resource\STAMPE\SFONDO_NOTE.BMP".

       78  78-sfondo-fattura-local
           value "C:\StudioPostura\Resource\STAMPE\SFONDO_FATTURE.BMP".
       78  78-sfondo-corrisp-local
           value "C:\StudioPostura\Resource\STAMPE\SFONDO_CORRISP.BMP".
       78  78-sfondo-notacr-local
           value "C:\StudioPostura\Resource\STAMPE\SFONDO_NOTE.BMP".    

       78  78-sfondo-fattura-sconto value
           "F:\StudioPostura\Resource\STAMPE\SFONDO_FATTURE_SCONTO.BMP".
       78  78-sfondo-corrisp-sconto value
           "F:\StudioPostura\Resource\STAMPE\SFONDO_CORRISP_SCONTO.BMP".
       78  78-sfondo-notacr-sconto  value
           "F:\StudioPostura\Resource\STAMPE\SFONDO_NOTE_SCONTO.BMP".

       78  78-sfondo-fattura-local-sconto value
           "C:\StudioPostura\Resource\STAMPE\SFONDO_FATTURE_SCONTO.BMP".
       78  78-sfondo-corrisp-local-sconto value
           "C:\StudioPostura\Resource\STAMPE\SFONDO_CORRISP_SCONTO.BMP".
       78  78-sfondo-notacr-local-sconto  value
           "C:\StudioPostura\Resource\STAMPE\SFONDO_NOTE_SCONTO.BMP".
       78  78-passo              value 0,7.
       78  78-col-studio         value 5,65.

      * RIGHE PER LA STAMPA
       01  r-titolo              pic x(100).

       01  r-intestazione.
           05 r-numero           pic x(12).
           05 r-data             pic x(10).
           05 r-pagamento        pic x(20).
           05 r-iban             pic x(30).

       01  r-riga1.
           05 r-articolo         pic x(6).
           05 r-descrizione      pic x(50).
       01  r-riga2.
           05 r-qta              pic z.zzz.
           05 r-listino          PIC zz.zzz,zz.
           05 r-euro1            pic x(2).
           05 r-sconto           pic ---,--.
           05 r-sconto-perce     pic x(2).
      *****     05 r-prz              PIC zz.zzz,zz.
      *****     05 r-euro2            pic x(2).
           05 r-tot              PIC zz.zzz,zz.
           05 r-euro3            pic x(2).
           05 r-iva.
              10 r-iva1          pic x(2).
              10 r-perce         pic x.

       01  r-totali.
           05 r-importo-sub      pic zzz.zz9,99 blank zero.
           05 r-euro-sub         pic x(2) value " €".
           05 r-importo-iva      pic zzz.zz9,99 blank zero.     
           05 r-euro-iva         pic x(2) value " €".
           05 r-importo-ese      pic zzz.zz9,99 blank zero.     
           05 r-euro-ese         pic x(2) value " €".
           05 r-importo-tot      pic zzz.zz9,99 blank zero.     
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
       77  como-sconto           pic s9(3)v99.
       77  idx                   pic 999.
       77  iva-aliquota-z        pic zz.
       77  BitmapSfondoHandle    handle of bitmap.
       77  messaggio             pic x(150) value spaces.
       77  font-size-dply        pic z(5).      
       77  WFONT-STATUS          pic s9(5)  value zero.

       77  Calibri24             handle of font.
       77  Calibri20B            handle of font.
       77  Calibri14B            handle of font.
       77  Calibri16B            handle of font.
       77  Calibri12B            handle of font.
       77  Calibri10B            handle of font.
       77  Calibri11B            handle of font.
       77  Verdana10             handle of font.
       77  save-riga             pic 9(7)v99.
       77  save-altezza-pagina   pic 9(7)v99.

       77  tot-riga              pic 9(9)v99.
       77  filler                pic 9.
           88 trovato-sconto     value 1, false 0.


       LINKAGE SECTION.
       01  link-chiave.
           05 link-anno          pic 9(4).
           05 link-numero        pic 9(6).
       01  link-tipo             pic x.
           88 link-fattura       value "F".
           88 link-corrisp       value "C".
           88 link-notacr        value "N".
       77  link-stampante        pic x(200).

      ******************************************************************
       PROCEDURE DIVISION using link-chiave, link-tipo, link-stampante.

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
           set tutto-ok       to true.
           set trovato-sconto to false.
           set RecLocked      to false.
           set trovato        to false.

      ***---
       OPEN-FILES.
           open input codiva clienti param.
           if link-fattura
              open input tfatture rfatture
           end-if.
           if link-corrisp
              open input tcorrisp rcorrisp
           end-if.
           if link-notacr
              open input tnotacr rnotacr
           end-if.

      ***---
       ELABORAZIONE.
           if link-fattura
              move link-chiave to tfa-chiave
              read tfatture no lock
                   invalid set errori to true
              end-read
           end-if.
           if link-corrisp
              move link-chiave to tco-chiave
              read tcorrisp no lock
                   invalid set errori to true
               not invalid
                   move tco-chiave       to tfa-chiave
                   move tco-cliente      to tfa-cliente
                   move tco-data-doc     to tfa-data-doc
                   move tco-pagamento    to tfa-pagamento
                   move tco-iban         to tfa-iban
                   move tco-iva          to tfa-iva
                   move tco-importo-sub  to tfa-importo-sub
                   move tco-importo-iva  to tfa-importo-iva
                   move tco-importo-tot  to tfa-importo-tot
                   move tco-note         to tfa-note
              end-read
           end-if.
           if link-notacr
              move link-chiave to tno-chiave
              read tnotacr no lock
                   invalid set errori to true
               not invalid
                   move tno-chiave       to tfa-chiave
                   move tno-cliente      to tfa-cliente
                   move tno-data-doc     to tfa-data-doc
                   move tno-pagamento    to tfa-pagamento
                   move tno-iban         to tfa-iban
                   move tno-iva          to tfa-iva
                   move tno-importo-sub  to tfa-importo-sub
                   move tno-importo-iva  to tfa-importo-iva
                   move tno-importo-tot  to tfa-importo-tot
                   move tno-note         to tfa-note
              end-read
           end-if.
           if tutto-ok
              move tfa-cliente to cli-codice
              read clienti no lock
              move tfa-iva to iva-codice
              read codiva no lock
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
              
              if link-fattura
                 move low-value  to rfa-rec
                 move tfa-chiave to rfa-chiave-testa
                 start rfatture key >= rfa-chiave
                 perform until 1 = 2
                    read rfatture next at end exit perform end-read
                    if rfa-chiave-testa not = tfa-chiave
                       exit perform
                    end-if
                    if rfa-sconto not = 0
                       set trovato-sconto to true
                       exit perform
                    end-if
                 end-perform
              end-if
              
              if link-corrisp
                 move low-value  to rco-rec
                 move tco-chiave to rco-chiave-testa
                 start rcorrisp key >= rco-chiave
                 perform until 1 = 2
                    read rcorrisp next at end exit perform end-read
                    if rco-chiave-testa not = tco-chiave
                       exit perform
                    end-if
                    if rco-sconto not = 0
                       set trovato-sconto to true
                       exit perform
                    end-if
                 end-perform
              end-if
             
              perform INTESTAZIONE
              move 10,4 to save-riga
              
              if link-fattura
                 move low-value  to rfa-rec
                 move tfa-chiave to rfa-chiave-testa
                 start rfatture key >= rfa-chiave
                 perform until 1 = 2
                    read rfatture next at end exit perform end-read
                    if rfa-chiave-testa not = tfa-chiave
                       exit perform
                    end-if
                    perform SCRIVI-RIGA
                 end-perform
              end-if
              
              if link-corrisp
                 move low-value  to rco-rec
                 move tco-chiave to rco-chiave-testa
                 start rcorrisp key >= rco-chiave
                 perform until 1 = 2
                    read rcorrisp next at end exit perform end-read
                    if rco-chiave-testa not = tco-chiave
                       exit perform
                    end-if
                    move rco-articolo     to rfa-articolo
                    move rco-descrizione  to rfa-descrizione
                    move rco-qta          to rfa-qta        
                    move rco-listino      to rfa-listino
                    move rco-sconto       to rfa-sconto
                    move rco-prz          to rfa-prz        
                    perform SCRIVI-RIGA
                 end-perform
              end-if
              
              if link-notacr
                 move low-value  to rno-rec
                 move tno-chiave to rno-chiave-testa
                 start rnotacr key >= rno-chiave
                 perform until 1 = 2
                    read rnotacr next at end exit perform end-read
                    if rno-chiave-testa not = tno-chiave
                       exit perform
                    end-if
                    move rno-articolo     to rfa-articolo
                    move rno-descrizione  to rfa-descrizione
                    move rno-qta          to rfa-qta
                    |UGUALI PERCHE' NON E' PREVISTO SCONTO
                    |E SE ANCHE CI FOSSE UNA DIMINUZIONE VIENE ESPOSTA
                    |SOLO QUELLA CHE VIENE USATA PER LA BASE IMPONIBILE
                    move rno-prz          to rfa-prz
                    move rno-prz          to rfa-listino
                    perform SCRIVI-RIGA
                 end-perform
              end-if

              move Calibri16B to spl-hfont
              move 3          to spl-tipo-colonna
              move 23         to save-riga
              if tfa-importo-iva = 0  
                 move 0               to r-importo-sub
                 move spaces          to r-euro-sub

                 move tfa-importo-iva to r-importo-iva
                 move spaces          to r-euro-iva

                 move tfa-importo-sub to r-importo-ese
                 move " €"            to r-euro-ese
              else
                 move tfa-importo-sub to r-importo-sub
                 move " €"            to r-euro-sub

                 move tfa-importo-iva to r-importo-iva
                 move " €"            to r-euro-iva

                 move 0               to r-importo-ese
                 move spaces          to r-euro-ese
              end-if
              move tfa-importo-tot to r-importo-tot
              move r-totali        to spl-riga-stampa
              perform SCRIVI
              
              move Calibri12B to spl-hfont
              move 24,60 to save-riga
              perform varying idx from 1 by 1
                        until idx > 5
                 move 0             to spl-tipo-colonna
                 move 1,0           to spl-colonna
                 move tfa-nota(idx) to spl-riga-stampa
                 perform SCRIVI
                 subtract 0,15 from save-riga
              end-perform

              set spl-chiusura to true
              call   "spooler" using spooler-link

           end-if.

      ***---
       SCRIVI-RIGA. 
           if rfa-listino = 0
              move rfa-prz to rfa-listino
           end-if.
           move Calibri10B to spl-hfont.
           move 2 to spl-tipo-colonna.
           move rfa-articolo    to r-articolo.
           move rfa-descrizione to r-descrizione.
           move r-riga1         to spl-riga-stampa.
           perform SCRIVI.            
           subtract 78-passo from save-riga.
              
           move Calibri11B to spl-hfont
           move 2,5 to spl-tipo-colonna
           if rfa-qta = 0
              compute tot-riga = rfa-prz
           else
              compute tot-riga = rfa-prz * rfa-qta
           end-if.
           if tot-riga = 0
              move spaces to r-euro1 
      *****        move spaces to r-euro2 
              move spaces to r-euro3 
              move spaces to r-perce 
              move spaces to r-iva1
           else                           
              if rfa-listino not = 0
                 move " €"         to r-euro1 
              end-if
      *****        move " €"         to r-euro2 
              move " €"         to r-euro3 
              if iva-aliquota = 0
                 move cli-iva-esente to r-iva
              else
                 move iva-aliquota   to iva-aliquota-z
                 move iva-aliquota-z to r-iva1
                 move "%"            to r-perce 
              end-if              
           end-if.                                  
           move rfa-qta         to r-qta.
           move rfa-listino     to r-listino.
           if rfa-sconto = 0
              move 0      to r-sconto
              move spaces to r-sconto-perce
           else
              compute como-sconto = rfa-sconto * -1
              move como-sconto  to r-sconto
              move " %"         to r-sconto-perce
           end-if.
      *****     move rfa-prz         to r-prz.
           move tot-riga        to r-tot.
           move r-riga2         to spl-riga-stampa.
           perform SCRIVI.
           add 0,1 to save-riga.

      ***---
       INTESTAZIONE.
           set spl-bitmap to true.
           move 4,2 to spl-colonna.
           move 3,5 to spl-riga.
                            
           move 0 to BitmapSfondoHandle.                             
           evaluate true
           when link-fattura
                if trovato-sconto                               
                   call "W$BITMAP" using WBITMAP-LOAD, 
                                         78-sfondo-fattura-sconto
                                  giving BitmapSfondoHandle
                   if BitmapSfondoHandle > 99999
                      call "W$BITMAP" using WBITMAP-LOAD, 
                                         78-sfondo-fattura-local-sconto,
                                     giving BitmapSfondoHandle
                   end-if
                else     
                   call "W$BITMAP" using WBITMAP-LOAD, 78-sfondo-fattura
                                  giving BitmapSfondoHandle
                   if BitmapSfondoHandle > 99999
                      call "W$BITMAP" using WBITMAP-LOAD, 
                                            78-sfondo-fattura-local,
                                     giving BitmapSfondoHandle
                   end-if                                     
                end-if
           when link-corrisp
                if trovato-sconto
                   call "W$BITMAP" using WBITMAP-LOAD, 
                                         78-sfondo-corrisp-sconto,
                                  giving BitmapSfondoHandle
                   if BitmapSfondoHandle > 99999
                      call "W$BITMAP" using WBITMAP-LOAD, 
                                         78-sfondo-corrisp-local-sconto,
                                     giving BitmapSfondoHandle
                    end-if
                else          
                   call "W$BITMAP" using WBITMAP-LOAD, 
                                         78-sfondo-corrisp,
                                  giving BitmapSfondoHandle
                   if BitmapSfondoHandle > 99999
                      call "W$BITMAP" using WBITMAP-LOAD, 
                                            78-sfondo-corrisp-local,
                                     giving BitmapSfondoHandle
                    end-if
                end-if
           when link-notacr
                call "W$BITMAP" using WBITMAP-LOAD, 78-sfondo-notacr,
                               giving BitmapSfondoHandle
                if BitmapSfondoHandle > 99999
                   call "W$BITMAP" using WBITMAP-LOAD, 
                                         78-sfondo-notacr-local,
                                  giving BitmapSfondoHandle
                end-if
           end-evaluate.

           move BitmapSfondoHandle   to spl-hbitmap.

           move 27,4 to spl-bitmap-height.
           move 19,4 to spl-bitmap-width.

           call "spooler" using spooler-link.

           move spaces to prm-chiave.
           read param  no lock.
                                                
           move 0                  to spl-tipo-colonna
           move 78-col-studio      to spl-colonna.
           move 0,6                to save-riga.
           move prm-ragsoc         to spl-riga-stampa.
           move Calibri16B         to spl-hfont.
           perform SCRIVI.
           
           move Verdana10          to spl-hfont.
           move 78-col-studio      to spl-colonna.
           move 1,5                to save-riga.
           move prm-indirizzo to spl-riga-stampa.                       
           perform SCRIVI.                                              
                        
           
           move 78-col-studio      to spl-colonna.
           move 2,2                to save-riga.                        
           inspect prm-citta     replacing trailing spaces by low-value.
           inspect prm-cap       replacing trailing spaces by low-value.
           initialize spl-riga-stampa.
           string prm-cap       delimited low-value
                  "  -  "       delimited size
                  prm-citta     delimited low-value
                  " ("          delimited size
                  prm-prov      delimited size
                  ")"           delimited size
                  into spl-riga-stampa
           end-string.
           perform SCRIVI.
           
           move 78-col-studio      to spl-colonna.
           move 2,9                to save-riga.
           initialize spl-riga-stampa.
           string "C.F./P.IVA:  " delimited size
                  prm-codfis      delimited size
                  into spl-riga-stampa
           end-string.
           perform SCRIVI.
           
           move 78-col-studio      to spl-colonna.
           move 3,6                to save-riga.
           inspect prm-cciaa    replacing trailing spaces by low-value.
           initialize spl-riga-stampa.
           string "C.C.I.A.A.:    " delimited size
                  prm-cciaa         delimited low-value
                  into spl-riga-stampa
           end-string.
           perform SCRIVI.
                                   
           move 1,5                to spl-tipo-colonna.
           move 3,3                to save-riga.
           initialize spl-riga-stampa.
           inspect cli-ragsoc replacing trailing spaces by low-value.
           string  cli-ragsoc delimited low-value
                   " "        delimited size
                   cli-nome   delimited size
                   into spl-riga-stampa
           end-string.
           perform SCRIVI.

           inspect cli-indirizzo replacing trailing spaces by low-value.
           inspect cli-localita  replacing trailing spaces by low-value.
                                   
           move Calibri10B         to spl-hfont.
           move 4,1                to save-riga.
           initialize spl-riga-stampa.
           string  cli-localita  delimited low-value
                   " ("          delimited size
                   cli-prov      delimited size
                   ")"           delimited size
                   into spl-riga-stampa
           end-string.
           perform SCRIVI.
                                                  
           move 4,7                to save-riga.
           initialize spl-riga-stampa.
           string  cli-indirizzo delimited low-value
                   "  -  "       delimited size
                   cli-cap       delimited size
                   into spl-riga-stampa
           end-string.
           perform SCRIVI.
                                                  
           move 5,3                to save-riga.
           initialize spl-riga-stampa.
           string "P.IVA:   "   delimited size
                  cli-piva      delimited low-value
                  into spl-riga-stampa
           end-string.
           perform SCRIVI.
                                                  
           move 5,9                 to save-riga.
           initialize spl-riga-stampa.
           string "C.F.:      " delimited size
                  cli-codfis    delimited size
                  into spl-riga-stampa
           end-string.
           perform SCRIVI.
                                         
           move Calibri14B  to spl-hfont.
           move 8,35        to save-riga.
           move 1           to spl-tipo-colonna.
           
           initialize             r-numero.
           move tfa-numero     to r-numero.
           call "C$JUSTIFY" using r-numero, "L".
           inspect r-numero replacing leading x"30" by x"20".
           call "C$JUSTIFY" using r-numero, "L".
           inspect r-numero replacing trailing spaces by low-value.
           string  r-numero           delimited low-value
                   "/"                delimited size        
                  tfa-data-doc(1:4)   delimited size
                  into r-numero
           end-string.                                       
           inspect r-numero replacing trailing low-value by spaces.


           string tfa-data-doc(7:2) delimited size
                  "/"               delimited size
                  tfa-data-doc(5:2) delimited size
                  "/"               delimited size
                  tfa-data-doc(1:4) delimited size
                  into r-data
           end-string.
           move tfa-pagamento to r-pagamento.
           if tfa-pagamento = "Bonifico"
              move tfa-iban   to r-iban
           end-if.
           move r-intestazione to spl-riga-stampa.
           perform SCRIVI.
      *****
      *****     perform STAMPA-FRAMES.
      *****
      *****     perform STAMPA-LINEA.
      *****     perform STAMPA-PIE-DI-PAGINA.
      *****          
      *****     move 1,7        to spl-colonna.
      *****     move 1,8        to save-riga.
      *****     move Calibri16B to spl-hfont.
      *****     
      *****     initialize spl-riga-stampa.
      *****     inspect cli-ragsoc replacing trailing spaces by low-value.
      *****     string  cli-ragsoc delimited low-value
      *****             " "        delimited size
      *****             cli-nome   delimited size
      *****             into spl-riga-stampa
      *****     end-string.
      *****     perform SCRIVI.
      *****     add 78-passo-intestazione to save-riga.
      *****     add 0,1                   to save-riga.
      *****                                  
      *****     move Calibri12B to spl-hfont.
      *****     initialize spl-riga-stampa
      *****     inspect cli-indirizzo replacing trailing spaces by low-value.
      *****     inspect cli-localita  replacing trailing spaces by low-value.
      *****
      *****     string  "INDIRIZZO: " delimited size
      *****             cli-indirizzo delimited low-value
      *****             "  -  "       delimited size
      *****             "LOCALITA': " delimited size
      *****             cli-localita  delimited low-value
      *****             " ("          delimited size
      *****             cli-prov      delimited size
      *****             ")"           delimited size
      *****             into spl-riga-stampa
      *****     end-string.
      *****     perform SCRIVI.
      *****     add 78-passo-intestazione to   save-riga.
      *****     subtract 0,1              from save-riga.
      *****
      *****     initialize spl-riga-stampa.
      *****     inspect cli-email     replacing trailing spaces by low-value.
      *****     inspect cli-tel       replacing trailing spaces by low-value.
      *****     inspect cli-cell      replacing trailing spaces by low-value.
      *****     inspect cli-fax       replacing trailing spaces by low-value.
      *****
      *****     string  "EMAIL: "     delimited size
      *****             cli-email     delimited low-value
      *****             "  -  "       delimited size
      *****             "TEL: "       delimited size
      *****             cli-tel       delimited low-value
      *****             "  -  "       delimited size
      *****             "MOBILE: "    delimited size
      *****             cli-cell      delimited low-value
      *****             "  -  "       delimited size
      *****             "FAX: "       delimited size
      *****             cli-fax       delimited low-value
      *****             into spl-riga-stampa
      *****     end-string.
      *****     perform SCRIVI.
      *****     
      *****     move 5,15   to save-riga.
      *****     move "DATA" to spl-riga-stampa.
      *****     move 2,12   to spl-colonna.
      *****     perform SCRIVI.
      *****     subtract 78-passo from save-riga.
      *****                                       
      *****     move "ART."     to spl-riga-stampa.
      *****     move 4,23       to spl-colonna.
      *****     perform SCRIVI.
      *****     subtract 78-passo from save-riga.
      *****                               
      *****     move "DESCRIZIONE" to spl-riga-stampa.
      *****     move 9,6           to spl-colonna.
      *****     perform SCRIVI.
      *****     subtract 78-passo from save-riga.
      *****                               
      *****     move "QTA"  to spl-riga-stampa.
      *****     move 16,22  to spl-colonna.
      *****     perform SCRIVI.
      *****     subtract 78-passo from save-riga.
      *****     
      *****     move "PREZZO" to spl-riga-stampa.
      *****     move 17,4     to spl-colonna.
      *****     perform SCRIVI.
      *****     subtract 78-passo from save-riga.
      *****     
      *****     move 0   to num-righe.
      *****     move 5,8 to save-riga.

      ***---
       SCRIVI.
           add  78-passo      to save-riga.
           move save-riga     to spl-riga.
           set  spl-stringa   to true.
           call "spooler"  using spooler-link.

      ***---
       CARICA-FONT.
      * Calibri 24
           initialize wfont-data Calibri24.
           move 24 to wfont-size.
           move "Calibri"            to wfont-name.
           set  wfcharset-dont-care  to true.
           set  wfont-bold           to false.
           set  wfont-italic         to false.
           set  wfont-underline      to false.
           set  wfont-strikeout      to false.
           set  wfont-fixed-pitch    to false.
           move 0                    to wfont-char-set.
           set  wfdevice-win-printer to true. |E' un carattere per la stampante
           call "W$FONT" using wfont-get-font, Calibri24, wfont-data
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

      * Calibri 10B
           initialize wfont-data Calibri10B.
           move 10 to wfont-size.
           move "Calibri"            to wfont-name.
           set  wfcharset-dont-care  to true.
           set  wfont-bold           to true.
           set  wfont-italic         to false.
           set  wfont-underline      to false.
           set  wfont-strikeout      to false.
           set  wfont-fixed-pitch    to false.
           move 0                    to wfont-char-set.
           set  wfdevice-win-printer to true. |E' un carattere per la stampante
           call "W$FONT" using wfont-get-font, Calibri10B, wfont-data
                        giving wfont-status.

      * ISACCO (QUESTI TEST CONTROLLANO L'ESISTENZA DEL FONT)
           if wfont-status not = 1
              set errori to true
              perform MESSAGGIO-ERR-FONT
              exit paragraph
           end-if. 
                                           
      * Verdana 10
           initialize wfont-data Verdana10.
           move 10 to wfont-size.
           move "Verdana"            to wfont-name.
           set  wfcharset-dont-care  to true.
           set  wfont-bold           to true.
           set  wfont-italic         to false.
           set  wfont-underline      to false.
           set  wfont-strikeout      to false.
           set  wfont-fixed-pitch    to false.
           move 0                    to wfont-char-set.
           set  wfdevice-win-printer to true. |E' un carattere per la stampante
           call "W$FONT" using wfont-get-font, Verdana10, wfont-data
                        giving wfont-status.

      * ISACCO (QUESTI TEST CONTROLLANO L'ESISTENZA DEL FONT)
           if wfont-status not = 1
              set errori to true
              perform MESSAGGIO-ERR-FONT
              exit paragraph
           end-if. 

      * Calibri 11B
           initialize wfont-data Calibri11B.
           move 11 to wfont-size.
           move "Calibri"            to wfont-name.
           set  wfcharset-dont-care  to true.
           set  wfont-bold           to true.
           set  wfont-italic         to false.
           set  wfont-underline      to false.
           set  wfont-strikeout      to false.
           set  wfont-fixed-pitch    to false.
           move 0                    to wfont-char-set.
           set  wfdevice-win-printer to true. |E' un carattere per la stampante
           call "W$FONT" using wfont-get-font, Calibri11B, wfont-data
                        giving wfont-status.

      * ISACCO (QUESTI TEST CONTROLLANO L'ESISTENZA DEL FONT)
           if wfont-status not = 1
              set errori to true
              perform MESSAGGIO-ERR-FONT
              exit paragraph
           end-if. 

      * Calibri 20B
           initialize wfont-data Calibri20B.
           move 20 to wfont-size.
           move "Calibri"            to wfont-name.
           set  wfcharset-dont-care  to true.
           set  wfont-bold           to true.
           set  wfont-italic         to false.
           set  wfont-underline      to false.
           set  wfont-strikeout      to false.
           set  wfont-fixed-pitch    to false.
           move 0                    to wfont-char-set.
           set  wfdevice-win-printer to true. |E' un carattere per la stampante
           call "W$FONT" using wfont-get-font, Calibri20B, wfont-data
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
           close clienti param codiva.
           if link-fattura
              close tfatture rfatture
           end-if.
           if link-corrisp
              close tcorrisp rcorrisp
           end-if.
           if link-notacr
              close tnotacr rnotacr
           end-if.

      ***---
       EXIT-PGM.
           set environment "PRINTER" to "-P SPOOLER-DIRECT".

           destroy Calibri24.
           destroy Calibri20B.
           destroy Calibri16B.
           destroy Calibri14B.
           destroy Calibri12B.
           destroy Calibri10B.
           destroy Calibri11B.

           cancel "spooler".
           initialize spooler-link.
           goback.
