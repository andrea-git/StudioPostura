       IDENTIFICATION DIVISION.
       PROGRAM-ID.                      genfiles.
       SPECIAL-NAMES. decimal-point is comma.
      ******************************************************************

       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           copy "articoli.sl".
           copy "clienti.sl".
           copy "codiva.sl".
           copy "param.sl".
           copy "rlistini.sl".
           copy "tipocli.sl".
           copy "tlistini.sl".     
           copy "tschede.sl".
           copy "rschede.sl".
           copy "tfatture.sl".
           copy "rfatture.sl".
           copy "tcorrisp.sl".
           copy "rcorrisp.sl".
           copy "tnotacr.sl".
           copy "rnotacr.sl".
           copy "appuntamenti.sl".    
      *
       SELECT FBLOCK
           ASSIGN       TO DISK "FBLOCK"
           ORGANIZATION IS INDEXED
           ACCESS MODE  IS DYNAMIC
           LOCK MODE    IS AUTOMATIC WITH LOCK ON RECORD 
           FILE STATUS  IS STATO-IO
           RECORD KEY   IS FB-PRI-KEY.
      *

      *****************************************************************
       DATA DIVISION.
       FILE SECTION.
           copy "articoli.fd".
           copy "clienti.fd".
           copy "codiva.fd".
           copy "param.fd".
           copy "rlistini.fd".
           copy "tipocli.fd".
           copy "tlistini.fd".     
           copy "tschede.fd".
           copy "rschede.fd". 
           copy "tfatture.fd".
           copy "rfatture.fd".
           copy "tcorrisp.fd".
           copy "rcorrisp.fd".
           copy "tnotacr.fd".
           copy "rnotacr.fd".
           copy "appuntamenti.fd". 
  
       FD  FBLOCK
           LABEL RECORD IS STANDARD.
       01  REC-FBLOCK.
           05 FB-PRI-KEY.
              10 FB-PROG-ID    PIC  X(15).
              10 FB-DATA       PIC  9(8).
              10 FB-ORA        PIC  9(8).
           05 FB-HND-WIN       PIC S9(9).
       
       WORKING-STORAGE SECTION.
           COPY "acucobol.def".

       77  status-articoli      pic xx.
       77  status-clienti       pic xx.
       77  status-codiva        pic xx.
       77  status-param         pic xx.
       77  status-rlistini      pic xx.
       77  status-tipocli       pic xx.
       77  status-tlistini      pic xx.
       77  status-tschede       pic xx.
       77  status-rschede       pic xx.
       77  status-tfatture      pic xx.
       77  status-rfatture      pic xx.
       77  status-tcorrisp      pic xx.
       77  status-rcorrisp      pic xx.
       77  status-tnotacr       pic xx.
       77  status-rnotacr       pic xx.
       77  status-appuntamenti  pic xx.  
       77  stato-io             pic xx.  

       78  titolo            value "Generazione files".

       LINKAGE SECTION.
       77  link-status       signed-short.

      ******************************************************************
       PROCEDURE DIVISION USING link-status.
       DECLARATIVES.
      ***---
       ARTICOLI-ERR SECTION.
           use after error procedure on ARTICOLI.
           evaluate status-ARTICOLI
           when "35" continue
           when "39"
                display message "File [ARTICOLI] Mismatch size!"
                           title titolo
                            icon 3
                
           when "98"
                display message "[ARTICOLI] Indexed file corrupt!"
                           title titolo
                            icon 3
                
           end-evaluate.      

      ***---
       RLISTINI-ERR SECTION.
           use after error procedure on RLISTINI.
           evaluate status-RLISTINI
           when "35" continue
           when "39"
                display message "File [RLISTINI] Mismatch size!"
                           title titolo
                            icon 3
                
           when "98"
                display message "[RLISTINI] Indexed file corrupt!"
                           title titolo
                            icon 3
                
           end-evaluate.      

      ***---
       TIPOCLI-ERR SECTION.
           use after error procedure on TIPOCLI.
           evaluate status-TIPOCLI
           when "35" continue
           when "39"
                display message "File [TIPOCLI] Mismatch size!"
                           title titolo
                            icon 3
                
           when "98"
                display message "[TIPOCLI] Indexed file corrupt!"
                           title titolo
                            icon 3
                
           end-evaluate.    

      ***---
       TLISTINI-ERR SECTION.
           use after error procedure on TLISTINI.
           evaluate status-TLISTINI
           when "35" continue
           when "39"
                display message "File [TLISTINI] Mismatch size!"
                           title titolo
                            icon 3
                
           when "98"
                display message "[TLISTINI] Indexed file corrupt!"
                           title titolo
                            icon 3
                
           end-evaluate.    

      ***---
       CODIVA-ERR SECTION.
           use after error procedure on CODIVA.
           evaluate status-CODIVA
           when "35" continue
           when "39"
                display message "File [CODIVA] Mismatch size!"
                           title titolo
                            icon 3
                
           when "98"
                display message "[CODIVA] Indexed file corrupt!"
                           title titolo
                            icon 3
                
           end-evaluate.    

      ***---
       PARAM-ERR SECTION.
           use after error procedure on PARAM.
           evaluate status-PARAM
           when "35" continue
           when "39"
                display message "File [PARAM] Mismatch size!"
                           title titolo
                            icon 3
                
           when "98"
                display message "[PARAM] Indexed file corrupt!"
                           title titolo
                            icon 3
                
           end-evaluate.    

      ***---
       CLIENTI-ERR SECTION.
           use after error procedure on CLIENTI.
           evaluate status-CLIENTI
           when "35" continue
           when "39"
                display message "File [CLIENTI] Mismatch size!"
                           title titolo
                            icon 3
                
           when "98"
                display message "[CLIENTI] Indexed file corrupt!"
                           title titolo
                            icon 3
                
           end-evaluate.      

      ***---
       TSCHEDE-ERR SECTION.
           use after error procedure on TSCHEDE.
           evaluate status-tschede
           when "35" continue
           when "39"
                display message "File [TSCHEDE] Mismatch size!"
                           title titolo
                            icon 3
                
           when "98"
                display message "[TSCHEDE] Indexed file corrupt!"
                           title titolo
                            icon 3
                
           end-evaluate.  

      ***---
       RSCHEDE-ERR SECTION.
           use after error procedure on RSCHEDE.
           evaluate status-rschede
           when "35" continue
           when "39"
                display message "File [RSCHEDE] Mismatch size!"
                           title titolo
                            icon 3
                
           when "98"
                display message "[RSCHEDE] Indexed file corrupt!"
                           title titolo
                            icon 3
                
           end-evaluate.  

      ***---
       TFATTURE-ERR SECTION.
           use after error procedure on tfatture.
           evaluate status-tfatture
           when "35" continue
           when "39"
                display message "File [TFATTURE] Mismatch size!"
                           title titolo
                            icon 3
                
           when "98"
                display message "[TFATTURE] Indexed file corrupt!"
                           title titolo
                            icon 3
                
           end-evaluate.  

      ***---
       RFATTURE-ERR SECTION.
           use after error procedure on RFATTURE.
           evaluate status-rfatture
           when "35" continue
           when "39"
                display message "File [RFATTURE] Mismatch size!"
                           title titolo
                            icon 3
                
           when "98"
                display message "[RFATTURE] Indexed file corrupt!"
                           title titolo
                            icon 3
                
           end-evaluate.  

      ***---
       TCORRISP-ERR SECTION.
           use after error procedure on tcorrisp.
           evaluate status-tcorrisp
           when "35" continue
           when "39"
                display message "File [TCORRISP] Mismatch size!"
                           title titolo
                            icon 3
                
           when "98"
                display message "[TCORRISP] Indexed file corrupt!"
                           title titolo
                            icon 3
                
           end-evaluate.  

      ***---
       RCORRISP-ERR SECTION.
           use after error procedure on rcorrisp.
           evaluate status-rcorrisp
           when "35" continue
           when "39"
                display message "File [RCORRISP] Mismatch size!"
                           title titolo
                            icon 3
                
           when "98"
                display message "[RCORRISP] Indexed file corrupt!"
                           title titolo
                            icon 3
                
           end-evaluate.  

      ***---
       TNOTACR-ERR SECTION.
           use after error procedure on tnotacr.
           evaluate status-tnotacr
           when "35" continue
           when "39"
                display message "File [TNOTACR] Mismatch size!"
                           title titolo
                            icon 3
                
           when "98"
                display message "[TNOTACR] Indexed file corrupt!"
                           title titolo
                            icon 3
                
           end-evaluate.  

      ***---
       RNOTACR-ERR SECTION.
           use after error procedure on rnotacr.
           evaluate status-rnotacr
           when "35" continue
           when "39"
                display message "File [RNOTACR] Mismatch size!"
                           title titolo
                            icon 3
                
           when "98"
                display message "[RNOTACR] Indexed file corrupt!"
                           title titolo
                            icon 3
                
           end-evaluate.   

      ***---
       APPUNTAMENTI-ERR SECTION.
           use after error procedure on appuntamenti.
           evaluate status-appuntamenti
           when "35" continue
           when "39"
                display message "File [APPUNTAMENTI] Mismatch size!"
                           title titolo
                            icon 3
                
           when "98"
                display message "[APPUNTAMENTI] Indexed file corrupt!"
                           title titolo
                            icon 3
                
           end-evaluate.  

       END DECLARATIVES.

       MAIN-PRG.
           accept SYSTEM-INFORMATION from system-info.
           move 0 to link-status.

           open input articoli.
           if status-articoli = "35"
              open output articoli
              if status-articoli not = "00"
                 move -1 to link-status
              end-if
           end-if.
           close articoli.

           open input clienti.
           if status-clienti = "35"
              open output clienti
              if status-clienti not = "00"
                 move -1 to link-status
              end-if
           end-if.
           close clienti.

           open input codiva.
           if status-codiva = "35"
              open output codiva
              if status-codiva not = "00"
                 move -1 to link-status
              end-if
           end-if.
           close codiva.

           open input rlistini.
           if status-rlistini = "35"
              open output rlistini
              if status-rlistini not = "00"
                 move -1 to link-status
              end-if
           end-if.
           close rlistini.

           open input tipocli.
           if status-tipocli = "35"
              open output tipocli
              if status-tipocli not = "00"
                 move -1 to link-status
              end-if
           end-if.
           close tipocli.

           open input tlistini.
           if status-tlistini = "35"
              open output tlistini
              if status-tlistini not = "00"
                 move -1 to link-status
              end-if
           end-if.
           close tlistini.

           open input param.
           if status-param = "35"
              open output param
              if status-param not = "00"
                 move -1 to link-status
              end-if
           end-if.
           close param.

           open input clienti.
           if status-clienti = "35"
              open output clienti
              if status-clienti not = "00"
                 move -1 to link-status
              end-if
           end-if.       
           close clienti.    

           open input tschede.
           if status-tschede = "35"
              open output tschede
              if status-tschede not = "00"
                 move -1 to link-status
              end-if
           end-if.
           close tschede.    

           open input rschede.
           if status-rschede = "35"
              open output rschede
              if status-rschede not = "00"
                 move -1 to link-status
              end-if
           end-if.
           close rschede.    

           open input tfatture.
           if status-tfatture = "35"
              open output tfatture
              if status-tfatture not = "00"
                 move -1 to link-status
              end-if
           end-if.
           close tfatture.   

           open input rfatture.
           if status-rfatture = "35"
              open output rfatture
              if status-rfatture not = "00"
                 move -1 to link-status
              end-if
           end-if.
           close rfatture.   

           open input tcorrisp.
           if status-tcorrisp = "35"
              open output tcorrisp
              if status-tcorrisp not = "00"
                 move -1 to link-status
              end-if
           end-if.
           close tcorrisp.   

           open input rcorrisp.
           if status-rcorrisp = "35"
              open output rcorrisp
              if status-rcorrisp not = "00"
                 move -1 to link-status
              end-if
           end-if.
           close rcorrisp.   

           open input tnotacr.
           if status-tnotacr = "35"
              open output tnotacr
              if status-tnotacr not = "00"
                 move -1 to link-status
              end-if
           end-if.

           open input rnotacr.
           if status-rnotacr = "35"
              open output rnotacr
              if status-rnotacr not = "00"
                 move -1 to link-status
              end-if
           end-if.

           open input appuntamenti.
           if status-appuntamenti = "35"
              open output appuntamenti
              if status-appuntamenti not = "00"
                 move -1 to link-status
              end-if
           end-if.

           delete file fblock.
           open output fblock.
           close       fblock.

           goback.
