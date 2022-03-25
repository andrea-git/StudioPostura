      *{TOTEM}PRG-COMMENT
      * browser.Cbl
      * browser.Cbl is generated by TOTEM
      * This is a generated file. DO NOT modify this file directly.
      *{TOTEM}END
       IDENTIFICATION       DIVISION.
      *{TOTEM}PRGID
       PROGRAM-ID.          browser.
       AUTHOR.              Andrea.
       DATE-WRITTEN.        luned� 21 maggio 2012 11:05:20.
       REMARKS.
      *{TOTEM}END

       ENVIRONMENT          DIVISION.
       CONFIGURATION        SECTION.
       SPECIAL-NAMES.
      *{TOTEM}SPECIAL-NAME
      * <TOTEM:EPT. INIT:browser, INIT:browser, SpecialName>
      * <TOTEM:END>
      *{TOTEM}END
      *{TOTEM}ACTIVEX-DEF
      *{TOTEM}END
      *{TOTEM}DECIMAL-POINT
           DECIMAL-POINT IS COMMA.
      *{TOTEM}END

       INPUT-OUTPUT         SECTION.
       FILE-CONTROL.
      *{TOTEM}FILE-CONTROL
      *{TOTEM}END
       DATA                 DIVISION.
       FILE                 SECTION.
      *{TOTEM}FILE
      *{TOTEM}END

       WORKING-STORAGE      SECTION.
      *{TOTEM}ACU-DEF
               COPY "acugui.def".
               COPY "acucobol.def".
               COPY "fonts.def".
               COPY "crtvars.def".
               COPY "showmsg.def".
               COPY "totem.def".
               COPY "comune.def".
               COPY "custom.def".
               COPY "UTYDATA.DEF".
      *{TOTEM}END

      *{TOTEM}COPY-WORKING
      * Key Status
       77 Key-Status IS SPECIAL-NAMES CRT STATUS PIC 9(5) VALUE 0.
          88 Enter-Pushed VALUE 13.
          88 Exit-Pushed VALUE 27.
          88 Message-Received VALUE 95.
          88 Event-Occurred VALUE 96.
          88 Screen-No-Input-Field VALUE 97.
          88 Screen-Time-Out VALUE 99.
      * Properties & User defined Working Stoarge
       77 Screen1-Handle
                  USAGE IS HANDLE OF WINDOW.
       77 counter          PIC  9(3).
       77 path-bmp         PIC  X(512).
       77 como-path        PIC  X(512).
       77 CountChar        PIC  9(3).
       77 counter2         PIC  9(3).
       77 BitmapNumBrowse  PIC  99.
       77 BitmapNumSel     PIC  99.
       77 BitmapNumPrev    PIC  99.
       77 BitmapNumNext    PIC  99.
       78 BitmapBrowseEnabled VALUE IS 2. 
       78 BitmapBrowseDisabled VALUE IS 6. 
       78 BitmapSelEnabled VALUE IS 3. 
       78 BitmapSelDisabled VALUE IS 7. 
       78 BitmapPrevEnabled VALUE IS 4. 
       78 BitmapPrevDisabled VALUE IS 8. 
       78 BitmapNextEnabled VALUE IS 5. 
       78 BitmapNextDisabled VALUE IS 9. 
       77 Small-Font
                  USAGE IS HANDLE OF FONT SMALL-FONT.
       01 FlagSelezionato  PIC  9.
           88 selezionato VALUE IS 1    WHEN SET TO FALSE  0. 
       77 OPENSAVE-STATUS  PIC  s99.
       78 OPENSAVE-SUPPORTED VALUE IS 1. 
       78 OPENSAVE-OPEN-BOX VALUE IS 2. 
       78 OPENSAVE-SAVE-BOX VALUE IS 3. 
       78 OPENSAVE-BROWSE-FOLDER VALUE IS 4. 
       78 OPNSAVERR-UNSUPPORTED VALUE IS 0. 
       78 OPNSAVERR-CANCELLED VALUE IS -1. 
       78 OPNSAVERR-NO-MEMORY VALUE IS -2. 
       78 OPNSAVERR-NAME-TOO-LARGE VALUE IS -3. 
       01 OPENSAVE-DATA.
           03 OPNSAV-FILENAME  PIC  X(256).
           03 OPNSAV-FLAGS     PIC  9(4)
                      USAGE IS COMP-X
                      VALUE IS 0.
           03 OPNSAV-DEFAULT-EXT           PIC  X(12).
           03 OPNSAV-TITLE     PIC  X(80).
           03 OPNSAV-FILTERS   PIC  X(512).
           03 OPNSAV-DEFAULT-FILTER        PIC  9(4)
                      USAGE IS COMP-X
                      VALUE IS 0.
           03 OPNSAV-DEFAULT-DIR           PIC  X(128).
           03 OPNSAV-BASENAME  PIC  X(128).
       77 Screen1-Tb-1-Handle
                  USAGE IS HANDLE OF WINDOW.
       77 toolbar-browser-bmp          PIC  S9(9)
                  USAGE IS COMP-4
                  VALUE IS 0.
       77 Default-Font
                  USAGE IS HANDLE OF FONT DEFAULT-FONT.
       77 Form1-Handle
                  USAGE IS HANDLE OF WINDOW.
       77 e-sel            PIC  9
                  VALUE IS 1.
       77 e-sfoglia        PIC  9
                  VALUE IS 0.
       77 e-prev           PIC  9
                  VALUE IS 1.
       77 e-next           PIC  9
                  VALUE IS 0.
       77 Calibri14-Occidentale
                  USAGE IS HANDLE OF FONT.
       77 Traditional-Font
                  USAGE IS HANDLE OF FONT TRADITIONAL-FONT.
       77 LOGO_PICCOLO-BMP PIC  S9(9)
                  USAGE IS COMP-4
                  VALUE IS 0.

      ***********************************************************
      *   Code Gen's Buffer                                     *
      ***********************************************************
       77 STATUS-Form1-FLAG-REFRESH PIC  9.
          88 Form1-FLAG-REFRESH  VALUE 1 FALSE 0. 
           copy "lmresize.def".     
       77  Mio-Layout        handle of LAYOUT-MANAGER, LM-RESIZE.
      *{TOTEM}END

      *{TOTEM}ID-LOGICI
      ***** Elenco ID Logici *****
      ***** Fine ID Logici *****
      *{TOTEM}END

       LINKAGE          SECTION.
      *{TOTEM}LINKAGE
           COPY  "LINK-BROWSER.DEF".
      *{TOTEM}END

       SCREEN           SECTION.
      *{TOTEM}COPY-SCREEN
      * FORM
       01 
           Form1, 
           .

      * ENTRY FIELD
       05
           ef-path, 
           Entry-Field, 
           COL 2,30, 
           LINE 2,00,
           LINES 1,30 ,
           SIZE 89,50 ,
           BOXED,
           ID IS 5,
           HEIGHT-IN-CELLS,
           WIDTH-IN-CELLS,
           VALUE path-bmp,
           LAYOUT-DATA RLM-RESIZE-X-ANY
           AFTER PROCEDURE ef-path-AfterProcedure, 
           BEFORE PROCEDURE ef-path-BeforeProcedure, 
           .

      * WEB-BROWSER
       05
           web-img, 
           Web-Browser, 
           COL 2,30, 
           LINE 4,00,
           LINES 29,83 ,
           SIZE 89,50 ,
           ID IS 1,
           HEIGHT-IN-CELLS,
           WIDTH-IN-CELLS,
           VALUE path-bmp,
           LAYOUT-DATA RLM-RESIZE-BOTH-ANY
           .

      * LABEL
       05
           Screen1-Custom1-1, 
           Label, 
           COL 4,29, 
           LINE 1,15,
           LINES 0,31 ,
           SIZE 8,14 ,
           FONT IS Default-Font,
           ID IS 18,
           TRANSPARENT,
           TITLE "CUSTOM CONTROL",
           VISIBLE v-custom,
           .

      * TOOLBAR
       01
           Screen1-Tb-1,
           .    

      * PUSH BUTTON
       05
           pb-esci, 
           Push-Button, 
           COL 1,00, 
           LINE 1,09,
           LINES 64,00 ,
           SIZE 48,00 ,
           BITMAP-HANDLE TOOLBAR-BROWSER-BMP,
           BITMAP-NUMBER 1,
           UNFRAMED,
           SQUARE,
           EXCEPTION-VALUE 27,
           FLAT,
           ID IS 2,
           SELF-ACT,
           ESCAPE-BUTTON,
           TITLE "&Esci",
           LAYOUT-DATA RLM-RESIZE-BOTH-ANY
           .

      * PUSH BUTTON
       05
           pb-sfoglia, 
           Push-Button, 
           COL 6,40, 
           LINE 1,09,
           LINES 64,00 ,
           SIZE 48,00 ,
           BITMAP-HANDLE TOOLBAR-BROWSER-BMP,
           UNFRAMED,
           SQUARE,
           ENABLED e-sfoglia,
           EXCEPTION-VALUE 1000,
           FLAT,
           ID IS 3,
           SELF-ACT,
           TITLE "&Sfoglia",
           BITMAP-NUMBER BitmapNumBrowse
           LAYOUT-DATA RLM-RESIZE-BOTH-ANY
           .

      * PUSH BUTTON
       05
           pb-sel, 
           Push-Button, 
           COL 11,80, 
           LINE 1,09,
           LINES 64,00 ,
           SIZE 48,00 ,
           BITMAP-HANDLE TOOLBAR-BROWSER-BMP,
           UNFRAMED,
           SQUARE,
           ENABLED e-sel,
           EXCEPTION-VALUE 1001,
           FLAT,
           ID IS 4,
           SELF-ACT,
           TITLE "Salva l'immagine selezionata",
           BITMAP-NUMBER BitmapNumSel
           LAYOUT-DATA RLM-RESIZE-BOTH-ANY
           .

      * PUSH BUTTON
       05
           PB-PREV, 
           Push-Button, 
           COL 17,20, 
           LINE 1,09,
           LINES 64,00 ,
           SIZE 48,00 ,
           BITMAP-HANDLE TOOLBAR-BROWSER-BMP,
           UNFRAMED,
           SQUARE,
           ENABLED e-prev,
           EXCEPTION-VALUE 67,
           FLAT,
           ID IS 6,
           SELF-ACT,
           TITLE "Precedente",
           BITMAP-NUMBER BitmapNumPrev
           LAYOUT-DATA RLM-RESIZE-BOTH-ANY
           .

      * PUSH BUTTON
       05
           PB-NEXT, 
           Push-Button, 
           COL 22,60, 
           LINE 1,09,
           LINES 64,00 ,
           SIZE 48,00 ,
           BITMAP-HANDLE TOOLBAR-BROWSER-BMP,
           UNFRAMED,
           SQUARE,
           ENABLED e-next,
           EXCEPTION-VALUE 68,
           FLAT,
           ID IS 7,
           SELF-ACT,
           TITLE "Successivo",
           BITMAP-NUMBER BitmapNumNext
           LAYOUT-DATA RLM-RESIZE-BOTH-ANY
           .

      *{TOTEM}END

      *{TOTEM}LINKPARA
       PROCEDURE  DIVISION USING LinkBrowser.
      *{TOTEM}END

      *{TOTEM}DECLARATIVE
      *{TOTEM}END

       MAIN-LOGIC.
      *{TOTEM}ENTRY-BEFPRG
      *    Before-Program
      *    Before Program
      *{TOTEM}END
           PERFORM INITIALIZE-ROUTINE.
      * run main screen
      *{TOTEM}RUN-MAINSCR
           PERFORM Form1-OPEN-ROUTINE.
      *{TOTEM}END

      *{TOTEM}COPY-PROCEDURE
       EXIT-STOP-ROUTINE.
      * <TOTEM:EPT. INIT:browser, INIT:browser, BeforeDestroyResource>
      * <TOTEM:END>
           DESTROY Calibri14-Occidentale
           CALL "w$bitmap" USING WBITMAP-DESTROY, TOOLBAR-BROWSER-BMP
      *    After-Program
           EXIT PROGRAM TOTEM-PgmStatus
           STOP RUN.

       INITIALIZE-ROUTINE.
      *    Before Init
      * initialize program status variable
           Initialize TOTEM-PgmStatus.
      * get system information
           ACCEPT SYSTEM-INFORMATION FROM SYSTEM-INFO.
      * get terminal information
           ACCEPT TERMINAL-ABILITIES FROM TERMINAL-INFO.
      * set font
           PERFORM INIT-FONT.
      * load bitmap
           PERFORM INIT-BMP.
      * load resource
           PERFORM INIT-RES.
      * create pop-up menu
           PERFORM INIT-POPUP.
      *    After Init
           .
    
       INIT-FONT.
      * Calibri14-Occidentale
           INITIALIZE WFONT-DATA Calibri14-Occidentale
           MOVE 14 TO WFONT-SIZE
           MOVE "Calibri" TO WFONT-NAME
           SET WFCHARSET-DONT-CARE TO TRUE
           SET WFONT-BOLD TO FALSE
           SET WFONT-ITALIC TO FALSE
           SET WFONT-UNDERLINE TO FALSE
           SET WFONT-STRIKEOUT TO FALSE
           SET WFONT-FIXED-PITCH TO FALSE
           MOVE 0 TO WFONT-CHAR-SET
           CALL "W$FONT" USING WFONT-GET-FONT, 
                     Calibri14-Occidentale, WFONT-DATA
           .

       INIT-BMP.
      * pb-esci
           COPY RESOURCE "TOOLBAR-BROWSER.BMP".
           CALL "w$bitmap" USING WBITMAP-LOAD "TOOLBAR-BROWSER.BMP", 
                   GIVING TOOLBAR-BROWSER-BMP.
           .

       INIT-RES.
           .

       INIT-POPUP.
           .


       Form1-Open-Routine.
           PERFORM Form1-Scrn
           PERFORM Form1-Proc
           .

       Form1-Scrn.
           PERFORM Form1-Create-Win
           PERFORM Form1-Init-Value
           PERFORM Form1-Init-Data
      * Tab keystrok settings
      * Tool Bar
           DISPLAY Screen1-Tb-1
           PERFORM Form1-DISPLAY
           .

       Form1-Create-Win.
           Display Independent GRAPHICAL WINDOW
              SCREEN LINE 1,
              SCREEN COLUMN 0,
              LINES 33,39,
              SIZE 92,20,
              HEIGHT-IN-CELLS,
              WIDTH-IN-CELLS,
              COLOR 131329,
              CONTROL FONT Calibri14-Occidentale,
              LINK TO THREAD,
              MIN-LINES 10,00,
              MIN-SIZE 25,00,
              RESIZABLE,
              NO SCROLL,
              TITLE-BAR,
              TITLE "Gestione Immagini",
              AUTO-MINIMIZE,
              WITH SYSTEM MENU,
              USER-GRAY,
              USER-WHITE,
              LAYOUT-MANAGER = mio-layout
              EVENT PROCEDURE Form1-Event-Proc,
              HANDLE IS Form1-Handle,
      * <TOTEM:EPT. FORM:Form1, FORM:Form1, AfterCreateWin>
      * <TOTEM:END>


      * Tool Bar    
           DISPLAY TOOL-BAR 
              LINES 3,35,   
              CONTROL FONT IS Calibri14-Occidentale
              HANDLE IN Screen1-Tb-1-Handle
           DISPLAY Screen1-Tb-1 UPON Screen1-Tb-1-Handle

      * Status-bar
           DISPLAY Form1 UPON Form1-Handle
      * DISPLAY-COLUMNS settings
           .

       Form1-PROC.
      * <TOTEM:EPT. FORM:Form1, FORM:Form1, BeforeAccept>
           modify Form1-Handle, action action-maximize.
           move LinkImage to path-bmp.

           if visualizzazione
              move 0 to e-sfoglia e-sel e-prev e-next
              move BitmapBrowseDisabled to BitmapNumBrowse 
              move BitmapSelDisabled    to BitmapNumSel
              move BitmapPrevDisabled   to BitmapNumPrev
              move BitmapNextDisabled   to BitmapNumNext
           else
              move 1 to e-sfoglia e-sel e-prev e-next
              move BitmapBrowseEnabled  to BitmapNumBrowse 
              move BitmapSelEnabled     to BitmapNumSel
              move BitmapPrevEnabled    to BitmapNumPrev
              move BitmapNextEnabled    to BitmapNumNext
           end-if.

           display pb-sfoglia pb-sel pb-prev pb-next.

           modify web-img,  value = path-bmp.
           modify ef-path,  value = path-bmp.

           set selezionato  to false.

           .
      * <TOTEM:END>
           PERFORM UNTIL Exit-Pushed
              ACCEPT Form1
                 ON EXCEPTION
                    PERFORM Form1-Evaluate-Func
                 MOVE 1 TO TOTEM-Form-Index
              END-ACCEPT
      * <TOTEM:EPT. FORM:Form1, FORM:Form1, AfterEndAccept>
      * <TOTEM:END>
           END-PERFORM
      * <TOTEM:EPT. FORM:Form1, FORM:Form1, BeforeDestroyWindow>
      * <TOTEM:END>
           DESTROY Form1-Handle
           INITIALIZE Key-Status
           .

       Form1-Evaluate-Func.
      * <TOTEM:EPT. FORM:Form1, FORM:Form1, AfterAccept>
      * <TOTEM:END>
           EVALUATE TRUE
              WHEN Exit-Pushed
                 PERFORM Form1-Exit
              WHEN Event-Occurred
                 IF Event-Type = Cmd-Close
                    PERFORM Form1-Exit
                 END-IF
              WHEN Key-Status = 1000
                 PERFORM pb-sfoglia-LinkTo
              WHEN Key-Status = 1001
                 PERFORM pb-sel-LinkTo
              WHEN Key-Status = 67
                 PERFORM PB-PREV-LinkTo
              WHEN Key-Status = 68
                 PERFORM PB-NEXT-LinkTo
           END-EVALUATE
      * avoid changing focus
           MOVE 4 TO Accept-Control
           .

       Form1-CLEAR.
           PERFORM Form1-INIT-VALUE
           PERFORM Form1-DISPLAY
           .

       Form1-DISPLAY.
      * <TOTEM:EPT. FORM:Form1, FORM:Form1, BeforeDisplay>
      * <TOTEM:END>
           DISPLAY Screen1-Tb-1
           DISPLAY Form1 UPON Form1-Handle
      * <TOTEM:EPT. FORM:Form1, FORM:Form1, AfterDisplay>
      * <TOTEM:END>
           .

       Form1-Exit.
      * for main screen
      * <TOTEM:EPT. FORM:Form1, FORM:Form1, BeforeExit>
           destroy mio-layout.

           .
      * <TOTEM:END>
           MOVE 27 TO Key-Status
           .

       Form1-Init-Data.
           MOVE 1 TO TOTEM-Form-Index
           MOVE 0 TO TOTEM-Frame-Index
           .

       Form1-Init-Value.
           MOVE "Gestione Immagini" TO TOTEM-MSG-TITLE
      * <TOTEM:EPT. FORM:Form1, FORM:Form1, SetDefault>
      * <TOTEM:END>
           PERFORM Form1-FLD-TO-BUF
           .


       Form1-ALLGRID-RESET.
           .

      * for Form's Validation
       Form1-VALIDATION-ROUTINE.
           SET TOTEM-CHECK-OK TO TRUE
           .


       Form1-Buf-To-Fld.
      * <TOTEM:EPT. FORM:Form1, FORM:Form1, BeforeBufToFld>
      * <TOTEM:END>
      * <TOTEM:EPT. FORM:Form1, FORM:Form1, AfterBufToFld>
      * <TOTEM:END>
           .

       Form1-Fld-To-Buf.
      * <TOTEM:EPT. FORM:Form1, FORM:Form1, BeforeFldToBuf>
      * <TOTEM:END>
      * <TOTEM:EPT. FORM:Form1, FORM:Form1, AfterFldToBuf>
      * <TOTEM:END>
           .

       Form1-CONTROLLO-OLD.
           set SiSalvato to true.
           if mod = 0 exit paragraph end-if.
           perform Form1-BUF-TO-FLD.
           move 0 to scelta.
           .
       Form1-EXTENDED-FILE-STATUS.
           CALL "C$RERRNAME" USING TOTEM-MSG-ERR-FILE
           CALL "C$RERR" USING EXTEND-STAT, TEXT-MESSAGE
           MOVE PRIMARY-ERROR TO TOTEM-MSG-ID
           PERFORM Form1-SHOW-MSG-ROUTINE
           .

       Form1-SHOW-MSG-ROUTINE.
           PERFORM SHOW-MSG-ROUTINE
           PERFORM Form1-DISPLAY-MESSAGE
           .

       Form1-DISPLAY-MESSAGE.
           PERFORM MESSAGE-BOX-ROUTINE
           DISPLAY MESSAGE BOX TOTEM-MSG-TEXT
               TITLE IS TOTEM-MSG-TITLE
               TYPE  IS TOTEM-MSG-BUTTON-TYPE
               ICON  IS TOTEM-MSG-DEFAULT-BUTTON
               RETURNING TOTEM-MSG-RETURN-VALUE
           .

       Form1-Save-Status.
           .             

       Form1-Restore-Status.
           .



       Form1-Event-Proc.
           .

      * USER DEFINE PARAGRAPH
       EXIT-PGM.
      * <TOTEM:PARA. EXIT-PGM>
           if selezionato 
              move 0 to counter
              move path-bmp  to LinkImage 
              move LinkImage to como-path
              inspect como-path tallying counter for all "file:///"
              if counter not = 0
                 move spaces to LinkImage
                 move como-path(9:) to LinkImage(1:)
              end-if
              move LinkImage to como-path                   
              inspect como-path tallying counter for all "%20"
              if counter > 0
                 inspect como-path replacing all "%20" by "! !"
                 move 1 to counter2
                 perform varying counter from 1 by 1 
                           until counter > 256
                    if como-path(counter:1) not = "!"
                       move como-path(counter:1) to 
           LinkImage(counter2:1)
                       add 1 to counter2
                    end-if
                 end-perform
              end-if
           end-if.
           destroy mio-layout 
           .
      * <TOTEM:END>

      * EVENT PARAGRAPH
       pb-sfoglia-LinkTo.
      * <TOTEM:PARA. pb-sfoglia-LinkTo>
           initialize opensave-data.
           if path-bmp = spaces
              accept path-bmp from environment "PATH_IMMAGINI"
              move path-bmp to opnsav-default-dir
           end-if.
                                    
           move "JPEG files (*.jpg)|*.jpg|GIF files (*.gif)|*.gif|BMP fi
      -    "les (*.bmp)|*.bmp|All files (*.*)|*.*" 
             to opnsav-filters.
           move 1 to opnsav-default-filter.
           move "Selezione immagine" to opnsav-title.
           call "C$OPENSAVEBOX" using opensave-save-box, opensave-data
                               giving opensave-status.

           if opensave-status = 1
              move opnsav-filename to path-bmp
              modify web-img, value = path-bmp
              modify ef-path, value = path-bmp
           end-if 
           .
      * <TOTEM:END>
       pb-sel-LinkTo.
      * <TOTEM:PARA. pb-sel-LinkTo>
           move path-bmp to LinkImage.
           set selezionato to true.
           perform EXIT-PGM.
           move 27 to key-status      
           .
      * <TOTEM:END>
       PB-PREV-LinkTo.
      * <TOTEM:PARA. PB-PREV-LinkTo>
           modify web-img,  GO-BACK 1.
           inquire web-img, value in path-bmp.
           modify ef-path,  value = path-bmp 
           .
      * <TOTEM:END>
       PB-NEXT-LinkTo.
      * <TOTEM:PARA. PB-NEXT-LinkTo>
           modify web-img, GO-FORWARD 1.
           inquire web-img, value in path-bmp.
           modify ef-path,  value = path-bmp 
           .
      * <TOTEM:END>
       ef-path-BeforeProcedure.
      * <TOTEM:PARA. ef-path-BeforeProcedure>
           MODIFY CONTROL-HANDLE COLOR = COLORE-NU
           .
      * <TOTEM:END>
       ef-path-AfterProcedure.
      * <TOTEM:PARA. ef-path-AfterProcedure>
           MODIFY CONTROL-HANDLE COLOR = COLORE-OR
           .
      * <TOTEM:END>

      *{TOTEM}END

      *{TOTEM}SHOW-MSG
       MESSAGE-FOR-FILEERROR.
           CALL "C$RERRNAME" USING TOTEM-MSG-ERR-FILE
           CALL "C$RERR" USING EXTEND-STAT, TEXT-MESSAGE
              MOVE PRIMARY-ERROR TO TOTEM-MSG-ID
           .

       SHOW-TRANSACTION-ROUTINE.
           IF TOTEM-TRANSACTION-FLAG = SPACES
              IF TRANSACTION-STATUS NOT = 0
                 STRING "TRANSACTION ERROR ", TRANSACTION-STATUS
                    DELIMITED BY SIZE INTO TOTEM-MSG-1
              END-IF
           ELSE
              PERFORM SHOW-MSG-ROUTINE
              IF TRANSACTION-STATUS = 0
                 MOVE "Transazione terminata con Rollback." TO 
           TOTEM-MSG-3
              ELSE
                 STRING "TRANSACTION ERROR ", TRANSACTION-STATUS
                    DELIMITED BY SIZE INTO TOTEM-MSG-3
              END-IF
           END-IF
           .

       SHOW-MSG-ROUTINE.
           MOVE SPACE TO TOTEM-MSG-1 TOTEM-MSG-2 TOTEM-MSG-3
           EVALUATE TOTEM-MSG-ID
               WHEN "10"
                    STRING "File:" TOTEM-MSG-ERR-FILE DELIMITED BY SPACE
                       INTO TOTEM-MSG-1
                    MOVE "Nessun altro dato." TO TOTEM-MSG-2
                    MOVE MB-DEFAULT-ICON TO TOTEM-MSG-ICON-TYPE
                    MOVE MB-OK TO TOTEM-MSG-BUTTON-TYPE
               WHEN "22"
                    STRING "File:" TOTEM-MSG-ERR-FILE DELIMITED BY SPACE
                       INTO TOTEM-MSG-1
                    MOVE "Chiave Duplicata." TO TOTEM-MSG-2
                    MOVE MB-ERROR-ICON TO TOTEM-MSG-ICON-TYPE
                    MOVE MB-OK TO TOTEM-MSG-BUTTON-TYPE
               WHEN "23"
                    STRING "File:" TOTEM-MSG-ERR-FILE DELIMITED BY SPACE
                       INTO TOTEM-MSG-1
                    MOVE "Raggiunta fine file." TO TOTEM-MSG-2
                    MOVE MB-WARNING-ICON TO TOTEM-MSG-ICON-TYPE
                    MOVE MB-OK TO TOTEM-MSG-BUTTON-TYPE
               WHEN "101"
                    MOVE "Terminare l'applicazione?" TO TOTEM-MSG-1
                    MOVE 4 TO TOTEM-MSG-ICON-TYPE
                    MOVE MB-YES-NO TO TOTEM-MSG-BUTTON-TYPE
               WHEN "201"
                    MOVE "Aggiungere un nuovo record?" TO TOTEM-MSG-1
                    MOVE 4 TO TOTEM-MSG-ICON-TYPE
                    MOVE MB-YES-NO TO TOTEM-MSG-BUTTON-TYPE
               WHEN "202"
                    MOVE "Aggiornare il Record?" TO TOTEM-MSG-1
                    MOVE 4 TO TOTEM-MSG-ICON-TYPE
                    MOVE MB-YES-NO TO TOTEM-MSG-BUTTON-TYPE
               WHEN "203"
                    MOVE "Cancellare il Record?" TO TOTEM-MSG-1
                    MOVE 4 TO TOTEM-MSG-ICON-TYPE
                    MOVE MB-YES-NO TO TOTEM-MSG-BUTTON-TYPE
               WHEN "204"
                    MOVE "Chiave duplicata." TO TOTEM-MSG-1
                    MOVE MB-WARNING-ICON TO TOTEM-MSG-ICON-TYPE
                    MOVE MB-OK TO TOTEM-MSG-BUTTON-TYPE
               WHEN "301"
                    MOVE "Inserimento avvenuto con Successo." TO 
           TOTEM-MSG-1
                    MOVE MB-DEFAULT-ICON TO TOTEM-MSG-ICON-TYPE
                    MOVE MB-OK TO TOTEM-MSG-BUTTON-TYPE
               WHEN "302"
                    MOVE "Aggiornamento avvenuto con Successo." TO 
           TOTEM-MSG-1
                    MOVE MB-DEFAULT-ICON TO TOTEM-MSG-ICON-TYPE
                    MOVE MB-OK TO TOTEM-MSG-BUTTON-TYPE
               WHEN "303"
                    MOVE "Cancellazione avvenuta con Successo." TO 
           TOTEM-MSG-1
                    MOVE MB-DEFAULT-ICON TO TOTEM-MSG-ICON-TYPE
                    MOVE MB-OK TO TOTEM-MSG-BUTTON-TYPE
               WHEN "401"
                    MOVE "Shell non trovata." TO TOTEM-MSG-1
                    MOVE MB-ERROR-ICON TO TOTEM-MSG-ICON-TYPE
                    MOVE MB-OK TO TOTEM-MSG-BUTTON-TYPE
               WHEN OTHER
                    MOVE TEXT-MESSAGE TO TOTEM-MSG-1
                    STRING "FILE:" TOTEM-MSG-ERR-FILE DELIMITED BY SPACE
                       INTO TOTEM-MSG-3
                    MOVE 0 TO TOTEM-IDX1
                    INSPECT TOTEM-MSG-3 TALLYING TOTEM-IDX1
                       FOR TRAILING SPACE
                    STRING TOTEM-MSG-3(1:TOTEM-MSG-LENGTH - 
           TOTEM-IDX1), 
                       ", FILE STATUS ", PRIMARY-ERROR "," 
                       SECONDARY-ERROR
                    DELIMITED BY SIZE INTO TOTEM-MSG-2
                    MOVE SPACES TO TOTEM-MSG-3
                    MOVE MB-ERROR-ICON TO TOTEM-MSG-ICON-TYPE
                    MOVE MB-OK TO TOTEM-MSG-BUTTON-TYPE
           END-EVALUATE
           PERFORM MESSAGE-BOX-ROUTINE
           .

       MESSAGE-BOX-ROUTINE.
           MOVE 1 TO TOTEM-MSG-TEXT-POINTER
           IF TOTEM-MSG-1 NOT = SPACE
              MOVE 0 TO TOTEM-MSG-SIZE
              INSPECT TOTEM-MSG-1 TALLYING TOTEM-MSG-SIZE FOR TRAILING 
           SPACE
              STRING TOTEM-MSG-1( 1 : TOTEM-MSG-LENGTH - TOTEM-MSG-SIZE 
           )
                 DELIMITED BY SIZE
                 INTO TOTEM-MSG-TEXT, POINTER TOTEM-MSG-TEXT-POINTER
           END-IF

           IF TOTEM-MSG-2 NOT = SPACE
              MOVE 0 TO TOTEM-MSG-SIZE
              INSPECT TOTEM-MSG-2 TALLYING TOTEM-MSG-SIZE FOR TRAILING 
           SPACE
              IF TOTEM-MSG-TEXT-POINTER > 1
                 STRING X"0A" DELIMITED BY SIZE
                     INTO TOTEM-MSG-TEXT, POINTER TOTEM-MSG-TEXT-POINTER
              END-IF
              STRING TOTEM-MSG-2( 1 : TOTEM-MSG-LENGTH - TOTEM-MSG-SIZE 
           )
                  DELIMITED BY SIZE
                  INTO TOTEM-MSG-TEXT, POINTER TOTEM-MSG-TEXT-POINTER
           END-IF

           IF TOTEM-MSG-3 NOT = SPACE
              MOVE 0 TO TOTEM-MSG-SIZE
              INSPECT TOTEM-MSG-3 TALLYING TOTEM-MSG-SIZE FOR TRAILING 
           SPACE
              IF TOTEM-MSG-TEXT-POINTER > 1
                 STRING X"0A" DELIMITED BY SIZE
                     INTO TOTEM-MSG-TEXT, POINTER TOTEM-MSG-TEXT-POINTER
              END-IF
              STRING TOTEM-MSG-3( 1 : TOTEM-MSG-LENGTH - TOTEM-MSG-SIZE 
           )
                  DELIMITED BY SIZE
                  INTO TOTEM-MSG-TEXT, POINTER TOTEM-MSG-TEXT-POINTER
           END-IF

           IF TOTEM-MSG-TEXT-POINTER = 1
              MOVE 0 TO TOTEM-MSG-SIZE
              INSPECT TOTEM-MSG-TEXT TALLYING TOTEM-MSG-SIZE FOR 
           TRAILING SPACE
                  COMPUTE TOTEM-MSG-TEXT-POINTER = 
           TOTEM-MSG-FULL-LENGTH - TOTEM-MSG-SIZE + 1
           END-IF
           MOVE LOW-VALUES TO TOTEM-MSG-TEXT(TOTEM-MSG-TEXT-POINTER : 1 
           )
           .

       MESSAGE-BOX-DISPLAY.
           PERFORM SHOW-MSG-ROUTINE
           PERFORM MESSAGE-BOX-ROUTINE
           DISPLAY MESSAGE BOX TOTEM-MSG-TEXT
               TITLE IS TOTEM-MSG-TITLE
               TYPE  IS TOTEM-MSG-BUTTON-TYPE
               ICON  IS TOTEM-MSG-DEFAULT-BUTTON
               RETURNING TOTEM-MSG-RETURN-VALUE
           .

      *{TOTEM}END

