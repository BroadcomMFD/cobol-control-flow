       CBL XOPTS(COBOL2)
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SLICKP2.
       AUTHOR.     Ira M. Slick.
      ******************************************************************
      *DESCRIPTION: List Accounts                                      *
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.    IBM-PC.
       OBJECT-COMPUTER.    IBM-PC.
       DATA DIVISION.
       WORKING-STORAGE SECTION.

           COPY SLICKCOM.

           COPY SLICKM2.

           COPY DFHAID.

           COPY ACCTFILE.

       01  SCREEN-BUFFER.
           03  SCREEN-LINE-1.
               05  SCR-ACCOUNT-ID            PIC 9(06).
               05  FILLER                    PIC X(02) VALUE SPACE.
               05  SCR-NAME-LAST             PIC X(16).
               05  FILLER                    PIC X(01) VALUE SPACE.
               05  SCR-NAME-FIRST            PIC X(16).
               05  FILLER                    PIC X(01) VALUE SPACE.
               05  SCR-NAME-INITIAL          PIC X(01).
               05  FILLER                    PIC X(35) VALUE SPACE.
           03  SCREEN-LINE-2.
               05  FILLER                    PIC X(08) VALUE SPACE.
               05  SCR-ADDRESS               PIC X(64).
               05  FILLER                    PIC X(06) VALUE SPACE.
           03  SCREEN-LINE-3.
               05  FILLER                    PIC X(08) VALUE SPACE.
               05  SCR-CITY                  PIC X(24).
               05  FILLER                    PIC X(01) VALUE SPACE.
               05  SCR-STATE                 PIC X(02).
               05  FILLER                    PIC X(03) VALUE SPACE.
               05  SCR-ZIP                   PIC 9(05).
               05  FILLER                    PIC X(01) VALUE SPACE.
               05  SCR-ZIP-EX                PIC 9(04).
               05  FILLER                    PIC X(08) VALUE SPACE.
               05  FILLER                    PIC X(01) VALUE '('.
               05  SCR-TEL-1A                PIC 9(03).
               05  FILLER                    PIC X(01) VALUE ')'.
               05  FILLER                    PIC X(01) VALUE SPACE.
               05  SCR-TEL-1B                PIC 9(03).
               05  FILLER                    PIC X(01) VALUE '-'.
               05  SCR-TEL-1C                PIC 9(04).
               05  FILLER                    PIC X(08) VALUE SPACE.

       77  WORK-NUM                          PIC 9(03).

       01  WS-EXIT                           PIC X(11) VALUE
                                             'Slick ended'.

       LINKAGE SECTION.

       01  DFHCOMMAREA.
           03  FILLER                        PIC X(01)
                                             OCCURS 1 TO 4096 TIMES
                                             DEPENDING ON EIBCALEN.

       PROCEDURE DIVISION.

       000-START-PROCESSING.

           MOVE DFHCOMMAREA TO SLICK-COMM.
           MOVE LOW-VALUES  TO SLICKM2I.

           IF COMM-INIT
               PERFORM 000-INITIALIZATION
           ELSE
               PERFORM 000-RECEIVE-MAP.

           IF COMM-NEXT-TRAN = EIBTRNID
               PERFORM 000-SEND-MAP
               EXEC CICS RETURN
                   TRANSID (COMM-NEXT-TRAN)
                   COMMAREA (SLICK-COMM)
               END-EXEC

           ELSE
               SET COMM-INIT TO TRUE

               IF COMM-QUIT
                  EXEC CICS SEND TEXT
                      FROM (WS-EXIT)
                      LENGTH (11)
                      FREEKB
                      ERASE
                  END-EXEC

                  EXEC CICS RETURN
                  END-EXEC

               ELSE
                  EXEC CICS RETURN
                      TRANSID (COMM-NEXT-TRAN)
                      COMMAREA (SLICK-COMM)
                      IMMEDIATE
                  END-EXEC.

       000-INITIALIZATION.

           IF COMM-MAX-ACCOUNT-ID < COMM-MIN-ACCOUNT-ID
               MOVE 'No data available'   TO COMM-MESSAGE
               SET COMM-MENU              TO TRUE

           ELSE
               MOVE EIBTRNID              TO COMM-NEXT-TRAN
               MOVE SPACE                 TO COMM-INIT-FLAG
               MOVE COMM-MIN-ACCOUNT-ID   TO COMM-ACCOUNT-ID.

       000-SEND-MAP.

           MOVE COMM-ACCOUNT-ID           TO A-S-ACCOUNT-ID.
           MOVE COMM-DISPLAY-DATE         TO M2DATEO.
           MOVE COMM-MESSAGE              TO M2MSGO.
           MOVE SPACE                     TO COMM-MESSAGE.

           PERFORM 000-FILL-SCREEN
               VARYING I FROM 1 BY 1
                 UNTIL I > 4.

           COMPUTE WORK-NUM =
               COMM-ACCOUNT-ID - COMM-MIN-ACCOUNT-ID + 1.
           MOVE WORK-NUM                  TO M2FROMO.

           COMPUTE WORK-NUM =
               COMM-MAX-ACCOUNT-ID - COMM-MIN-ACCOUNT-ID + 1.
           MOVE WORK-NUM                  TO M2OFO.

           EXEC CICS SEND
               MAP ('SLICKM2')
               CURSOR
               ERASE
           END-EXEC.

       000-FILL-SCREEN.

           IF A-S-ACCOUNT-ID > COMM-MAX-ACCOUNT-ID
               PERFORM 000-BLANK-ENTRY

           ELSE

               COMPUTE WORK-NUM =
                   A-S-ACCOUNT-ID - COMM-MIN-ACCOUNT-ID + 1
               MOVE WORK-NUM         TO M2TOO

               EXEC CICS READ
                   FILE      ('ACCTFILE')
                   INTO      (ACCOUNT-STATISTICS-RECORD)
                   RIDFLD    (A-S-KEY)
                   LENGTH    (ACCTFILE-LENGTH)
                   KEYLENGTH (ACCTFILE-KEYLENGTH)
                   RESP      (ACCTFILE-RESP)
                   RESP2     (ACCTFILE-RESP2)
               END-EXEC

               IF ACCTFILE-RESP = DFHRESP(NORMAL)
                   PERFORM 000-FILL-ENTRY

               ELSE
                   PERFORM 000-BLANK-ENTRY.

           ADD 1 TO A-S-ACCOUNT-ID.

       000-FILL-ENTRY.

           MOVE A-S-ADDRESS          TO SCR-ADDRESS.
           MOVE A-S-CITY             TO SCR-CITY.
           MOVE A-S-STATE            TO SCR-STATE.
           MOVE A-S-ZIP              TO SCR-ZIP.
           MOVE A-S-ZIP-EX           TO SCR-ZIP-EX.
           MOVE A-S-TEL-1A           TO SCR-TEL-1A.
           MOVE A-S-TEL-1B           TO SCR-TEL-1B.
           MOVE A-S-TEL-1C           TO SCR-TEL-1C.
           MOVE A-S-ACCOUNT-ID       TO SCR-ACCOUNT-ID.
           MOVE A-S-NAME-LAST        TO SCR-NAME-LAST.
           MOVE A-S-NAME-FIRST       TO SCR-NAME-FIRST.
           MOVE A-S-NAME-INITIAL     TO SCR-NAME-INITIAL.

           IF I = 1
               MOVE SCREEN-LINE-1    TO M2A1L1O
               MOVE SCREEN-LINE-2    TO M2A1L2O
               MOVE SCREEN-LINE-3    TO M2A1L3O
           ELSE
           IF I = 2
               MOVE SCREEN-LINE-1    TO M2A2L1O
               MOVE SCREEN-LINE-2    TO M2A2L2O
               MOVE SCREEN-LINE-3    TO M2A2L3O
           ELSE
           IF I = 3
               MOVE SCREEN-LINE-1    TO M2A3L1O
               MOVE SCREEN-LINE-2    TO M2A3L2O
               MOVE SCREEN-LINE-3    TO M2A3L3O
           ELSE
           IF I = 4
               MOVE SCREEN-LINE-1    TO M2A4L1O
               MOVE SCREEN-LINE-2    TO M2A4L2O
               MOVE SCREEN-LINE-3    TO M2A4L3O.

       000-BLANK-ENTRY.

           MOVE SPACE                TO SCREEN-LINE-1.

           IF A-S-ACCOUNT-ID NOT > COMM-MAX-ACCOUNT-ID
               MOVE A-S-ACCOUNT-ID   TO SCR-ACCOUNT-ID
               MOVE 'Unavailable'    TO SCR-NAME-LAST.

           IF I = 1
               MOVE SCREEN-LINE-1    TO M2A1L1O
               MOVE SPACE            TO M2A1L2O
               MOVE SPACE            TO M2A1L3O
           ELSE
           IF I = 2
               MOVE SCREEN-LINE-1    TO M2A2L1O
               MOVE SPACE            TO M2A2L2O
               MOVE SPACE            TO M2A2L3O
           ELSE
           IF I = 3
               MOVE SCREEN-LINE-1    TO M2A3L1O
               MOVE SPACE            TO M2A3L2O
               MOVE SPACE            TO M2A3L3O
           ELSE
           IF I = 4
               MOVE SCREEN-LINE-1    TO M2A4L1O
               MOVE SPACE            TO M2A4L2O
               MOVE SPACE            TO M2A4L3O.

       000-RECEIVE-MAP.

           IF EIBAID = DFHPF11
               SET COMM-MENU TO TRUE
           ELSE
               IF EIBAID = DFHPF12
               OR EIBAID = DFHPF24
                   SET COMM-QUIT TO TRUE
               ELSE
                   PERFORM 000-SCROLL.

       000-SCROLL.

           IF EIBAID = DFHENTER
               NEXT SENTENCE
           ELSE
               IF EIBAID = DFHPF1
                   MOVE 1001 TO COMM-ACCOUNT-ID
               ELSE
                   IF EIBAID = DFHPF2
                       SUBTRACT 3 FROM COMM-MAX-ACCOUNT-ID
                           GIVING COMM-ACCOUNT-ID
                   ELSE
                       IF EIBAID = DFHPF7
                           SUBTRACT 4 FROM COMM-ACCOUNT-ID
                       ELSE
                           IF EIBAID = DFHPF8
                               ADD 4 TO COMM-ACCOUNT-ID
                           ELSE
                               PERFORM 000-ALARM.

            IF COMM-ACCOUNT-ID > COMM-MAX-ACCOUNT-ID
                MOVE COMM-MAX-ACCOUNT-ID TO COMM-ACCOUNT-ID
            ELSE
                IF COMM-ACCOUNT-ID < 1001
                    MOVE 1001 TO COMM-ACCOUNT-ID.

        000-ALARM.

           EXEC CICS SEND CONTROL
               FREEKB
               ALARM
           END-EXEC.





