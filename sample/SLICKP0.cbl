       CBL XOPTS(COBOL2)
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SLICKP0.
       AUTHOR.     Ira M. Slick.
      ******************************************************************
      *DESCRIPTION:Main menu program                                   *
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.    IBM-PC.
       OBJECT-COMPUTER.    IBM-PC.
       DATA DIVISION.
       WORKING-STORAGE SECTION.

           COPY SLICKCOM.

           COPY SLICKM0.

           COPY DFHAID.

           COPY DFHBMSCA.

           COPY ACCTFILE.

       01  WS-TIME                           PIC 9(15) COMP-3.

       01  WS-DATE.
           03  WS-DATE-YY                    PIC 9(2).
           03  FILLER                        PIC X(1).
           03  WS-DATE-MM                    PIC 9(2).
           03  FILLER                        PIC X(1).
           03  WS-DATE-DD                    PIC 9(2).

       01  WS-DISPLAY-DATE.
           03  WS-DISPLAY-DATE-MMM           PIC X(3).
           03  FILLER                        PIC X(1) VALUE ' '.
           03  WS-DISPLAY-DATE-DD            PIC 9(2).
           03  FILLER                        PIC X(2) VALUE ', '.
           03  WS-DISPLAY-DATE-CC            PIC 9(2).
           03  WS-DISPLAY-DATE-YY            PIC 9(2).

       01  WS-TRAN-DATE.
           03  WS-TRAN-DATE-YY               PIC 9(2).
           03  WS-TRAN-DATE-MM               PIC 9(2).
           03  WS-TRAN-DATE-DD               PIC 9(2).

       01  WS-MONTH-NAMES.
           03  FILLER                        PIC X(3) VALUE 'Jan'.
           03  FILLER                        PIC X(3) VALUE 'Feb'.
           03  FILLER                        PIC X(3) VALUE 'Mar'.
           03  FILLER                        PIC X(3) VALUE 'Apr'.
           03  FILLER                        PIC X(3) VALUE 'May'.
           03  FILLER                        PIC X(3) VALUE 'Jun'.
           03  FILLER                        PIC X(3) VALUE 'Jul'.
           03  FILLER                        PIC X(3) VALUE 'Aug'.
           03  FILLER                        PIC X(3) VALUE 'Sep'.
           03  FILLER                        PIC X(3) VALUE 'Oct'.
           03  FILLER                        PIC X(3) VALUE 'Nov'.
           03  FILLER                        PIC X(3) VALUE 'Dec'.

       01  FILLER                            REDEFINES WS-MONTH-NAMES.
           03  WS-MONTH                      PIC X(3) OCCURS 12.

       01  WS-EXIT                           PIC X(11) VALUE
                                             'Slick ended'.

       LINKAGE SECTION.

       01  DFHCOMMAREA.
           03  FILLER                        PIC X(01)
                                             OCCURS 1 TO 4096 TIMES
                                             DEPENDING ON EIBCALEN.
      *
       PROCEDURE DIVISION.

       000-START-PROCESSING.

           IF EIBCALEN > ZERO
               MOVE DFHCOMMAREA TO SLICK-COMM.

           MOVE LOW-VALUES TO SLICKM0I.

           IF COMM-INIT
               PERFORM 000-INITIALIZATION
           ELSE
               PERFORM 000-RECEIVE-MAP.

           IF COMM-NEXT-TRAN = EIBTRNID
               PERFORM 000-SEND-MAP
               EXEC CICS RETURN
                   TRANSID  (COMM-NEXT-TRAN)
                   COMMAREA (SLICK-COMM)
               END-EXEC
           ELSE
               MOVE 'Y' TO COMM-INIT-FLAG

               IF COMM-UPDATE
               OR COMM-HISTORY
               OR COMM-STATUS

                   EXEC CICS RETURN
                       TRANSID ('SLKA')
                       COMMAREA (SLICK-COMM)
                       IMMEDIATE
                   END-EXEC

               ELSE
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

           MOVE EIBTRNID TO COMM-NEXT-TRAN.
           MOVE SPACE    TO COMM-INIT-FLAG.

           IF EIBCALEN = ZERO
               PERFORM 000-VERIFY-ACCOUNT-FILE.

       000-VERIFY-ACCOUNT-FILE.

           EXEC CICS READ
               FILE      ('ACCTFILE')
               INTO      (ACCOUNT-CONTROL-RECORD)
               RIDFLD    (A-C-KEY)
               LENGTH    (ACCTFILE-LENGTH)
               KEYLENGTH (ACCTFILE-KEYLENGTH)
               RESP      (ACCTFILE-RESP)
           END-EXEC.

           IF ACCTFILE-RESP NOT = DFHRESP(NORMAL)

               MOVE 1001        TO A-C-MIN-ACCOUNT-ID
               MOVE 1000        TO A-C-MAX-ACCOUNT-ID

               EXEC CICS WRITE
                   FILE      ('ACCTFILE')
                   FROM      (ACCOUNT-CONTROL-RECORD)
                   RIDFLD    (A-C-KEY)
                   LENGTH    (ACCTFILE-LENGTH)
                   KEYLENGTH (ACCTFILE-KEYLENGTH)
                   RESP      (ACCTFILE-RESP)
               END-EXEC

               IF ACCTFILE-RESP NOT = DFHRESP(NORMAL)

                   MOVE 'SE00' TO COMM-ABCODE
                   EXEC CICS ABEND
                       ABCODE (COMM-ABCODE)
                       NODUMP
                   END-EXEC.

           MOVE A-C-MIN-ACCOUNT-ID TO COMM-MIN-ACCOUNT-ID.
           MOVE A-C-MAX-ACCOUNT-ID TO COMM-MAX-ACCOUNT-ID.

       000-SEND-MAP.

           EXEC CICS ASKTIME
               ABSTIME  (WS-TIME)
           END-EXEC.

           EXEC CICS FORMATTIME
               ABSTIME  (WS-TIME)
               DATESEP  ('/')
               YYMMDD   (WS-DATE)
           END-EXEC.

Y2K        IF WS-DATE-YY > 80
Y2K            MOVE 19                TO WS-DISPLAY-DATE-CC
Y2K        ELSE
Y2K            MOVE 20                TO WS-DISPLAY-DATE-CC.

           MOVE WS-MONTH (WS-DATE-MM) TO WS-DISPLAY-DATE-MMM.
           MOVE WS-DATE-DD            TO WS-DISPLAY-DATE-DD.
           MOVE WS-DATE-YY            TO WS-DISPLAY-DATE-YY.
Y2K   *    MOVE 19                    TO WS-DISPLAY-DATE-CC.
           MOVE WS-DISPLAY-DATE       TO COMM-DISPLAY-DATE.

           MOVE WS-DATE-YY            TO WS-TRAN-DATE-YY.
           MOVE WS-DATE-MM            TO WS-TRAN-DATE-MM.
           MOVE WS-DATE-DD            TO WS-TRAN-DATE-DD.
           MOVE WS-TRAN-DATE          TO COMM-TRAN-DATE.

           MOVE COMM-DISPLAY-DATE     TO M0DATEO.
           MOVE COMM-MESSAGE          TO M0MSGO.
           MOVE SPACE                 TO COMM-MESSAGE.

           IF COMM-MIN-ACCOUNT-ID > COMM-MAX-ACCOUNT-ID
               MOVE DFHBMDAR          TO M0PF2A
               MOVE DFHBMDAR          TO M0PF3A
               MOVE DFHBMDAR          TO M0PF4A
               MOVE DFHBMDAR          TO M0PF5A
               MOVE DFHBMDAR          TO M0PF6A
           ELSE
               MOVE DFHBMASB          TO M0PF2A
               MOVE DFHBMASB          TO M0PF3A
               MOVE DFHBMASB          TO M0PF4A
               MOVE DFHBMASB          TO M0PF5A
               MOVE DFHBMASB          TO M0PF6A.

           EXEC CICS SEND MAP ('SLICKM0')
               CURSOR
               ERASE
           END-EXEC.

           IF M0MSGO NOT = SPACE
               PERFORM 000-ALARM.

       000-RECEIVE-MAP.

           IF EIBAID = DFHPF1
               MOVE 'Add'          TO COMM-FUNCTION
               SET COMM-ADD        TO TRUE
           ELSE
           IF EIBAID =  DFHPF12
           OR EIBAID =  DFHPF24
               MOVE 'Quit'         TO COMM-FUNCTION
               SET COMM-QUIT       TO TRUE
           ELSE
           IF COMM-MIN-ACCOUNT-ID > COMM-MAX-ACCOUNT-ID
               MOVE 'Invalid key.' TO COMM-MESSAGE
           ELSE
           IF EIBAID = DFHPF2
               MOVE 'List'         TO COMM-FUNCTION
               SET COMM-LIST       TO TRUE
           ELSE
           IF EIBAID =  DFHPF3
               MOVE 'Update'       TO COMM-FUNCTION
               SET COMM-UPDATE     TO TRUE
           ELSE
           IF EIBAID =  DFHPF4
               MOVE 'History'      TO COMM-FUNCTION
               SET COMM-HISTORY    TO TRUE
           ELSE
           IF EIBAID =  DFHPF5
               MOVE 'Status'       TO COMM-FUNCTION
               SET COMM-STATUS     TO TRUE
           ELSE
           IF EIBAID =  DFHPF6
               MOVE 'Schedule'     TO COMM-FUNCTION
               SET COMM-SCHEDULE   TO TRUE
           ELSE
               MOVE 'Invalid Key.' TO COMM-MESSAGE.

       000-ALARM.

           EXEC CICS SEND CONTROL
               ALARM
               FREEKB
           END-EXEC.




