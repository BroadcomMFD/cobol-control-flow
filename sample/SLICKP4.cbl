       CBL XOPTS(COBOL2)
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SLICKP4.
       AUTHOR.     Ira M. Slick.
      ******************************************************************
      *DESCRIPTION: Account History                                    *
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.    IBM-PC.
       OBJECT-COMPUTER.    IBM-PC.
       DATA DIVISION.
       WORKING-STORAGE SECTION.

           COPY SLICKCOM.

           COPY SLICKM4.

           COPY DFHAID.

           COPY ACCTFILE.

       01  WS-MAP-FIELDS.
           03  WS-FROM                       PIC S9(04) COMP VALUE ZERO.
           03  WS-TO                         PIC  9(04) COMP VALUE ZERO.
           03  WS-INDEX                      PIC  9(04) COMP VALUE ZERO.
           03  WS-TABLE-ENTRIES              PIC  9(04) COMP VALUE ZERO.
           03  WS-TABLE-SIZE                 PIC  9(08) COMP VALUE ZERO.
           03  WS-TABLE-POINTER              USAGE IS POINTER.

       01  WS-DELIVERY-ENTRY                 PIC  9(04) COMP VALUE ZERO.
       01  WS-DELIVERY-RECORD                PIC  9(04) COMP VALUE ZERO.
       01  WS-DELIVERY-SUB                   PIC  9(04) COMP VALUE ZERO.

       01  WS-PAYMENT-ENTRY                  PIC  9(04) COMP VALUE ZERO.
       01  WS-PAYMENT-RECORD                 PIC  9(04) COMP VALUE ZERO.
       01  WS-PAYMENT-SUB                    PIC  9(04) COMP VALUE ZERO.

       01  WS-MAINTENANCE-ENTRY              PIC  9(04) COMP VALUE ZERO.
       01  WS-MAINTENANCE-RECORD             PIC  9(04) COMP VALUE ZERO.
       01  WS-MAINTENANCE-SUB                PIC  9(04) COMP VALUE ZERO.

       01  SCREEN-BUFFER                     PIC  X(80) VALUE SPACE.

       01  DELIVERY-MESSAGE.
           03  D-M-MM                        PIC  9(02).
           03  FILLER                        PIC  X(01) VALUE '/'.
           03  D-M-DD                        PIC  9(02).
           03  FILLER                        PIC  X(01) VALUE '/'.
           03  D-M-YY                        PIC  9(02).
           03  FILLER                        PIC  X(03) VALUE SPACE.
           03  D-M-EMPL                      PIC  9(04).
           03  FILLER                        PIC  X(03) VALUE SPACE.
           03  FILLER                        PIC  X(12) VALUE
                                                  'Delivery of '.
           03  D-M-GALLONS                   PIC  ZZZ9.
           03  FILLER                        PIC  X(12) VALUE
                                                  ' gallons at '.
           03  D-M-UNIT-PRICE                PIC  X(05).
           03  FILLER                        PIC  X(01) VALUE '.'.

       01  PAYMENT-MESSAGE.
           03  P-M-MM                        PIC  9(02).
           03  FILLER                        PIC  X(01) VALUE '/'.
           03  P-M-DD                        PIC  9(02).
           03  FILLER                        PIC  X(01) VALUE '/'.
           03  P-M-YY                        PIC  9(02).
           03  FILLER                        PIC  X(03) VALUE SPACE.
           03  FILLER                        PIC  X(04) VALUE 'n/a'.
           03  FILLER                        PIC  X(03) VALUE SPACE.
           03  FILLER                        PIC  X(11) VALUE
                                                  'Payment of '.
           03  P-M-AMOUNT                    PIC  X(08).
           03  FILLER                        PIC  X(10) VALUE
                                                  ' received.'.

       01  MAINTENANCE-MESSAGE.
           03  M-M-MM                        PIC  9(02).
           03  FILLER                        PIC  X(01) VALUE '/'.
           03  M-M-DD                        PIC  9(02).
           03  FILLER                        PIC  X(01) VALUE '/'.
           03  M-M-YY                        PIC  9(02).
           03  FILLER                        PIC  X(03) VALUE SPACE.
           03  M-M-EMPL                      PIC  9(04).
           03  FILLER                        PIC  X(03) VALUE SPACE.
           03  FILLER                        PIC  X(08) VALUE
                                                  'Service '.
           03  M-M-SERVICE-CHARGE            PIC  X(40).

       01  SERVICE-CHARGE-MESSAGE.
           03  FILLER                        PIC  X(13) VALUE
                                                  '(a charge of '.
           03  M-M-AMOUNT                    PIC  X(08).
           03  FILLER                        PIC  X(14) VALUE
                                                  ' was applied).'.
       01  NO-CHARGE-MESSAGE.
           03  FILLER                        PIC  X(14) VALUE
                                                  '(No charge)'.

       01  TEMP-DATE.
           03  TEMP-YY                       PIC  9(02).
           03  TEMP-MM                       PIC  9(02).
           03  TEMP-DD                       PIC  9(02).

       01  WORK-NUM                          PIC  9(03).

       01  WS-NULL                           PIC  X(01) VALUE LOW-VALUE.

       01  WS-EXIT                           PIC X(11) VALUE
                                             'Slick ended'.

       LINKAGE SECTION.

       01  DFHCOMMAREA.
           03  FILLER                        PIC  X(01)
                                             OCCURS 1 TO 4096 TIMES
                                             DEPENDING ON EIBCALEN.

       01  HISTORY-TABLE.
           03  HISTORY-ENTRY              OCCURS 1 TO 1000 TIMES
                                          DEPENDING ON WS-TABLE-ENTRIES.
               05  H-RECORD-TYPE             PIC  9(02).
               05  H-ENTRY                   PIC  9(04).

       PROCEDURE DIVISION.

       000-START-PROCESSING.

           MOVE DFHCOMMAREA                  TO SLICK-COMM.
           MOVE LOW-VALUES                   TO SLICKM4I.

           IF COMM-INIT
               PERFORM 000-INITIALIZATION

           ELSE
               MOVE COMM-SAVE                TO WS-MAP-FIELDS
               SET ADDRESS OF HISTORY-TABLE  TO WS-TABLE-POINTER
               PERFORM 000-RECEIVE-MAP.

           IF COMM-NEXT-TRAN = EIBTRNID
               MOVE WS-MAP-FIELDS            TO COMM-SAVE
               PERFORM 000-SEND-MAP

               EXEC CICS RETURN
                   TRANSID (COMM-NEXT-TRAN)
                   COMMAREA (SLICK-COMM)
               END-EXEC

           ELSE
               SET COMM-INIT                 TO TRUE

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

       000-TERMINATION.

           IF WS-TABLE-POINTER NOT = NULL
               EXEC CICS FREEMAIN
                   DATA  (HISTORY-TABLE)
                   RESP  (COMM-RESP)
                   RESP2 (COMM-RESP2)
               END-EXEC.

       000-INITIALIZATION.

           MOVE EIBTRNID              TO COMM-NEXT-TRAN.
           MOVE SPACE                 TO COMM-INIT-FLAG.

           MOVE COMM-ACCOUNT-ID       TO A-S-ACCOUNT-ID.
           MOVE COMM-ACCOUNT-ID       TO A-D-ACCOUNT-ID.
           MOVE COMM-ACCOUNT-ID       TO A-P-ACCOUNT-ID.
           MOVE COMM-ACCOUNT-ID       TO A-M-ACCOUNT-ID.
           MOVE 1                     TO WS-FROM.

           EXEC CICS READ
               FILE      ('ACCTFILE')
               INTO      (ACCOUNT-STATISTICS-RECORD)
               RIDFLD    (A-S-KEY)
               LENGTH    (ACCTFILE-LENGTH)
               KEYLENGTH (ACCTFILE-KEYLENGTH)
               RESP      (ACCTFILE-RESP)
               RESP2     (ACCTFILE-RESP2)
           END-EXEC.

           IF ACCTFILE-RESP = DFHRESP(NORMAL)
               PERFORM 000-BUILD-HISTORY-TABLE

           ELSE
               MOVE 'Error reading account file.'
                                          TO M4MSGO
               SET COMM-MENU              TO TRUE.

       000-BUILD-HISTORY-TABLE.

           MOVE A-S-DELIVERY-ENTRIES      TO WS-DELIVERY-ENTRY.
           MOVE A-S-PAYMENT-ENTRIES       TO WS-PAYMENT-ENTRY.
           MOVE A-S-MAINTENANCE-ENTRIES   TO WS-MAINTENANCE-ENTRY.

           COMPUTE WS-TABLE-ENTRIES = A-S-DELIVERY-ENTRIES +
                                      A-S-PAYMENT-ENTRIES  +
                                      A-S-MAINTENANCE-ENTRIES.

           MULTIPLY WS-TABLE-ENTRIES BY 6 GIVING WS-TABLE-SIZE.

           EXEC CICS GETMAIN
               SET     (WS-TABLE-POINTER)
               LENGTH  (WS-TABLE-SIZE)
               INITIMG (WS-NULL)
               SHARED
               RESP    (COMM-RESP)
               RESP2   (COMM-RESP2)
           END-EXEC.

           SET ADDRESS OF HISTORY-TABLE   TO WS-TABLE-POINTER.

           MOVE 9999                      TO A-D-RECORD-NUMBER.
           MOVE 9999                      TO A-P-RECORD-NUMBER.
           MOVE 9999                      TO A-M-RECORD-NUMBER.

           IF COMM-RESP = DFHRESP(NORMAL)
               PERFORM 000-BUILD-HISTORY-ENTRY
                   VARYING I FROM 1 BY 1
                     UNTIL I > WS-TABLE-ENTRIES.

       000-BUILD-HISTORY-ENTRY.

           PERFORM 000-GET-DELIVERY-ENTRY.
           PERFORM 000-GET-PAYMENT-ENTRY.
           PERFORM 000-GET-MAINTENANCE-ENTRY.

Y2K        MOVE A-D-DATE-OF-DELIVERY (WS-DELIVERY-SUB)
Y2K                                          TO Y2K-YYMMDD-1.
Y2K        PERFORM 000-Y2K-WINDOW-1.
Y2K
Y2K        MOVE A-P-DATE-OF-PAYMENT  (WS-PAYMENT-SUB)
Y2K                                          TO Y2K-YYMMDD-2.
Y2K        PERFORM 000-Y2K-WINDOW-2.
Y2K
Y2K        MOVE A-M-DATE-OF-SERVICE  (WS-MAINTENANCE-SUB)
Y2K                                          TO Y2K-YYMMDD-3.
Y2K        PERFORM 000-Y2K-WINDOW-3.

Y2K   *    IF  A-D-DATE-OF-DELIVERY (WS-DELIVERY-SUB) >
Y2K   *        A-P-DATE-OF-PAYMENT  (WS-PAYMENT-SUB)
Y2K   *    AND A-D-DATE-OF-DELIVERY (WS-DELIVERY-SUB) >
Y2K   *        A-M-DATE-OF-SERVICE  (WS-MAINTENANCE-SUB)

Y2K        IF  Y2K-DATE-1 > Y2K-DATE-2
Y2K        AND Y2K-DATE-1 > Y2K-DATE-3
Y2K
               MOVE A-D-RECORD-TYPE          TO H-RECORD-TYPE   (I)
               MOVE WS-DELIVERY-ENTRY        TO H-ENTRY         (I)
               SUBTRACT 1 FROM WS-DELIVERY-ENTRY

           ELSE

Y2K   *        IF A-P-DATE-OF-PAYMENT (WS-PAYMENT-SUB) >
Y2K   *           A-M-DATE-OF-SERVICE  (WS-MAINTENANCE-SUB)

Y2K            IF Y2K-DATE-2 > Y2K-DATE-3

                   MOVE A-P-RECORD-TYPE      TO H-RECORD-TYPE   (I)
                   MOVE WS-PAYMENT-ENTRY     TO H-ENTRY         (I)
                   SUBTRACT 1 FROM WS-PAYMENT-ENTRY

               ELSE

                   MOVE A-M-RECORD-TYPE      TO H-RECORD-TYPE   (I)
                   MOVE WS-MAINTENANCE-ENTRY TO H-ENTRY         (I)
                   SUBTRACT 1 FROM WS-MAINTENANCE-ENTRY.

       000-GET-DELIVERY-ENTRY.

           IF WS-DELIVERY-ENTRY = ZERO
               MOVE ZERO             TO A-D-DATE-OF-DELIVERY (1)
               MOVE 1                TO WS-DELIVERY-SUB

           ELSE
               COMPUTE WS-DELIVERY-RECORD =
                       (WS-DELIVERY-ENTRY - 1) / 20

               COMPUTE WS-DELIVERY-SUB =
                       WS-DELIVERY-ENTRY - (WS-DELIVERY-RECORD * 20)

               IF WS-DELIVERY-RECORD NOT = A-D-RECORD-NUMBER

                   MOVE WS-DELIVERY-RECORD TO A-D-RECORD-NUMBER

                   EXEC CICS READ
                       FILE      ('ACCTFILE')
                       INTO      (ACCOUNT-DELIVERY-RECORD)
                       RIDFLD    (A-D-KEY)
                       LENGTH    (ACCTFILE-LENGTH)
                       KEYLENGTH (ACCTFILE-KEYLENGTH)
                       RESP      (ACCTFILE-RESP)
                       RESP2     (ACCTFILE-RESP2)
                   END-EXEC

                   IF ACCTFILE-RESP NOT = DFHRESP(NORMAL)
                       MOVE ZERO     TO A-D-DATE-OF-DELIVERY (1)
                       MOVE 1        TO WS-DELIVERY-SUB
                       MOVE ZERO     TO WS-DELIVERY-ENTRY.

       000-GET-PAYMENT-ENTRY.

           IF WS-PAYMENT-ENTRY = ZERO
               MOVE ZERO             TO A-P-DATE-OF-PAYMENT (1)
               MOVE 1                TO WS-PAYMENT-SUB

           ELSE
               COMPUTE WS-PAYMENT-RECORD =
                       (WS-PAYMENT-ENTRY - 1) / 20

               COMPUTE WS-PAYMENT-SUB =
                       WS-PAYMENT-ENTRY - (WS-PAYMENT-RECORD * 20)

               IF WS-PAYMENT-RECORD NOT = A-P-RECORD-NUMBER

                   MOVE WS-PAYMENT-RECORD TO A-P-RECORD-NUMBER

                   EXEC CICS READ
                       FILE      ('ACCTFILE')
                       INTO      (ACCOUNT-PAYMENT-RECORD)
                       RIDFLD    (A-P-KEY)
                       LENGTH    (ACCTFILE-LENGTH)
                       KEYLENGTH (ACCTFILE-KEYLENGTH)
                       RESP      (ACCTFILE-RESP)
                       RESP2     (ACCTFILE-RESP2)
                   END-EXEC

                   IF ACCTFILE-RESP NOT = DFHRESP(NORMAL)
                       MOVE ZERO     TO A-P-DATE-OF-PAYMENT (1)
                       MOVE 1        TO WS-PAYMENT-SUB
                       MOVE ZERO     TO WS-PAYMENT-ENTRY.

       000-GET-MAINTENANCE-ENTRY.

           IF WS-MAINTENANCE-ENTRY = ZERO
               MOVE ZERO             TO A-M-DATE-OF-SERVICE (1)
               MOVE 1                TO WS-MAINTENANCE-SUB

           ELSE
               COMPUTE WS-MAINTENANCE-RECORD =
                     (WS-MAINTENANCE-ENTRY - 1) / 10

               COMPUTE WS-MAINTENANCE-SUB =
                     WS-MAINTENANCE-ENTRY - (WS-MAINTENANCE-RECORD * 10)

               IF WS-MAINTENANCE-RECORD NOT = A-M-RECORD-NUMBER

                   MOVE WS-MAINTENANCE-RECORD TO A-M-RECORD-NUMBER

                   EXEC CICS READ
                       FILE      ('ACCTFILE')
                       INTO      (ACCOUNT-MAINTENANCE-RECORD)
                       RIDFLD    (A-M-KEY)
                       LENGTH    (ACCTFILE-LENGTH)
                       KEYLENGTH (ACCTFILE-KEYLENGTH)
                       RESP      (ACCTFILE-RESP)
                       RESP2     (ACCTFILE-RESP2)
                   END-EXEC

                   IF ACCTFILE-RESP NOT = DFHRESP(NORMAL)
                       MOVE ZERO     TO A-M-DATE-OF-SERVICE (1)
                       MOVE 1        TO WS-MAINTENANCE-SUB
                       MOVE ZERO     TO WS-MAINTENANCE-ENTRY.

       000-SEND-MAP.

           MOVE COMM-DISPLAY-DATE         TO M4DATEO.
           MOVE COMM-MESSAGE              TO M4MSGO.
           MOVE SPACE                     TO COMM-MESSAGE.

           ADD WS-FROM 7 GIVING WS-TO.

           IF WS-TO > WS-TABLE-ENTRIES
               MOVE WS-TABLE-ENTRIES      TO WS-TO.

           MOVE WS-FROM                   TO WORK-NUM.
           MOVE WORK-NUM                  TO M4FROMO.
           MOVE WS-TO                     TO WORK-NUM.
           MOVE WORK-NUM                  TO M4TOO.
           MOVE WS-TABLE-ENTRIES          TO WORK-NUM
           MOVE WORK-NUM                  TO M4OFO.

           MOVE COMM-ACCOUNT-ID           TO A-D-ACCOUNT-ID.
           MOVE COMM-ACCOUNT-ID           TO A-P-ACCOUNT-ID.
           MOVE COMM-ACCOUNT-ID           TO A-M-ACCOUNT-ID.
           MOVE COMM-ACCOUNT-ID           TO M4IDO.

           MOVE 9999                      TO A-D-RECORD-NUMBER.
           MOVE 9999                      TO A-P-RECORD-NUMBER.
           MOVE 9999                      TO A-M-RECORD-NUMBER.

           MOVE 1                         TO I.

           PERFORM 000-FILL-SCREEN
               VARYING WS-INDEX FROM WS-FROM BY 1
                 UNTIL WS-INDEX > WS-TO.

           EXEC CICS SEND
               MAP ('SLICKM4')
               CURSOR
               ERASE
           END-EXEC.

       000-FILL-SCREEN.

           IF H-RECORD-TYPE (WS-INDEX) = A-D-RECORD-TYPE
               PERFORM 000-FORMAT-DELIVERY

           ELSE
               IF H-RECORD-TYPE (WS-INDEX) = A-P-RECORD-TYPE
                   PERFORM 000-FORMAT-PAYMENT

               ELSE
                   IF H-RECORD-TYPE (WS-INDEX) = A-M-RECORD-TYPE
                       PERFORM 000-FORMAT-MAINTENANCE.

           IF I = 1
               MOVE SCREEN-BUFFER                TO M4L1O
           ELSE
           IF I = 2
               MOVE SCREEN-BUFFER                TO M4L2O
           ELSE
           IF I = 3
               MOVE SCREEN-BUFFER                TO M4L3O
           ELSE
           IF I = 4
               MOVE SCREEN-BUFFER                TO M4L4O
           ELSE
           IF I = 5
               MOVE SCREEN-BUFFER                TO M4L5O
           ELSE
           IF I = 6
               MOVE SCREEN-BUFFER                TO M4L6O
           ELSE
           IF I = 7
               MOVE SCREEN-BUFFER                TO M4L7O.

           ADD 1 TO I.

       000-FORMAT-DELIVERY.
           MOVE H-ENTRY (WS-INDEX)               TO WS-DELIVERY-ENTRY.
           PERFORM 000-GET-DELIVERY-ENTRY.
           MOVE A-D-DATE-OF-DELIVERY (WS-DELIVERY-SUB)
                                                 TO TEMP-DATE.
           MOVE TEMP-YY                          TO D-M-YY.
           MOVE TEMP-MM                          TO D-M-MM.
           MOVE TEMP-DD                          TO D-M-DD.
           MOVE A-D-TECHNICIAN (WS-DELIVERY-SUB) TO D-M-EMPL.
           MOVE A-D-GALLONS (WS-DELIVERY-SUB)    TO D-M-GALLONS.
           MOVE A-D-UNIT-PRICE (WS-DELIVERY-SUB) TO NUM-5.
           MOVE DOLLARS-5                        TO D-5.
           MOVE CENTS-5                          TO C-5.
           MOVE DC-5                             TO D-M-UNIT-PRICE.
           MOVE DELIVERY-MESSAGE                 TO SCREEN-BUFFER.

       000-FORMAT-PAYMENT.
           MOVE H-ENTRY (WS-INDEX)               TO WS-PAYMENT-ENTRY
           PERFORM 000-GET-PAYMENT-ENTRY.
           MOVE A-P-DATE-OF-PAYMENT (WS-PAYMENT-SUB)
                                                 TO TEMP-DATE.
           MOVE TEMP-YY                          TO P-M-YY.
           MOVE TEMP-MM                          TO P-M-MM.
           MOVE TEMP-DD                          TO P-M-DD.
           MOVE A-P-AMOUNT (WS-PAYMENT-SUB)      TO NUM-8.
           MOVE DOLLARS-8                        TO D-8.
           MOVE CENTS-8                          TO C-8.
           MOVE DC-8                             TO P-M-AMOUNT.
           MOVE PAYMENT-MESSAGE                  TO SCREEN-BUFFER.

       000-FORMAT-MAINTENANCE.
           MOVE H-ENTRY (WS-INDEX)               TO WS-MAINTENANCE-ENTRY
           PERFORM 000-GET-MAINTENANCE-ENTRY.
           MOVE A-M-DATE-OF-SERVICE (WS-MAINTENANCE-SUB)
                                                 TO TEMP-DATE.
           MOVE TEMP-YY                          TO M-M-YY.
           MOVE TEMP-MM                          TO M-M-MM.
           MOVE TEMP-DD                          TO M-M-DD.

           MOVE A-M-TECHNICIAN (WS-MAINTENANCE-SUB)
                                                 TO M-M-EMPL.

           IF A-M-SERVICE-CHARGE (WS-MAINTENANCE-SUB) = ZERO
               MOVE NO-CHARGE-MESSAGE            TO M-M-SERVICE-CHARGE

           ELSE
               MOVE A-M-SERVICE-CHARGE (WS-MAINTENANCE-SUB)
                                                 TO NUM-8
               MOVE DOLLARS-8                    TO D-8
               MOVE CENTS-8                      TO C-8
               MOVE DC-8                         TO M-M-AMOUNT
               MOVE SERVICE-CHARGE-MESSAGE       TO M-M-SERVICE-CHARGE.

           MOVE MAINTENANCE-MESSAGE              TO SCREEN-BUFFER.

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
                   MOVE 1 TO WS-FROM
               ELSE
                   IF EIBAID = DFHPF2
                       SUBTRACT 6 FROM WS-TABLE-ENTRIES
                           GIVING WS-FROM
                   ELSE
                       IF EIBAID = DFHPF7
                           SUBTRACT 7 FROM WS-FROM
                       ELSE
                           IF EIBAID = DFHPF8
                               ADD 7 TO WS-FROM
                           ELSE
                               PERFORM 000-ALARM.

            IF WS-FROM > WS-TABLE-ENTRIES
                MOVE WS-TABLE-ENTRIES TO WS-FROM
            ELSE
                IF WS-FROM < 1
                    MOVE 1 TO WS-FROM.

       000-ALARM.

           EXEC CICS SEND CONTROL
               FREEKB
               ALARM
           END-EXEC.

           COPY SLICKNUM.
