       CBL XOPTS(COBOL2)
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SLICKP5.
       AUTHOR.     Ira M. Slick.
      ******************************************************************
      *DESCRIPTION: Account Status                                     *
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.    IBM-PC.
       OBJECT-COMPUTER.    IBM-PC.
       DATA DIVISION.
       WORKING-STORAGE SECTION.

           COPY SLICKCOM.

           COPY SLICKM5.

           COPY DFHAID.

           COPY ACCTFILE.

       01  WS-MAP-FIELDS.
           03  WS-NAME-LAST                  PIC X(16) VALUE SPACE.
           03  WS-NAME-FIRST                 PIC X(16) VALUE SPACE.
           03  WS-NAME-INITIAL               PIC X(01) VALUE SPACE.
           03  WS-ADDRESS                    PIC X(64) VALUE SPACE.
           03  WS-CITY                       PIC X(24) VALUE SPACE.
           03  WS-STATE                      PIC X(02) VALUE SPACE.
           03  WS-ZIP                        PIC 9(05) VALUE ZERO.
           03  WS-ZIP-EX                     PIC 9(04) VALUE ZERO.
           03  WS-TEL-1A                     PIC 9(03) VALUE ZERO.
           03  WS-TEL-1B                     PIC 9(03) VALUE ZERO.
           03  WS-TEL-1C                     PIC 9(04) VALUE ZERO.
           03  WS-TEL-2A                     PIC 9(03) VALUE ZERO.
           03  WS-TEL-2B                     PIC 9(03) VALUE ZERO.
           03  WS-TEL-2C                     PIC 9(04) VALUE ZERO.
           03  WS-LAST-PAYMENT-DATE.
               05  WS-L-P-YY                 PIC 9(02) VALUE ZERO.
                   88  NO-PAYMENTS           VALUE ZERO.
               05  WS-L-P-MM                 PIC 9(02) VALUE ZERO.
               05  WS-L-P-DD                 PIC 9(02) VALUE ZERO.
           03  WS-LAST-PAYMENT-AMOUNT        PIC 9(08) VALUE ZERO.
           03  WS-LAST-DELIVERY-DATE.
               05  WS-L-D-YY                 PIC 9(02) VALUE ZERO.
                   88  NO-DELIVERIES         VALUE ZERO.
               05  WS-L-D-MM                 PIC 9(02) VALUE ZERO.
               05  WS-L-D-DD                 PIC 9(02) VALUE ZERO.
           03  WS-LAST-DELIVERY-AMOUNT       PIC ZZZ9  VALUE ZERO.
           03  WS-LAST-DELIVERY-PRICE        PIC 9(05) VALUE ZERO.
           03  WS-LAST-SERVICE-DATE.
               05  WS-L-S-YY                 PIC 9(02) VALUE ZERO.
                   88  NO-SERVICE            VALUE ZERO.
               05  WS-L-S-MM                 PIC 9(02) VALUE ZERO.
               05  WS-L-S-DD                 PIC 9(02) VALUE ZERO.
           03  WS-LAST-SERVICE-CHARGE        PIC 9(08) VALUE ZERO.
           03  WS-NEXT-PAYMENT-DATE.
               05  WS-N-P-YY                 PIC 9(02) VALUE ZERO.
               05  WS-N-P-MM                 PIC 9(02) VALUE ZERO.
               05  WS-N-P-DD                 PIC 9(02) VALUE ZERO.
           03  WS-NEXT-PAYMENT-AMOUNT        PIC 9(08) VALUE ZERO.
           03  WS-BALANCE                    PIC 9(08) VALUE ZERO.
           03  WS-CR                         PIC X(02) VALUE SPACE.
           03  WS-LAST-AID                   PIC X(01) VALUE SPACE.

       01  WS-FORMAT-TEL.
           03  FILLER                        PIC X(01) VALUE '('.
           03  WS-TEL-A                      PIC 9(03) VALUE ZERO.
           03  FILLER                        PIC X(02) VALUE ') '.
           03  WS-TEL-B                      PIC 9(03) VALUE ZERO.
           03  FILLER                        PIC X(01) VALUE '-'.
           03  WS-TEL-C                      PIC 9(04) VALUE ZERO.

       01  WS-SUB                            PIC 9(02) VALUE ZERO.

       01  WS-EXIT                           PIC X(11) VALUE
                                             'Slick ended'.

       LINKAGE SECTION.

       01  DFHCOMMAREA.
           03  FILLER                        PIC X(01)
                                             OCCURS 1 TO 4096 TIMES
                                             DEPENDING ON EIBCALEN.

       PROCEDURE DIVISION.

       000-START-PROCESSING.

           MOVE DFHCOMMAREA           TO SLICK-COMM.
           MOVE LOW-VALUES            TO SLICKM5I.

           IF COMM-INIT
               PERFORM 000-INITIALIZATION

           ELSE
               MOVE COMM-SAVE         TO WS-MAP-FIELDS
               PERFORM 000-RECEIVE-MAP.

           IF COMM-NEXT-TRAN = EIBTRNID
               MOVE WS-MAP-FIELDS     TO COMM-SAVE
               PERFORM 000-INITIALIZE-FIELDS
               PERFORM 000-SEND-MAP

               EXEC CICS RETURN
                   TRANSID (COMM-NEXT-TRAN)
                   COMMAREA (SLICK-COMM)
               END-EXEC

           ELSE
               SET COMM-INIT          TO TRUE

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

           MOVE EIBTRNID              TO COMM-NEXT-TRAN.
           MOVE SPACE                 TO COMM-INIT-FLAG.
           PERFORM 000-READ-STATISTICS.

       000-READ-STATISTICS.

           MOVE COMM-ACCOUNT-ID       TO A-S-ACCOUNT-ID.

           EXEC CICS READ
               FILE      ('ACCTFILE')
               INTO      (ACCOUNT-STATISTICS-RECORD)
               RIDFLD    (A-S-KEY)
               LENGTH    (ACCTFILE-LENGTH)
               KEYLENGTH (ACCTFILE-KEYLENGTH)
               RESP      (ACCTFILE-RESP)
               RESP2     (ACCTFILE-RESP2)
           END-EXEC.

           IF ACCTFILE-RESP NOT = DFHRESP(NORMAL)
               MOVE ACCTFILE-RESP         TO ERROR-RESP
               MOVE ACCTFILE-RESP2        TO ERROR-RESP2
               MOVE A-S-KEY               TO ERROR-KEY
               MOVE 'READ'                TO ERROR-TYPE
               MOVE ERROR-MESSAGE         TO COMM-MESSAGE
           ELSE
               MOVE A-S-NAME-LAST         TO WS-NAME-LAST
               MOVE A-S-NAME-FIRST        TO WS-NAME-FIRST
               MOVE A-S-NAME-INITIAL      TO WS-NAME-INITIAL
               MOVE A-S-ADDRESS           TO WS-ADDRESS
               MOVE A-S-CITY              TO WS-CITY
               MOVE A-S-STATE             TO WS-STATE
               MOVE A-S-ZIP               TO WS-ZIP
               MOVE A-S-ZIP-EX            TO WS-ZIP-EX
               MOVE A-S-TEL-1A            TO WS-TEL-1A
               MOVE A-S-TEL-1B            TO WS-TEL-1B
               MOVE A-S-TEL-1C            TO WS-TEL-1C
               MOVE A-S-TEL-2A            TO WS-TEL-2A
               MOVE A-S-TEL-2B            TO WS-TEL-2B
               MOVE A-S-TEL-2C            TO WS-TEL-2C
               MOVE A-S-LAST-PAYMENT      TO WS-LAST-PAYMENT-DATE
               MOVE A-S-LAST-DELIVERY     TO WS-LAST-DELIVERY-DATE
               MOVE A-S-LAST-MAINTENANCE  TO WS-LAST-SERVICE-DATE
               MOVE A-S-PAYMENT-DUE       TO WS-NEXT-PAYMENT-DATE
               MOVE A-S-BUDGET-AMOUNT     TO WS-NEXT-PAYMENT-AMOUNT
               MOVE A-S-BALANCE           TO WS-BALANCE

               IF NOT NO-PAYMENTS
                   PERFORM 000-READ-PAYMENT.

               IF NOT NO-DELIVERIES
                   PERFORM 000-READ-DELIVERY.

               IF NOT NO-SERVICE
                   PERFORM 000-READ-MAINTENANCE.

               IF A-S-BALANCE < ZERO
                   MOVE 'CR'              TO WS-CR.

       000-READ-PAYMENT.

           MOVE COMM-ACCOUNT-ID       TO A-P-ACCOUNT-ID.

           DIVIDE A-S-PAYMENT-ENTRIES BY 20
               GIVING A-P-RECORD-NUMBER
               REMAINDER WS-SUB.

           EXEC CICS READ
               FILE      ('ACCTFILE')
               INTO      (ACCOUNT-PAYMENT-RECORD)
               RIDFLD    (A-P-KEY)
               LENGTH    (ACCTFILE-LENGTH)
               KEYLENGTH (ACCTFILE-KEYLENGTH)
               RESP      (ACCTFILE-RESP)
               RESP2     (ACCTFILE-RESP2)
           END-EXEC.

           IF ACCTFILE-RESP NOT = DFHRESP(NORMAL)
               MOVE ACCTFILE-RESP         TO ERROR-RESP
               MOVE ACCTFILE-RESP2        TO ERROR-RESP2
               MOVE A-P-KEY               TO ERROR-KEY
               MOVE 'READ'                TO ERROR-TYPE
               MOVE ERROR-MESSAGE         TO COMM-MESSAGE
           ELSE
               MOVE A-P-AMOUNT (WS-SUB)   TO WS-LAST-PAYMENT-AMOUNT.

       000-READ-DELIVERY.

           MOVE COMM-ACCOUNT-ID       TO A-D-ACCOUNT-ID.

           DIVIDE A-S-DELIVERY-ENTRIES BY 20
               GIVING A-D-RECORD-NUMBER
               REMAINDER WS-SUB.

           EXEC CICS READ
               FILE      ('ACCTFILE')
               INTO      (ACCOUNT-DELIVERY-RECORD)
               RIDFLD    (A-D-KEY)
               LENGTH    (ACCTFILE-LENGTH)
               KEYLENGTH (ACCTFILE-KEYLENGTH)
               RESP      (ACCTFILE-RESP)
               RESP2     (ACCTFILE-RESP2)
           END-EXEC.

           IF ACCTFILE-RESP NOT = DFHRESP(NORMAL)
               MOVE ACCTFILE-RESP           TO ERROR-RESP
               MOVE ACCTFILE-RESP2          TO ERROR-RESP2
               MOVE A-D-KEY                 TO ERROR-KEY
               MOVE 'READ'                  TO ERROR-TYPE
               MOVE ERROR-MESSAGE           TO COMM-MESSAGE
           ELSE
               MOVE A-D-GALLONS (WS-SUB)    TO WS-LAST-DELIVERY-AMOUNT
               MOVE A-D-UNIT-PRICE (WS-SUB) TO WS-LAST-DELIVERY-PRICE.

       000-READ-MAINTENANCE.

           MOVE COMM-ACCOUNT-ID       TO A-M-ACCOUNT-ID.

           DIVIDE A-S-MAINTENANCE-ENTRIES BY 10
               GIVING A-M-RECORD-NUMBER
               REMAINDER WS-SUB.

           EXEC CICS READ
               FILE      ('ACCTFILE')
               INTO      (ACCOUNT-MAINTENANCE-RECORD)
               RIDFLD    (A-M-KEY)
               LENGTH    (ACCTFILE-LENGTH)
               KEYLENGTH (ACCTFILE-KEYLENGTH)
               RESP      (ACCTFILE-RESP)
               RESP2     (ACCTFILE-RESP2)
           END-EXEC.

           IF ACCTFILE-RESP NOT = DFHRESP(NORMAL)
               MOVE ACCTFILE-RESP         TO ERROR-RESP
               MOVE ACCTFILE-RESP2        TO ERROR-RESP2
               MOVE A-M-KEY               TO ERROR-KEY
               MOVE 'READ'                TO ERROR-TYPE
               MOVE ERROR-MESSAGE         TO COMM-MESSAGE
           ELSE
               MOVE A-M-SERVICE-CHARGE (WS-SUB)
                                          TO WS-LAST-SERVICE-CHARGE.

       000-INITIALIZE-FIELDS.

           MOVE COMM-ACCOUNT-ID           TO M5IDO.
           MOVE WS-NAME-LAST              TO M5LASTO.
           MOVE WS-NAME-FIRST             TO M5FIRSTO.
           MOVE WS-NAME-INITIAL           TO M5INITO.
           MOVE WS-ADDRESS                TO M5ADDRO.
           MOVE WS-CITY                   TO M5CITYO.
           MOVE WS-STATE                  TO M5STATEO.
           MOVE WS-ZIP                    TO M5ZIPO.
           MOVE WS-ZIP-EX                 TO M5ZIPXO.
           MOVE WS-TEL-1A                 TO WS-TEL-A.
           MOVE WS-TEL-1B                 TO WS-TEL-B.
           MOVE WS-TEL-1C                 TO WS-TEL-C.
           MOVE WS-FORMAT-TEL             TO M5TEL1O.
           MOVE WS-TEL-2A                 TO WS-TEL-A.
           MOVE WS-TEL-2B                 TO WS-TEL-B.
           MOVE WS-TEL-2C                 TO WS-TEL-C.
           MOVE WS-FORMAT-TEL             TO M5TEL2O.
           MOVE WS-L-P-YY                 TO WORK-YY.
           MOVE WS-L-P-MM                 TO WORK-MM.
           MOVE WS-L-P-DD                 TO WORK-DD.
           MOVE WORK-DATE                 TO M5DPAYO.
           MOVE WS-LAST-PAYMENT-AMOUNT    TO NUM-8.
           MOVE DOLLARS-8                 TO D-8.
           MOVE CENTS-8                   TO C-8.
           MOVE DC-8                      TO M5APAYO.
           MOVE WS-L-D-YY                 TO WORK-YY.
           MOVE WS-L-D-MM                 TO WORK-MM.
           MOVE WS-L-D-DD                 TO WORK-DD.
           MOVE WORK-DATE                 TO M5DDELO.
           MOVE WS-LAST-DELIVERY-AMOUNT   TO M5ADELO.
           MOVE WS-LAST-DELIVERY-PRICE    TO NUM-5.
           MOVE DOLLARS-5                 TO D-5.
           MOVE CENTS-5                   TO C-5.
           MOVE DC-5                      TO M5PDELO.
           MOVE WS-L-S-YY                 TO WORK-YY.
           MOVE WS-L-S-MM                 TO WORK-MM.
           MOVE WS-L-S-DD                 TO WORK-DD.
           MOVE WORK-DATE                 TO M5DSVCO.
           MOVE WS-LAST-SERVICE-CHARGE    TO NUM-8.
           MOVE DOLLARS-8                 TO D-8.
           MOVE CENTS-8                   TO C-8.
           MOVE DC-8                      TO M5ASVCO.
           MOVE WS-N-P-YY                 TO WORK-YY.
           MOVE WS-N-P-MM                 TO WORK-MM.
           MOVE WS-N-P-DD                 TO WORK-DD.
           MOVE WORK-DATE                 TO M5DNEXTO.
           MOVE WS-NEXT-PAYMENT-AMOUNT    TO NUM-8.
           MOVE DOLLARS-8                 TO D-8.
           MOVE CENTS-8                   TO C-8.
           MOVE DC-8                      TO M5ANEXTO.
           MOVE WS-BALANCE                TO NUM-8.
           MOVE DOLLARS-8                 TO D-8.
           MOVE CENTS-8                   TO C-8.
           MOVE DC-8                      TO M5BALO.
           MOVE WS-CR                     TO M5CRO.

       000-SEND-MAP.

           MOVE COMM-DISPLAY-DATE     TO M5DATEO.
           MOVE COMM-MESSAGE          TO M5MSGO.
           MOVE SPACE                 TO COMM-MESSAGE.

           IF NOT BAD-DATA
               MOVE -1                TO M5MSGL.

           EXEC CICS SEND
               MAP ('SLICKM5')
               CURSOR
               ERASE
           END-EXEC.

           IF M5MSGO NOT = SPACE
               PERFORM 000-ALARM.

       000-RECEIVE-MAP.

           IF EIBAID = DFHPF11
               SET COMM-MENU          TO TRUE
           ELSE
               IF EIBAID = DFHPF12
               OR EIBAID = DFHPF24
                   SET COMM-QUIT      TO TRUE
               ELSE
                   IF EIBAID NOT = DFHENTER
                       MOVE 'Invalid Key' TO COMM-MESSAGE.

       000-ALARM.

           EXEC CICS SEND CONTROL
               FREEKB
               ALARM
           END-EXEC.

           COPY SLICKNUM.
