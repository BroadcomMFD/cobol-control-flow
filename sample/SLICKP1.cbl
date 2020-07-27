       CBL XOPTS(COBOL2)
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SLICKP1.
       AUTHOR.     Ira M. Slick.
      ******************************************************************
      *DESCRIPTION: Add Account                                        *
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.    IBM-PC.
       OBJECT-COMPUTER.    IBM-PC.
       DATA DIVISION.
       WORKING-STORAGE SECTION.

           COPY SLICKCOM.

           COPY SLICKM1.

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
           03  WS-SIZE-OF-HOME               PIC 9(04) VALUE ZERO.
           03  WS-SIZE-OF-TANK               PIC 9(04) VALUE ZERO.
           03  WS-UNIT-PRICE                 PIC 9(05) VALUE ZERO.
           03  WS-LAST-AID                   PIC X(01) VALUE SPACE.

       01  TEMP-DATE.
           03  TEMP-YY                       PIC 9(02) VALUE ZERO.
           03  TEMP-MM                       PIC 9(02) VALUE ZERO.
           03  TEMP-DD                       PIC 9(02) VALUE ZERO.

       01  ACCOUNT-ADDED-MESSAGE.
           03  FILLER                        PIC X(08) VALUE 'Account '.
           03  A-A-ACCOUNT-ID                PIC 9(04) VALUE ZERO.
           03  FILLER                        PIC X(07) VALUE ' added.'.

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
           MOVE LOW-VALUES            TO SLICKM1I.

           IF COMM-INIT
               PERFORM 000-INITIALIZATION

           ELSE
               MOVE COMM-SAVE         TO WS-MAP-FIELDS
               PERFORM 000-RECEIVE-MAP.

           IF COMM-NEXT-TRAN = EIBTRNID
               MOVE WS-MAP-FIELDS     TO COMM-SAVE
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

       000-SEND-MAP.

           MOVE COMM-DISPLAY-DATE     TO M1DATEO.
           MOVE COMM-MESSAGE          TO M1MSGO.
           MOVE SPACE                 TO COMM-MESSAGE.

           IF NOT BAD-DATA
               MOVE -1                TO M1LASTL.

           EXEC CICS SEND
               MAP ('SLICKM1')
               CURSOR
               ERASE
           END-EXEC.

           IF M1MSGO NOT = SPACE
               PERFORM 000-ALARM.

       000-RECEIVE-MAP.

           IF EIBAID = DFHPF11
               MOVE 'Transaction cancelled.' TO COMM-MESSAGE
               SET COMM-MENU          TO TRUE
           ELSE
               IF EIBAID = DFHPF12
               OR EIBAID = DFHPF24
                   SET COMM-QUIT      TO TRUE
               ELSE
                   PERFORM 000-PROCESS-FIELDS
                   MOVE EIBAID        TO WS-LAST-AID.

       000-PROCESS-FIELDS.

           EXEC CICS RECEIVE
               MAP   ('SLICKM1')
               RESP  (COMM-RESP)
           END-EXEC.

           PERFORM 000-VALIDATE-MAP-FIELDS.
           SET MAP-RECEIVED           TO TRUE.

           IF EIBAID = DFHENTER
               NEXT SENTENCE
           ELSE
               IF EIBAID NOT = DFHPF1
                   MOVE 'Invalid Key.'
                                      TO COMM-MESSAGE
               ELSE
                   IF BAD-DATA
                       MOVE 'Enter required field(s).'
                                      TO COMM-MESSAGE
                   ELSE
                       IF WS-LAST-AID NOT = DFHPF1
                       OR MAP-CHANGED
                           MOVE 'Hit PF1 to confirm request.'
                                      TO COMM-MESSAGE
                       ELSE
                           PERFORM 000-ADD-ACCOUNT.

       000-VALIDATE-MAP-FIELDS.

      * Unit Cost (Required)
           IF M1UNITL = ZERO
               MOVE WS-UNIT-PRICE     TO NUM-5
               MOVE DOLLARS-5         TO D-5
               MOVE CENTS-5           TO C-5
               MOVE DC-5              TO M1UNITI
           ELSE
               SET MAP-CHANGED        TO TRUE
               MOVE 5                 TO WS-FIELD-SIZE
               MOVE M1UNITI           TO CH-ARRAY
               PERFORM 000-MONEY-FIELD
               MOVE CH-ARRAY          TO WS-UNIT-PRICE
               MOVE DC-5              TO M1UNITI.

           IF WS-UNIT-PRICE = ZERO
               SET BAD-DATA           TO TRUE
               MOVE -1                TO M1UNITL.

      * Tank Size (Required)
           IF M1TANKL = ZERO
               MOVE WS-SIZE-OF-TANK   TO M1TANKI
           ELSE
               SET MAP-CHANGED        TO TRUE
               MOVE 4                 TO WS-FIELD-SIZE
               MOVE M1TANKI           TO CH-ARRAY
               PERFORM 000-NUM-FIELD
               MOVE CH-ARRAY          TO WS-SIZE-OF-TANK
               MOVE CH-ARRAY          TO M1TANKI.

           IF WS-SIZE-OF-TANK = ZERO
               SET BAD-DATA           TO TRUE
               MOVE -1                TO M1TANKL.

      * Home Size (Required)
           IF M1HOMEL = ZERO
               MOVE WS-SIZE-OF-HOME   TO M1HOMEI
           ELSE
               SET MAP-CHANGED        TO TRUE
               MOVE 4                 TO WS-FIELD-SIZE
               MOVE M1HOMEI           TO CH-ARRAY
               PERFORM 000-NUM-FIELD
               MOVE CH-ARRAY          TO WS-SIZE-OF-HOME
               MOVE CH-ARRAY          TO M1HOMEI.

           IF WS-SIZE-OF-HOME = ZERO
               SET BAD-DATA           TO TRUE
               MOVE -1                TO M1HOMEL.

      * Office Telephone (Optional)
           IF M1TEL2CL = ZERO
               MOVE WS-TEL-2C         TO M1TEL2CI
           ELSE
               SET MAP-CHANGED        TO TRUE
               MOVE 3                 TO WS-FIELD-SIZE
               MOVE M1TEL2CI          TO CH-ARRAY
               PERFORM 000-NUM-FIELD
               MOVE CH-ARRAY          TO WS-TEL-2C
               MOVE CH-ARRAY          TO M1TEL2CI.

      * Office Telephone Exchange (Optional)
           IF M1TEL2BL = ZERO
               MOVE WS-TEL-2B         TO M1TEL2BI
           ELSE
               SET MAP-CHANGED        TO TRUE
               MOVE 3                 TO WS-FIELD-SIZE
               MOVE M1TEL2BI          TO CH-ARRAY
               PERFORM 000-NUM-FIELD
               MOVE CH-ARRAY          TO WS-TEL-2B
               MOVE CH-ARRAY          TO M1TEL2BI.

      * Office Telephone Area Code (Optional)
           IF M1TEL2AL = ZERO
               MOVE WS-TEL-2A         TO M1TEL2AI
           ELSE
               SET MAP-CHANGED        TO TRUE
               MOVE 3                 TO WS-FIELD-SIZE
               MOVE M1TEL2AI          TO CH-ARRAY
               PERFORM 000-NUM-FIELD
               MOVE CH-ARRAY          TO WS-TEL-2A
               MOVE CH-ARRAY          TO M1TEL2AI.

      * Home Telephone (Required)
           IF M1TEL1CL = ZERO
               MOVE WS-TEL-1C         TO M1TEL1CI
           ELSE
               SET MAP-CHANGED        TO TRUE
               MOVE 4                 TO WS-FIELD-SIZE
               MOVE M1TEL1CI          TO CH-ARRAY
               PERFORM 000-NUM-FIELD
               MOVE CH-ARRAY          TO WS-TEL-1C
               MOVE CH-ARRAY          TO M1TEL1CI.

      * Home Telephone Exchange (Required)
           IF M1TEL1BL = ZERO
               MOVE WS-TEL-1B         TO M1TEL1BI
           ELSE
               SET MAP-CHANGED        TO TRUE
               MOVE 3                 TO WS-FIELD-SIZE
               MOVE M1TEL1BI          TO CH-ARRAY
               PERFORM 000-NUM-FIELD
               MOVE CH-ARRAY          TO WS-TEL-1B
               MOVE CH-ARRAY          TO M1TEL1BI.

           IF WS-TEL-1B = ZERO
               SET BAD-DATA           TO TRUE
               MOVE -1                TO M1TEL1BL.

      * Home Telephone Area Code (Required)
           IF M1TEL1AL = ZERO
               MOVE WS-TEL-1A         TO M1TEL1AI
           ELSE
               SET MAP-CHANGED        TO TRUE
               MOVE 3                 TO WS-FIELD-SIZE
               MOVE M1TEL1AI          TO CH-ARRAY
               PERFORM 000-NUM-FIELD
               MOVE CH-ARRAY          TO WS-TEL-1A
               MOVE CH-ARRAY          TO M1TEL1AI.

           IF WS-TEL-1A = ZERO
               SET BAD-DATA           TO TRUE
               MOVE -1                TO M1TEL1AL.

      * Extended Zip Code (Optional)
           IF M1ZIPXL = ZERO
               MOVE WS-ZIP-EX         TO M1ZIPXI
           ELSE
               SET MAP-CHANGED        TO TRUE
               MOVE 4                 TO WS-FIELD-SIZE
               MOVE M1ZIPXI           TO CH-ARRAY
               PERFORM 000-NUM-FIELD
               MOVE CH-ARRAY          TO WS-ZIP-EX
               MOVE CH-ARRAY          TO M1ZIPXI.

      * Zip Code (Required)
           IF M1ZIPL = ZERO
               MOVE WS-ZIP            TO M1ZIPI
           ELSE
               SET MAP-CHANGED        TO TRUE
               MOVE 5                 TO WS-FIELD-SIZE
               MOVE M1ZIPI            TO CH-ARRAY
               PERFORM 000-NUM-FIELD
               MOVE CH-ARRAY          TO WS-ZIP
               MOVE CH-ARRAY          TO M1ZIPI.

           IF WS-ZIP = ZERO
               SET BAD-DATA           TO TRUE
               MOVE -1                TO M1ZIPL.

      * State Name (Required)
           IF M1STATEL = ZERO
               MOVE WS-STATE          TO M1STATEI
           ELSE
               INSPECT M1STATEI REPLACING ALL '_' BY ' '
               SET MAP-CHANGED        TO TRUE
               MOVE M1STATEI          TO WS-STATE.

           IF WS-STATE = SPACE
               SET BAD-DATA           TO TRUE
               MOVE ALL '_'           TO M1STATEI
               MOVE -1                TO M1STATEL.

      * City (Required)
           IF M1CITYL = ZERO
               MOVE WS-CITY           TO M1CITYI
           ELSE
               INSPECT M1CITYI REPLACING ALL '_' BY ' '
               SET MAP-CHANGED        TO TRUE
               MOVE M1CITYI           TO WS-CITY.

           IF WS-CITY = SPACE
               SET BAD-DATA           TO TRUE
               MOVE ALL '_'           TO M1CITYI
               MOVE -1                TO M1CITYL.

      * Address (Required)
           IF M1ADDRL = ZERO
               MOVE WS-ADDRESS        TO M1ADDRI
           ELSE
               INSPECT M1ADDRI REPLACING ALL '_' BY ' '
               SET MAP-CHANGED        TO TRUE
               MOVE M1ADDRI           TO WS-ADDRESS.

           IF WS-ADDRESS = SPACE
               SET BAD-DATA           TO TRUE
               MOVE ALL '_'           TO M1ADDRI
               MOVE -1                TO M1ADDRL.

      * Middle Initial (Optional)
           IF M1INITL = ZERO
               MOVE WS-NAME-INITIAL   TO M1INITI
           ELSE
               INSPECT M1INITI REPLACING ALL '_' BY ' '
               SET MAP-CHANGED        TO TRUE
               MOVE M1INITI           TO WS-NAME-INITIAL.

           IF WS-NAME-INITIAL = SPACE
               MOVE ALL '_'           TO M1INITI.

      * First Name (Required)
           IF M1FIRSTL = ZERO
               MOVE WS-NAME-FIRST     TO M1FIRSTI
           ELSE
               INSPECT M1FIRSTI REPLACING ALL '_' BY ' '
               SET MAP-CHANGED        TO TRUE
               MOVE M1FIRSTI          TO WS-NAME-FIRST.

           IF WS-NAME-FIRST = SPACE
               SET BAD-DATA           TO TRUE
               MOVE ALL '_'           TO M1FIRSTI
               MOVE -1                TO M1FIRSTL.

      * Last Name (Required)
           IF M1LASTL = ZERO
               MOVE WS-NAME-LAST      TO M1LASTI
           ELSE
               INSPECT M1LASTI REPLACING ALL '_' BY ' '
               SET MAP-CHANGED        TO TRUE
               MOVE M1LASTI           TO WS-NAME-LAST.

           IF WS-NAME-LAST = SPACE
               SET BAD-DATA           TO TRUE
               MOVE ALL '_'           TO M1LASTI
               MOVE -1                TO M1LASTL.

       000-ADD-ACCOUNT.

           COMPUTE COMM-ACCOUNT-ID = COMM-MAX-ACCOUNT-ID + 1.
           MOVE ZERO TO A-S-RECORD-NUMBER.

           PERFORM 000-CHECK-FOR-DUPLICATE
               VARYING A-S-ACCOUNT-ID FROM COMM-MIN-ACCOUNT-ID BY 1
                 UNTIL A-S-ACCOUNT-ID = COMM-ACCOUNT-ID
                    OR A-S-NAME-LAST  = M1LASTI
                   AND A-S-ADDRESS    = M1ADDRI.

           IF A-S-ACCOUNT-ID < COMM-ACCOUNT-ID
               MOVE 'Duplicate account data.'
                                        TO COMM-MESSAGE
               PERFORM 000-ALARM

           ELSE
               MOVE WS-NAME-LAST        TO A-S-NAME-LAST
               MOVE WS-NAME-FIRST       TO A-S-NAME-FIRST
               MOVE WS-NAME-INITIAL     TO A-S-NAME-INITIAL
               MOVE WS-ADDRESS          TO A-S-ADDRESS
               MOVE WS-CITY             TO A-S-CITY
               MOVE WS-STATE            TO A-S-STATE
               MOVE WS-ZIP              TO A-S-ZIP
               MOVE WS-ZIP-EX           TO A-S-ZIP-EX
               MOVE WS-TEL-1A           TO A-S-TEL-1A
               MOVE WS-TEL-1B           TO A-S-TEL-1B
               MOVE WS-TEL-1C           TO A-S-TEL-1C
               MOVE WS-TEL-2A           TO A-S-TEL-2A
               MOVE WS-TEL-2B           TO A-S-TEL-2B
               MOVE WS-TEL-2C           TO A-S-TEL-2C
               MOVE WS-SIZE-OF-TANK     TO A-S-SIZE-OF-TANK
               MOVE WS-SIZE-OF-HOME     TO A-S-SIZE-OF-HOME
               MOVE WS-UNIT-PRICE       TO A-S-UNIT-PRICE

               COMPUTE A-S-ESTIMATED-USAGE = (WS-SIZE-OF-HOME * 2) / 3

               COMPUTE A-S-ESTIMATED-COST  =
                   (A-S-ESTIMATED-USAGE * WS-UNIT-PRICE)

               COMPUTE A-S-BUDGET-AMOUNT =
                   (((A-S-ESTIMATED-COST / 12) + 50) / 1000) * 1000

               MOVE COMM-TRAN-DATE      TO A-S-START-OF-CONTRACT
               MOVE ZERO                TO A-S-END-OF-CONTRACT

               MOVE ZERO                TO A-S-MAINTENANCE-ENTRIES
               MOVE ZERO                TO A-S-LAST-MAINTENANCE

               MOVE ZERO                TO A-S-DELIVERY-ENTRIES
               MOVE ZERO                TO A-S-LAST-DELIVERY

               MOVE ZERO                TO A-S-PAYMENT-ENTRIES
               MOVE ZERO                TO A-S-LAST-PAYMENT

               MOVE COMM-TRAN-DATE      TO TEMP-DATE

               ADD 1                    TO TEMP-MM

               IF TEMP-MM = 12
                   MOVE 1               TO TEMP-MM
                   ADD 1                TO TEMP-YY
               ELSE
                   ADD 1                TO TEMP-MM
               END-IF

               IF TEMP-DD > 28
                   MOVE 28              TO TEMP-DD
               END-IF

               MOVE TEMP-DATE           TO A-S-PAYMENT-DUE
               MOVE ZERO                TO A-S-BALANCE

               EXEC CICS WRITE
                   FILE       ('ACCTFILE')
                   FROM       (ACCOUNT-STATISTICS-RECORD)
                   RIDFLD     (A-S-KEY)
                   LENGTH     (ACCTFILE-LENGTH)
                   KEYLENGTH  (ACCTFILE-KEYLENGTH)
                   RESP       (ACCTFILE-RESP)
                   RESP2      (ACCTFILE-RESP2)
               END-EXEC

               IF ACCTFILE-RESP NOT = DFHRESP(NORMAL)
                   MOVE 'SE13'    TO COMM-ABCODE
                   EXEC CICS ABEND
                       ABCODE (COMM-ABCODE)
                       NODUMP
                   END-EXEC
               ELSE
                   EXEC CICS READ
                       FILE       ('ACCTFILE')
                       INTO       (ACCOUNT-CONTROL-RECORD)
                       RIDFLD     (A-C-KEY)
                       LENGTH     (ACCTFILE-LENGTH)
                       KEYLENGTH  (ACCTFILE-KEYLENGTH)
                       RESP       (ACCTFILE-RESP)
                       RESP2      (ACCTFILE-RESP2)
                       UPDATE
                   END-EXEC

                   MOVE A-S-ACCOUNT-ID  TO A-C-MAX-ACCOUNT-ID

                   EXEC CICS REWRITE
                       FILE       ('ACCTFILE')
                       FROM       (ACCOUNT-CONTROL-RECORD)
                       LENGTH     (ACCTFILE-LENGTH)
                       RESP       (ACCTFILE-RESP)
                       RESP2      (ACCTFILE-RESP2)
                   END-EXEC

                   IF ACCTFILE-RESP NOT = DFHRESP(NORMAL)
                       MOVE 'SE12' TO COMM-ABCODE
                       EXEC CICS ABEND
                           ABCODE (COMM-ABCODE)
                           NODUMP
                       END-EXEC
                   ELSE
                       MOVE A-S-ACCOUNT-ID        TO COMM-MAX-ACCOUNT-ID
                       MOVE A-S-ACCOUNT-ID        TO A-A-ACCOUNT-ID
                       MOVE ACCOUNT-ADDED-MESSAGE TO COMM-MESSAGE
                       SET COMM-MENU              TO TRUE.

       000-CHECK-FOR-DUPLICATE.

           EXEC CICS READ
               FILE       ('ACCTFILE')
               INTO       (ACCOUNT-STATISTICS-RECORD)
               RIDFLD     (A-S-KEY)
               LENGTH     (ACCTFILE-LENGTH)
               KEYLENGTH  (ACCTFILE-KEYLENGTH)
               RESP       (ACCTFILE-RESP)
               RESP2      (ACCTFILE-RESP2)
           END-EXEC.

           IF ACCTFILE-RESP NOT = DFHRESP(NORMAL)
               MOVE 'SE11' TO COMM-ABCODE
               EXEC CICS ABEND
                   ABCODE (COMM-ABCODE)
                   NODUMP
               END-EXEC.

       000-ALARM.

           EXEC CICS SEND CONTROL
               FREEKB
               ALARM
           END-EXEC.

           COPY SLICKNUM.
