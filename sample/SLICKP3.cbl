       CBL XOPTS(COBOL2)
       IDENTIFICATION DIVISION.
       PROGRAM-ID. SLICKP3.
       AUTHOR.     Ira M. Slick.
      ******************************************************************
      *DESCRIPTION: Update Account                                     *
      ******************************************************************
       ENVIRONMENT DIVISION.
       CONFIGURATION SECTION.
       SOURCE-COMPUTER.    IBM-PC.
       OBJECT-COMPUTER.    IBM-PC.
       DATA DIVISION.
       WORKING-STORAGE SECTION.

           COPY SLICKCOM.

           COPY SLICKM3.

           COPY DFHAID.

           COPY DFHBMSCA.

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
           03  WS-ESTIMATED-USAGE            PIC 9(04) VALUE ZERO.
           03  WS-ESTIMATED-COST             PIC 9(08) VALUE ZERO.
           03  WS-BUDGET-AMOUNT              PIC 9(08) VALUE ZERO.
           03  WS-END-OF-CONTRACT.
               05  WS-END-YY                 PIC 9(02) VALUE ZERO.
                   88  ACCOUNT-ACTIVE        VALUE ZERO.
               05  WS-END-MM                 PIC 9(02) VALUE ZERO.
               05  WS-END-DD                 PIC 9(02) VALUE ZERO.
           03  WS-LAST-AID                   PIC X(01) VALUE SPACE.

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
           MOVE LOW-VALUES            TO SLICKM3I.

           IF COMM-INIT
           OR COMM-RESUME
               PERFORM 000-INITIALIZATION

           ELSE
               MOVE COMM-SAVE         TO WS-MAP-FIELDS
               PERFORM 000-RECEIVE-MAP.

           MOVE WS-MAP-FIELDS         TO COMM-SAVE.

           IF COMM-NEXT-TRAN = EIBTRNID
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

           IF COMM-INIT
               PERFORM 000-READ-STATISTICS
           ELSE
               MOVE COMM-SAVE         TO WS-MAP-FIELDS.

           MOVE EIBTRNID              TO COMM-NEXT-TRAN.
           MOVE SPACE                 TO COMM-INIT-FLAG.

           PERFORM 000-INITIALIZE-MAP-FIELDS.

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

           IF ACCTFILE-RESP = DFHRESP(NORMAL)
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
               MOVE A-S-SIZE-OF-TANK      TO WS-SIZE-OF-TANK
               MOVE A-S-SIZE-OF-HOME      TO WS-SIZE-OF-HOME
               MOVE A-S-ESTIMATED-USAGE   TO WS-ESTIMATED-USAGE
               MOVE A-S-UNIT-PRICE        TO WS-UNIT-PRICE
               MOVE A-S-ESTIMATED-COST    TO WS-ESTIMATED-COST
               MOVE A-S-BUDGET-AMOUNT     TO WS-BUDGET-AMOUNT
               MOVE A-S-END-OF-CONTRACT   TO WS-END-OF-CONTRACT

           ELSE
               MOVE 'Error reading account file.'
                                          TO M3MSGO
               SET COMM-MENU              TO TRUE.

       000-INITIALIZE-MAP-FIELDS.

           MOVE WS-NAME-LAST              TO M3LASTO.
           MOVE WS-NAME-FIRST             TO M3FIRSTO.
           MOVE WS-NAME-INITIAL           TO M3INITO.
           MOVE WS-ADDRESS                TO M3ADDRO.
           MOVE WS-CITY                   TO M3CITYO.
           MOVE WS-STATE                  TO M3STATEO.
           MOVE WS-ZIP                    TO M3ZIPO.
           MOVE WS-ZIP-EX                 TO M3ZIPXO.
           MOVE WS-TEL-1A                 TO M3TEL1AO.
           MOVE WS-TEL-1B                 TO M3TEL1BO.
           MOVE WS-TEL-1C                 TO M3TEL1CO.
           MOVE WS-TEL-2A                 TO M3TEL2AO.
           MOVE WS-TEL-2B                 TO M3TEL2BO.
           MOVE WS-TEL-2C                 TO M3TEL2CO.
           MOVE WS-SIZE-OF-TANK           TO M3TANKO.
           MOVE WS-SIZE-OF-HOME           TO M3HOMEO.
           MOVE WS-ESTIMATED-USAGE        TO M3USAGEO.
           MOVE WS-UNIT-PRICE             TO NUM-5.
           MOVE DOLLARS-5                 TO D-5.
           MOVE CENTS-5                   TO C-5.
           MOVE DC-5                      TO M3UNITO.
           MOVE WS-ESTIMATED-COST         TO NUM-8.
           MOVE DOLLARS-8                 TO D-8.
           MOVE CENTS-8                   TO C-8.
           MOVE DC-8                      TO M3COSTO.
           MOVE WS-BUDGET-AMOUNT          TO NUM-8.
           MOVE DOLLARS-8                 TO D-8.
           MOVE CENTS-8                   TO C-8.
           MOVE DC-8                      TO M3PAYO.

       000-SEND-MAP.

           MOVE COMM-DISPLAY-DATE         TO M3DATEO.
           MOVE COMM-ACCOUNT-ID           TO M3IDO.
           MOVE COMM-MESSAGE              TO M3MSGO.
           MOVE SPACE                     TO COMM-MESSAGE.

           IF ACCOUNT-ACTIVE
               MOVE 'Active'              TO M3STATO
               MOVE 'Disable'             TO M3PF5O
               MOVE DFHBMASB              TO M3PF2A
               MOVE DFHBMASB              TO M3PF4A
           ELSE
               MOVE 'Disabled'            TO M3STATO
               MOVE 'Enable'              TO M3PF5O
               MOVE DFHBMDAR              TO M3PF2A
               MOVE DFHBMDAR              TO M3PF4A.

           IF NOT BAD-DATA
               MOVE -1                    TO M3MSGL.

           EXEC CICS SEND
               MAP ('SLICKM3')
               CURSOR
               ERASE
           END-EXEC.

           IF M3MSGO NOT = SPACE
               PERFORM 000-ALARM.

       000-RECEIVE-MAP.

           IF EIBAID = DFHPF11
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
               MAP   ('SLICKM3')
               RESP  (COMM-RESP)
           END-EXEC.

           PERFORM 000-VALIDATE-MAP-FIELDS.
           SET MAP-RECEIVED           TO TRUE.

           IF EIBAID = DFHENTER
               NEXT SENTENCE
           ELSE
               IF EIBAID NOT = DFHPF1 AND DFHPF2 AND DFHPF3
                           AND DFHPF4 AND DFHPF5
                   MOVE 'Invalid Key.'
                                      TO COMM-MESSAGE
               ELSE
                   IF BAD-DATA
                       MOVE 'Enter required field(s).'
                                      TO COMM-MESSAGE
                   ELSE
                       PERFORM 000-PROCESS-REQUEST.

       000-VALIDATE-MAP-FIELDS.

      * Unit Cost (Required)
           IF M3UNITL = ZERO
               MOVE WS-UNIT-PRICE      TO NUM-5
               MOVE DOLLARS-5          TO D-5
               MOVE CENTS-5            TO C-5
               MOVE DC-5               TO M3UNITI
           ELSE
               SET MAP-CHANGED         TO TRUE
               MOVE 5                  TO WS-FIELD-SIZE
               MOVE M3UNITI            TO CH-ARRAY
               PERFORM 000-MONEY-FIELD
               MOVE CH-ARRAY           TO WS-UNIT-PRICE
               MOVE DC-5               TO M3UNITI.

           IF WS-UNIT-PRICE = ZERO
               SET BAD-DATA            TO TRUE
               MOVE -1                 TO M3UNITL.

      * Tank Size (Required)
           IF M3TANKL = ZERO
               MOVE WS-SIZE-OF-TANK    TO M3TANKI
           ELSE
               SET MAP-CHANGED         TO TRUE
               MOVE 4                  TO WS-FIELD-SIZE
               MOVE M3TANKI            TO CH-ARRAY
               PERFORM 000-NUM-FIELD
               MOVE CH-ARRAY           TO WS-SIZE-OF-TANK
               MOVE CH-ARRAY           TO M3TANKI.

           IF WS-SIZE-OF-TANK = ZERO
               SET BAD-DATA            TO TRUE
               MOVE -1                 TO M3TANKL.

      * Estimated Usage (Required)
           IF M3USAGEL = ZERO
               MOVE WS-ESTIMATED-USAGE TO M3USAGEI
           ELSE
               SET MAP-CHANGED         TO TRUE
               MOVE 4                  TO WS-FIELD-SIZE
               MOVE M3USAGEI           TO CH-ARRAY
               PERFORM 000-NUM-FIELD
               MOVE CH-ARRAY           TO WS-ESTIMATED-USAGE
               MOVE CH-ARRAY           TO M3USAGEI.

           IF WS-ESTIMATED-USAGE = ZERO
               SET BAD-DATA            TO TRUE
               MOVE -1                 TO M3USAGEL.

      * Home Size (Required)
           IF M3HOMEL = ZERO
               MOVE WS-SIZE-OF-HOME    TO M3HOMEI
           ELSE
               SET MAP-CHANGED         TO TRUE
               MOVE 4                  TO WS-FIELD-SIZE
               MOVE M3HOMEI            TO CH-ARRAY
               PERFORM 000-NUM-FIELD
               MOVE CH-ARRAY           TO WS-SIZE-OF-HOME
               MOVE CH-ARRAY           TO M3HOMEI.

           IF WS-SIZE-OF-HOME = ZERO
               SET BAD-DATA            TO TRUE
               MOVE -1                 TO M3HOMEL.

      * Office Telephone (Optional)
           IF M3TEL2CL = ZERO
               MOVE WS-TEL-2C         TO M3TEL2CI
           ELSE
               SET MAP-CHANGED        TO TRUE
               MOVE 3                 TO WS-FIELD-SIZE
               MOVE M3TEL2CI          TO CH-ARRAY
               PERFORM 000-NUM-FIELD
               MOVE CH-ARRAY          TO WS-TEL-2C
               MOVE CH-ARRAY          TO M3TEL2CI.

      * Office Telephone Exchange (Optional)
           IF M3TEL2BL = ZERO
               MOVE WS-TEL-2B         TO M3TEL2BI
           ELSE
               SET MAP-CHANGED        TO TRUE
               MOVE 3                 TO WS-FIELD-SIZE
               MOVE M3TEL2BI          TO CH-ARRAY
               PERFORM 000-NUM-FIELD
               MOVE CH-ARRAY          TO WS-TEL-2B
               MOVE CH-ARRAY          TO M3TEL2BI.

      * Office Telephone Area Code (Optional)
           IF M3TEL2AL = ZERO
               MOVE WS-TEL-2A         TO M3TEL2AI
           ELSE
               SET MAP-CHANGED        TO TRUE
               MOVE 3                 TO WS-FIELD-SIZE
               MOVE M3TEL2AI          TO CH-ARRAY
               PERFORM 000-NUM-FIELD
               MOVE CH-ARRAY          TO WS-TEL-2A
               MOVE CH-ARRAY          TO M3TEL2AI.

      * Home Telephone (Required)
           IF M3TEL1CL = ZERO
               MOVE WS-TEL-1C         TO M3TEL1CI
           ELSE
               SET MAP-CHANGED        TO TRUE
               MOVE 4                 TO WS-FIELD-SIZE
               MOVE M3TEL1CI          TO CH-ARRAY
               PERFORM 000-NUM-FIELD
               MOVE CH-ARRAY          TO WS-TEL-1C
               MOVE CH-ARRAY          TO M3TEL1CI.

      * Home Telephone Exchange (Required)
           IF M3TEL1BL = ZERO
               MOVE WS-TEL-1B         TO M3TEL1BI
           ELSE
               SET MAP-CHANGED        TO TRUE
               MOVE 3                 TO WS-FIELD-SIZE
               MOVE M3TEL1BI          TO CH-ARRAY
               PERFORM 000-NUM-FIELD
               MOVE CH-ARRAY          TO WS-TEL-1B
               MOVE CH-ARRAY          TO M3TEL1BI.

           IF WS-TEL-1B = ZERO
               SET BAD-DATA           TO TRUE
               MOVE -1                TO M3TEL1BL.

      * Home Telephone Area Code (Required)
           IF M3TEL1AL = ZERO
               MOVE WS-TEL-1A         TO M3TEL1AI
           ELSE
               SET MAP-CHANGED        TO TRUE
               MOVE 3                 TO WS-FIELD-SIZE
               MOVE M3TEL1AI          TO CH-ARRAY
               PERFORM 000-NUM-FIELD
               MOVE CH-ARRAY          TO WS-TEL-1A
               MOVE CH-ARRAY          TO M3TEL1AI.

           IF WS-TEL-1A = ZERO
               SET BAD-DATA           TO TRUE
               MOVE -1                TO M3TEL1AL.

      * Extended Zip Code (Optional)
           IF M3ZIPXL = ZERO
               MOVE WS-ZIP-EX         TO M3ZIPXI
           ELSE
               SET MAP-CHANGED        TO TRUE
               MOVE 4                 TO WS-FIELD-SIZE
               MOVE M3ZIPXI           TO CH-ARRAY
               PERFORM 000-NUM-FIELD
               MOVE CH-ARRAY          TO WS-ZIP-EX
               MOVE CH-ARRAY          TO M3ZIPXI.

      * Zip Code (Required)
           IF M3ZIPL = ZERO
               MOVE WS-ZIP            TO M3ZIPI
           ELSE
               SET MAP-CHANGED        TO TRUE
               MOVE 5                 TO WS-FIELD-SIZE
               MOVE M3ZIPI            TO CH-ARRAY
               PERFORM 000-NUM-FIELD
               MOVE CH-ARRAY          TO WS-ZIP
               MOVE CH-ARRAY          TO M3ZIPI.

           IF WS-ZIP = ZERO
               SET BAD-DATA           TO TRUE
               MOVE -1                TO M3ZIPL.

      * State Name (Required)
           IF M3STATEL = ZERO
               MOVE WS-STATE          TO M3STATEI
           ELSE
               INSPECT M3STATEI REPLACING ALL '_' BY ' '
               SET MAP-CHANGED        TO TRUE
               MOVE M3STATEI          TO WS-STATE.

           IF WS-STATE = SPACE
               SET BAD-DATA           TO TRUE
               MOVE ALL '_'           TO M3STATEI
               MOVE -1                TO M3STATEL.

      * City (Required)
           IF M3CITYL = ZERO
               MOVE WS-CITY           TO M3CITYI
           ELSE
               INSPECT M3CITYI REPLACING ALL '_' BY ' '
               SET MAP-CHANGED        TO TRUE
               MOVE M3CITYI           TO WS-CITY.

           IF WS-CITY = SPACE
               SET BAD-DATA           TO TRUE
               MOVE ALL '_'           TO M3CITYI
               MOVE -1                TO M3CITYL.

      * Address (Required)
           IF M3ADDRL = ZERO
               MOVE WS-ADDRESS        TO M3ADDRI
           ELSE
               INSPECT M3ADDRI REPLACING ALL '_' BY ' '
               SET MAP-CHANGED        TO TRUE
               MOVE M3ADDRI           TO WS-ADDRESS.

           IF WS-ADDRESS = SPACE
               SET BAD-DATA           TO TRUE
               MOVE ALL '_'           TO M3ADDRI
               MOVE -1                TO M3ADDRL.

      * Middle Initial (Optional)
           IF M3INITL = ZERO
               MOVE WS-NAME-INITIAL   TO M3INITI
           ELSE
               INSPECT M3INITI REPLACING ALL '_' BY ' '
               SET MAP-CHANGED        TO TRUE
               MOVE M3INITI           TO WS-NAME-INITIAL.

           IF WS-NAME-INITIAL = SPACE
               MOVE ALL '_'           TO M3INITI.

      * First Name (Required)
           IF M3FIRSTL = ZERO
               MOVE WS-NAME-FIRST     TO M3FIRSTI
           ELSE
               INSPECT M3FIRSTI REPLACING ALL '_' BY ' '
               SET MAP-CHANGED        TO TRUE
               MOVE M3FIRSTI          TO WS-NAME-FIRST.

           IF WS-NAME-FIRST = SPACE
               SET BAD-DATA           TO TRUE
               MOVE ALL '_'           TO M3FIRSTI
               MOVE -1                TO M3FIRSTL.

      * Last Name (Required)
           IF M3LASTL = ZERO
               MOVE WS-NAME-LAST      TO M3LASTI
           ELSE
               INSPECT M3LASTI REPLACING ALL '_' BY ' '
               SET MAP-CHANGED        TO TRUE
               MOVE M3LASTI           TO WS-NAME-LAST.

           IF WS-NAME-LAST = SPACE
               SET BAD-DATA           TO TRUE
               MOVE ALL '_'           TO M3LASTI
               MOVE -1                TO M3LASTL.

      * Adjust the Estimated Usage if the Size of Home has changed
           IF M3HOMEL > ZERO
               COMPUTE WS-ESTIMATED-USAGE = (WS-SIZE-OF-HOME * 2) / 3
               MOVE WS-ESTIMATED-USAGE TO M3USAGEI
               MOVE ZERO               TO M3USAGEL.

      * Estimated Cost (Calculated)
           COMPUTE WS-ESTIMATED-COST =
               (WS-ESTIMATED-USAGE * WS-UNIT-PRICE).

           MOVE WS-ESTIMATED-COST         TO NUM-8.
           MOVE DOLLARS-8                 TO D-8.
           MOVE CENTS-8                   TO C-8.
           MOVE DC-8                      TO M3COSTO.

      * Budget Amount (Calculated)
           COMPUTE WS-BUDGET-AMOUNT =
               ((((WS-ESTIMATED-COST / 12) + 50) / 1000) * 1000).

           MOVE WS-BUDGET-AMOUNT          TO NUM-8.
           MOVE DOLLARS-8                 TO D-8.
           MOVE CENTS-8                   TO C-8.
           MOVE DC-8                      TO M3PAYO.

       000-PROCESS-REQUEST.

           IF EIBAID = DFHPF1
               IF WS-LAST-AID NOT = DFHPF1
               OR MAP-CHANGED
                   MOVE 'Hit PF1 to confirm update.'
                                            TO COMM-MESSAGE
               ELSE
                   PERFORM 000-UPDATE-ACCOUNT
           ELSE
           IF EIBAID = DFHPF2
               IF  ACCOUNT-ACTIVE
                   SET COMM-UPDATE-DELIVERY TO TRUE
                   SET COMM-INIT            TO TRUE
               ELSE
                   MOVE 'Invalid key for disabled account.'
                                            TO COMM-MESSAGE
           ELSE
           IF EIBAID = DFHPF3
               SET COMM-UPDATE-PAYMENT      TO TRUE
               SET COMM-INIT                TO TRUE
           ELSE
           IF EIBAID = DFHPF4
               IF  ACCOUNT-ACTIVE
                   SET COMM-UPDATE-SERVICE  TO TRUE
                   SET COMM-INIT            TO TRUE
               ELSE
                   MOVE 'Invalid key for disabled account.'
                                            TO COMM-MESSAGE
           ELSE
           IF EIBAID = DFHPF5
               IF ACCOUNT-ACTIVE
                   MOVE COMM-TRAN-DATE      TO WS-END-OF-CONTRACT
               ELSE
                   MOVE ZERO                TO WS-END-OF-CONTRACT.

       000-UPDATE-ACCOUNT.

           MOVE COMM-ACCOUNT-ID       TO A-S-ACCOUNT-ID.

           EXEC CICS READ
               FILE      ('ACCTFILE')
               INTO      (ACCOUNT-STATISTICS-RECORD)
               RIDFLD    (A-S-KEY)
               LENGTH    (ACCTFILE-LENGTH)
               KEYLENGTH (ACCTFILE-KEYLENGTH)
               RESP      (ACCTFILE-RESP)
               RESP2     (ACCTFILE-RESP2)
               UPDATE
           END-EXEC.

           IF ACCTFILE-RESP NOT = DFHRESP(NORMAL)
               MOVE 'Error updating account file.'
                                         TO COMM-MESSAGE
           ELSE
               MOVE WS-NAME-LAST         TO A-S-NAME-LAST
               MOVE WS-NAME-FIRST        TO A-S-NAME-FIRST
               MOVE WS-NAME-INITIAL      TO A-S-NAME-INITIAL
               MOVE WS-ADDRESS           TO A-S-ADDRESS
               MOVE WS-CITY              TO A-S-CITY
               MOVE WS-STATE             TO A-S-STATE
               MOVE WS-ZIP               TO A-S-ZIP
               MOVE WS-ZIP-EX            TO A-S-ZIP-EX
               MOVE WS-TEL-1A            TO A-S-TEL-1A
               MOVE WS-TEL-1B            TO A-S-TEL-1B
               MOVE WS-TEL-1C            TO A-S-TEL-1C
               MOVE WS-TEL-2A            TO A-S-TEL-2A
               MOVE WS-TEL-2B            TO A-S-TEL-2B
               MOVE WS-TEL-2C            TO A-S-TEL-2C
               MOVE WS-SIZE-OF-TANK      TO A-S-SIZE-OF-TANK
               MOVE WS-SIZE-OF-HOME      TO A-S-SIZE-OF-HOME
               MOVE WS-UNIT-PRICE        TO A-S-UNIT-PRICE
               MOVE WS-ESTIMATED-USAGE   TO A-S-ESTIMATED-USAGE
               MOVE WS-ESTIMATED-COST    TO A-S-ESTIMATED-COST
               MOVE WS-BUDGET-AMOUNT     TO A-S-BUDGET-AMOUNT
               MOVE WS-END-OF-CONTRACT   TO A-S-END-OF-CONTRACT

               EXEC CICS REWRITE
                   FILE       ('ACCTFILE')
                   FROM       (ACCOUNT-STATISTICS-RECORD)
                   LENGTH     (ACCTFILE-LENGTH)
                   RESP       (ACCTFILE-RESP)
                   RESP2      (ACCTFILE-RESP2)
               END-EXEC

               IF ACCTFILE-RESP NOT = DFHRESP(NORMAL)
                   MOVE ACCTFILE-RESP     TO ERROR-RESP
                   MOVE ACCTFILE-RESP2    TO ERROR-RESP2
                   MOVE A-S-KEY           TO ERROR-KEY
                   MOVE 'Rewrite'         TO ERROR-TYPE
                   MOVE ERROR-MESSAGE     TO COMM-MESSAGE
               ELSE
                   MOVE 'Account updated.'
                                          TO COMM-MESSAGE.

       000-ALARM.

           EXEC CICS SEND CONTROL
               FREEKB
               ALARM
           END-EXEC.

           COPY SLICKNUM.
