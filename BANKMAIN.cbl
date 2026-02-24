       IDENTIFICATION DIVISION.
       PROGRAM-ID. BANKMAIN.
      *============================================================*
      * FIRST NATIONAL BANK - CORE BANKING SYSTEM                  *
      * BATCH/ONLINE ACCOUNT PROCESSING ENGINE v4.2.1              *
      *------------------------------------------------------------*
      * PROD DSNAME: FNB.PROD.ACCTPROC.LOADLIB(BANKMAIN)          *
      * COPYLIBS:    FNB.PROD.COPYLIB(CUSTMSTR)                   *
      *              FNB.PROD.COPYLIB(ACCTMSTR)                    *
      *              FNB.PROD.COPYLIB(TXNJRNL)                     *
      *------------------------------------------------------------*
      * CHANGE LOG:                                                 *
      *  04/2017 - R.GUPTA  - ADDED REG-D COUNTER FOR MMA/SAV     *
      *  09/2018 - S.OKONKWO - AML RISK RATING ON CIF             *
      *  01/2020 - J.MURPHY  - DUAL GL POSTING ON TXNJRNL         *
      *  03/2022 - P.ZHANG   - ESCHEAT STATUS CODE                *
      *  11/2024 - K.DAVIES  - UNCOLL FLOAT BAL FIELD             *
      *============================================================*

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CUSTOMER-FILE
               ASSIGN TO "CUSTMSTR.dat"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-CUST-FS.
           SELECT ACCOUNT-FILE
               ASSIGN TO "ACCTMSTR.dat"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-ACCT-FS.
           SELECT TRANSACTION-FILE
               ASSIGN TO "TXNJRNL.dat"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-TXN-FS.
           SELECT REPORT-FILE
               ASSIGN TO "RPTFILE.txt"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-RPT-FS.
           SELECT ACCT-TEMP-FILE
               ASSIGN TO "ACCTTMP.dat"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-ATMP-FS.
           SELECT CUST-TEMP-FILE
               ASSIGN TO "CUSTTMP.dat"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-CTMP-FS.

       DATA DIVISION.
       FILE SECTION.

      *------------------------------------------------------------*
      * CUSTMSTR - CUSTOMER INFORMATION FILE (CIF)                 *
      * RECORD LENGTH: 256 BYTES                                    *
      *------------------------------------------------------------*
       FD CUSTOMER-FILE.
       01 CUST-REC.
           05 CUST-REC-TYPE           PIC X(02).
              88 CUST-REC-VALID                  VALUE "CI".
           05 CUST-CIF-ID             PIC X(10).
           05 CUST-TAX-ID-TYPE        PIC X(01).
              88 CUST-TAX-SSN                    VALUE "S".
              88 CUST-TAX-EIN                    VALUE "E".
              88 CUST-TAX-ITIN                   VALUE "I".
           05 CUST-TAX-ID             PIC X(11).
           05 CUST-NAME-DATA.
              10 CUST-LAST-NAME       PIC X(25).
              10 CUST-FIRST-NAME      PIC X(20).
              10 CUST-MI              PIC X(01).
              10 CUST-SUFFIX          PIC X(04).
           05 CUST-ADDR-DATA.
              10 CUST-ADDR-LINE1      PIC X(30).
              10 CUST-ADDR-LINE2      PIC X(30).
              10 CUST-CITY            PIC X(20).
              10 CUST-STATE           PIC X(02).
              10 CUST-ZIP             PIC X(05).
              10 CUST-ZIP4            PIC X(04).
              10 CUST-COUNTRY         PIC X(03).
           05 CUST-PHONE-PRIMARY      PIC X(12).
           05 CUST-PHONE-ALT         PIC X(12).
           05 CUST-EMAIL             PIC X(40).
           05 CUST-KYC-STATUS        PIC X(01).
              88 CUST-KYC-VERIFIED               VALUE "V".
              88 CUST-KYC-PENDING                VALUE "P".
              88 CUST-KYC-EXPIRED                VALUE "X".
           05 CUST-KYC-DATE          PIC 9(08).
           05 CUST-AML-RISK          PIC X(01).
              88 CUST-AML-LOW                    VALUE "L".
              88 CUST-AML-MED                    VALUE "M".
              88 CUST-AML-HIGH                   VALUE "H".
              88 CUST-AML-PROHIB                 VALUE "P".
           05 CUST-CRA-CODE          PIC X(02).
           05 CUST-REL-OFFICER       PIC X(06).
           05 CUST-SEGMENT           PIC X(02).
              88 CUST-SEG-RETAIL                 VALUE "RT".
              88 CUST-SEG-PRIVATE                VALUE "PB".
              88 CUST-SEG-WEALTH                 VALUE "WM".
              88 CUST-SEG-COMMERCIAL             VALUE "CB".
              88 CUST-SEG-CORPORATE              VALUE "CP".
           05 CUST-STATUS            PIC X(02).
              88 CUST-STAT-ACTIVE                VALUE "AC".
              88 CUST-STAT-INACTIVE              VALUE "IN".
              88 CUST-STAT-DECEASED              VALUE "DC".
           05 CUST-OPEN-DATE         PIC 9(08).
           05 CUST-MAINT-DATE        PIC 9(08).
           05 CUST-MAINT-USER        PIC X(08).
           05 CUST-FILLER            PIC X(04).

      *------------------------------------------------------------*
      * ACCTMSTR - ACCOUNT MASTER FILE                              *
      * RECORD LENGTH: 300 BYTES                                    *
      *------------------------------------------------------------*
       FD ACCOUNT-FILE.
       01 ACCT-REC.
           05 ACCT-REC-TYPE           PIC X(02).
              88 ACCT-REC-VALID                  VALUE "AM".
           05 ACCT-NUM                PIC X(16).
           05 ACCT-CIF-ID            PIC X(10).
           05 ACCT-PROD-CODE         PIC X(04).
              88 ACCT-IS-DDA1                    VALUE "DDA1".
              88 ACCT-IS-DDA2                    VALUE "DDA2".
              88 ACCT-IS-SAV1                    VALUE "SAV1".
              88 ACCT-IS-MMA1                    VALUE "MMA1".
              88 ACCT-IS-CD01                    VALUE "CD01".
              88 ACCT-IS-HEL1                    VALUE "HEL1".
              88 ACCT-IS-MTG1                    VALUE "MTG1".
              88 ACCT-IS-COM1                    VALUE "COM1".
           05 ACCT-PROD-DESC         PIC X(20).
           05 ACCT-STATUS            PIC X(02).
              88 ACCT-ACTIVE                     VALUE "AC".
              88 ACCT-DORMANT                    VALUE "DO".
              88 ACCT-CLOSED                     VALUE "CL".
              88 ACCT-FROZEN                     VALUE "FZ".
              88 ACCT-CHARGED-OFF                VALUE "CO".
              88 ACCT-ESCHEAT                    VALUE "ES".
           05 ACCT-OPEN-DT           PIC 9(08).
           05 ACCT-CLOSE-DT          PIC 9(08).
           05 ACCT-BAL-DATA.
              10 ACCT-LEDGER-BAL     PIC S9(11)V99.
              10 ACCT-AVAIL-BAL      PIC S9(11)V99.
              10 ACCT-HOLD-BAL       PIC S9(11)V99.
              10 ACCT-UNCOLL-BAL     PIC S9(11)V99.
           05 ACCT-OD-LIMIT          PIC S9(09)V99.
           05 ACCT-CREDIT-LIMIT      PIC S9(11)V99.
           05 ACCT-INT-DATA.
              10 ACCT-INT-RATE       PIC S9(03)V9(06).
              10 ACCT-INT-ACCRUED    PIC S9(09)V99.
              10 ACCT-INT-YTD        PIC S9(09)V99.
              10 ACCT-INT-PRIOR-YR   PIC S9(09)V99.
              10 ACCT-INT-LAST-CALC  PIC 9(08).
           05 ACCT-MATURITY-DT       PIC 9(08).
           05 ACCT-COST-CENTER       PIC X(06).
           05 ACCT-GL-ACCT           PIC X(10).
           05 ACCT-OFFICER           PIC X(06).
           05 ACCT-CURRENCY          PIC X(03).
           05 ACCT-STMT-CYCLE        PIC X(02).
           05 ACCT-STMT-LAST-DT      PIC 9(08).
           05 ACCT-RESTRICT-CD       PIC X(02).
              88 ACCT-NO-RESTRICT                VALUE "  ".
              88 ACCT-NO-DEBIT                   VALUE "ND".
              88 ACCT-NO-CREDIT                  VALUE "NC".
              88 ACCT-LEGAL-HOLD                 VALUE "LH".
              88 ACCT-GARNISHMENT                VALUE "GR".
           05 ACCT-REGD-CTR          PIC 9(02).
           05 ACCT-TAX-RPTG          PIC X(01).
              88 ACCT-TAX-YES                    VALUE "Y".
              88 ACCT-TAX-NO                     VALUE "N".
              88 ACCT-TAX-EXEMPT                 VALUE "E".
           05 ACCT-BRANCH            PIC X(06).
           05 ACCT-LAST-ACTIVITY-DT  PIC 9(08).
           05 ACCT-MAINT-DT          PIC 9(08).
           05 ACCT-MAINT-USER        PIC X(08).
           05 ACCT-FILLER            PIC X(20).

      *------------------------------------------------------------*
      * TXNJRNL - TRANSACTION JOURNAL                               *
      * RECORD LENGTH: 256 BYTES                                    *
      *------------------------------------------------------------*
       FD TRANSACTION-FILE.
       01 TXN-REC.
           05 TXN-REC-TYPE           PIC X(02).
              88 TXN-REC-VALID                   VALUE "TJ".
           05 TXN-SEQ-NUM            PIC X(12).
           05 TXN-ACCT-NUM           PIC X(16).
           05 TXN-CIF-ID             PIC X(10).
           05 TXN-EFFDT              PIC 9(08).
           05 TXN-POST-DT            PIC 9(08).
           05 TXN-POST-TIME          PIC 9(06).
           05 TXN-TYPE-CD            PIC X(03).
              88 TXN-IS-DEP                      VALUE "DEP".
              88 TXN-IS-WDL                      VALUE "WDL".
              88 TXN-IS-XDR                      VALUE "XDR".
              88 TXN-IS-XCR                      VALUE "XCR".
              88 TXN-IS-SVC                      VALUE "SVC".
              88 TXN-IS-INT                      VALUE "INT".
              88 TXN-IS-ICH                      VALUE "ICH".
              88 TXN-IS-ODF                      VALUE "ODF".
              88 TXN-IS-WFE                      VALUE "WFE".
              88 TXN-IS-ACR                      VALUE "ACR".
              88 TXN-IS-ADR                      VALUE "ADR".
              88 TXN-IS-CKD                      VALUE "CKD".
              88 TXN-IS-CKP                      VALUE "CKP".
           05 TXN-DR-CR             PIC X(01).
              88 TXN-DEBIT                       VALUE "D".
              88 TXN-CREDIT                      VALUE "C".
           05 TXN-AMT               PIC S9(11)V99.
           05 TXN-RUN-BAL           PIC S9(11)V99.
           05 TXN-CHANNEL           PIC X(03).
              88 TXN-CHAN-BRANCH                  VALUE "BRN".
              88 TXN-CHAN-ATM                     VALUE "ATM".
              88 TXN-CHAN-ONLINE                  VALUE "ONL".
              88 TXN-CHAN-MOBILE                  VALUE "MOB".
              88 TXN-CHAN-WIRE                    VALUE "WIR".
              88 TXN-CHAN-ACH                     VALUE "ACH".
              88 TXN-CHAN-BATCH                   VALUE "BAT".
              88 TXN-CHAN-INTERNAL                VALUE "INT".
           05 TXN-TELLER-ID         PIC X(06).
           05 TXN-TERMINAL-ID        PIC X(08).
           05 TXN-AUTH-CODE          PIC X(06).
           05 TXN-REF-NUM           PIC X(20).
           05 TXN-DESC              PIC X(40).
           05 TXN-BATCH-NUM         PIC X(08).
           05 TXN-CYCLE-NUM         PIC X(04).
           05 TXN-GL-DR-ACCT        PIC X(10).
           05 TXN-GL-CR-ACCT        PIC X(10).
           05 TXN-REV-FLAG          PIC X(01).
              88 TXN-NOT-REVERSED                VALUE " ".
              88 TXN-IS-REVERSAL                 VALUE "R".
              88 TXN-HAS-REVERSAL                VALUE "V".
           05 TXN-REV-ORIG-SEQ      PIC X(12).
           05 TXN-STATUS            PIC X(01).
              88 TXN-POSTED                      VALUE "P".
              88 TXN-HOLD                        VALUE "H".
              88 TXN-REVERSED                    VALUE "R".
              88 TXN-REJECTED                    VALUE "X".
           05 TXN-FILLER            PIC X(13).

       FD REPORT-FILE.
       01 RPT-LINE                    PIC X(132).

       FD ACCT-TEMP-FILE.
       01 ATMP-REC                    PIC X(300).

       FD CUST-TEMP-FILE.
       01 CTMP-REC                    PIC X(256).

       WORKING-STORAGE SECTION.

       01 WS-FILE-STATUSES.
           05 WS-CUST-FS             PIC XX.
           05 WS-ACCT-FS             PIC XX.
           05 WS-TXN-FS              PIC XX.
           05 WS-RPT-FS              PIC XX.
           05 WS-ATMP-FS             PIC XX.
           05 WS-CTMP-FS             PIC XX.

       01 WS-FLAGS.
           05 WS-EOF                  PIC X(01) VALUE "N".
              88 EOF-YES                         VALUE "Y".
              88 EOF-NO                          VALUE "N".
           05 WS-FOUND                PIC X(01) VALUE "N".
              88 FOUND-YES                       VALUE "Y".
              88 FOUND-NO                        VALUE "N".
           05 WS-VALID                PIC X(01) VALUE "N".
              88 VALID-YES                       VALUE "Y".
              88 VALID-NO                        VALUE "N".

       01 WS-MENU-CHOICE             PIC 9(01) VALUE 9.
       01 WS-SUB-CHOICE              PIC 9(01).

       01 WS-SEARCH-ACCT             PIC X(16).
       01 WS-SEARCH-CIF              PIC X(10).
       01 WS-INPUT-AMT               PIC S9(11)V99.
       01 WS-INPUT-NAME              PIC X(25).
       01 WS-INPUT-FNAME             PIC X(20).
       01 WS-INPUT-PROD              PIC X(04).
       01 WS-INPUT-BRANCH            PIC X(06).
       01 WS-XFER-TARGET             PIC X(16).

       01 WS-CURR-DT-DATA.
           05 WS-CURR-DATE           PIC 9(08).
           05 WS-CURR-TIME           PIC 9(08).
           05 WS-GMT-DIFF            PIC S9(04).

       01 WS-DISP-DATE.
           05 WS-DD-YEAR             PIC 9(04).
           05 FILLER                  PIC X(01) VALUE "/".
           05 WS-DD-MONTH            PIC 9(02).
           05 FILLER                  PIC X(01) VALUE "/".
           05 WS-DD-DAY              PIC 9(02).

       01 WS-COUNTERS.
           05 WS-ACCT-CNT            PIC 9(05) VALUE 0.
           05 WS-TXN-CNT             PIC 9(07) VALUE 0.
           05 WS-TOT-DEP             PIC S9(13)V99 VALUE 0.
           05 WS-TOT-WDR             PIC S9(13)V99 VALUE 0.
           05 WS-TOT-BAL             PIC S9(13)V99 VALUE 0.
           05 WS-RPT-LINES           PIC 9(03) VALUE 0.
           05 WS-RPT-PAGES           PIC 9(03) VALUE 0.

       01 WS-NEXT-ACCT-SEQ           PIC 9(10) VALUE 0.
       01 WS-NEXT-TXN-SEQ            PIC 9(12) VALUE 0.
       01 WS-NEXT-CIF-SEQ            PIC 9(10) VALUE 0.

       01 WS-GEN-ACCT-NUM            PIC X(16).
       01 WS-GEN-CIF-ID              PIC X(10).
       01 WS-GEN-TXN-SEQ             PIC X(12).

       01 WS-FMT-BAL                PIC $$$$,$$$,$$$,$$9.99-.
       01 WS-FMT-AMT                PIC $$$$,$$$,$$$,$$9.99-.
       01 WS-FMT-RATE               PIC Z9.9(06).

       01 WS-FOUND-CIF-ID           PIC X(10).
       01 WS-FOUND-LEDGER           PIC S9(11)V99.
       01 WS-SAVE-EOF               PIC X(01).

      *------------------------------------------------------------*
      * REPORT LAYOUTS                                              *
      *------------------------------------------------------------*
       01 WS-RPT-SEP.
           05 FILLER PIC X(132) VALUE ALL "=".

       01 WS-RPT-TITLE.
           05 FILLER PIC X(04) VALUE "  ".
           05 FILLER PIC X(60) VALUE
              "FIRST NATIONAL BANK - ACCOUNT SUMMARY REPORT".
           05 FILLER PIC X(50) VALUE SPACES.
           05 FILLER PIC X(06) VALUE "PAGE: ".
           05 WS-RT-PG PIC Z(03)9.

       01 WS-RPT-DATE-LN.
           05 FILLER PIC X(16) VALUE "  REPORT DATE: ".
           05 WS-RT-DT PIC X(10).

       01 WS-RPT-COL-HDR.
           05 FILLER PIC X(18) VALUE "  ACCOUNT         ".
           05 FILLER PIC X(27) VALUE
              "ACCOUNT HOLDER             ".
           05 FILLER PIC X(06) VALUE "PROD  ".
           05 FILLER PIC X(04) VALUE "ST  ".
           05 FILLER PIC X(18) VALUE "    LEDGER BAL    ".
           05 FILLER PIC X(18) VALUE "    AVAIL BAL     ".
           05 FILLER PIC X(12) VALUE "  RATE      ".
           05 FILLER PIC X(08) VALUE "BRANCH  ".
           05 FILLER PIC X(04) VALUE "REST".

       01 WS-RPT-DTL.
           05 FILLER                  PIC X(02) VALUE "  ".
           05 WS-RD-ACCT             PIC X(16).
           05 FILLER                  PIC X(02) VALUE "  ".
           05 WS-RD-NAME             PIC X(25).
           05 FILLER                  PIC X(02) VALUE "  ".
           05 WS-RD-PROD             PIC X(04).
           05 FILLER                  PIC X(02) VALUE "  ".
           05 WS-RD-STAT             PIC X(02).
           05 FILLER                  PIC X(02) VALUE "  ".
           05 WS-RD-LEDGER           PIC $$$$,$$$,$$9.99-.
           05 FILLER                  PIC X(02) VALUE "  ".
           05 WS-RD-AVAIL            PIC $$$$,$$$,$$9.99-.
           05 FILLER                  PIC X(02) VALUE "  ".
           05 WS-RD-RATE             PIC Z9.9(06).
           05 FILLER                  PIC X(02) VALUE "  ".
           05 WS-RD-BRNCH            PIC X(06).
           05 FILLER                  PIC X(02) VALUE "  ".
           05 WS-RD-RESTRICT         PIC X(02).

       01 WS-RPT-TOT-LN1.
           05 FILLER PIC X(40) VALUE SPACES.
           05 FILLER PIC X(20) VALUE "TOTAL ACCOUNTS:     ".
           05 WS-RT-TOT-ACCT PIC Z(04)9.

       01 WS-RPT-TOT-LN2.
           05 FILLER PIC X(40) VALUE SPACES.
           05 FILLER PIC X(20) VALUE "TOTAL LEDGER BAL:   ".
           05 WS-RT-TOT-BAL PIC $$$$,$$$,$$$,$$9.99-.

       01 WS-TXN-COL-HDR.
           05 FILLER PIC X(14) VALUE "  SEQ NUM     ".
           05 FILLER PIC X(18) VALUE "ACCOUNT           ".
           05 FILLER PIC X(05) VALUE "TYP  ".
           05 FILLER PIC X(04) VALUE "D/C ".
           05 FILLER PIC X(18) VALUE "         AMOUNT   ".
           05 FILLER PIC X(12) VALUE "EFF DATE    ".
           05 FILLER PIC X(05) VALUE "CHAN ".
           05 FILLER PIC X(42) VALUE
              "DESCRIPTION                               ".
           05 FILLER PIC X(04) VALUE "STAT".

       01 WS-TXN-DTL.
           05 FILLER                  PIC X(02) VALUE "  ".
           05 WS-TD-SEQ              PIC X(12).
           05 FILLER                  PIC X(02) VALUE "  ".
           05 WS-TD-ACCT             PIC X(16).
           05 FILLER                  PIC X(02) VALUE "  ".
           05 WS-TD-TYPE             PIC X(03).
           05 FILLER                  PIC X(02) VALUE "  ".
           05 WS-TD-DRCR             PIC X(01).
           05 FILLER                  PIC X(02) VALUE "  ".
           05 WS-TD-AMT              PIC $$$$,$$$,$$9.99-.
           05 FILLER                  PIC X(02) VALUE "  ".
           05 WS-TD-EFFDT            PIC X(10).
           05 FILLER                  PIC X(02) VALUE "  ".
           05 WS-TD-CHAN             PIC X(03).
           05 FILLER                  PIC X(02) VALUE "  ".
           05 WS-TD-DESC             PIC X(40).
           05 FILLER                  PIC X(02) VALUE "  ".
           05 WS-TD-STAT             PIC X(01).

       PROCEDURE DIVISION.

       0000-MAIN.
           PERFORM 1000-INIT
           PERFORM 2000-MENU UNTIL WS-MENU-CHOICE = 0
           PERFORM 9000-TERM
           STOP RUN
           .

       1000-INIT.
           MOVE FUNCTION CURRENT-DATE TO WS-CURR-DT-DATA
           DISPLAY "============================================="
           DISPLAY "  FIRST NATIONAL BANK"
           DISPLAY "  CORE BANKING SYSTEM"
           DISPLAY "  BATCH/ONLINE PROCESSING ENGINE v4.2.1"
           DISPLAY "============================================="
           DISPLAY " "
           PERFORM 1100-LOAD-NEXT-IDS
           .

       1100-LOAD-NEXT-IDS.
           OPEN INPUT CUSTOMER-FILE
           IF WS-CUST-FS NOT = "00"
               MOVE 1 TO WS-NEXT-CIF-SEQ
           ELSE
               SET EOF-NO TO TRUE
               PERFORM UNTIL EOF-YES
                   READ CUSTOMER-FILE
                       AT END SET EOF-YES TO TRUE
                       NOT AT END
                           IF CUST-CIF-ID > WS-NEXT-CIF-SEQ
                               MOVE CUST-CIF-ID TO WS-NEXT-CIF-SEQ
                           END-IF
                   END-READ
               END-PERFORM
               ADD 1 TO WS-NEXT-CIF-SEQ
           END-IF
           CLOSE CUSTOMER-FILE

           OPEN INPUT ACCOUNT-FILE
           IF WS-ACCT-FS NOT = "00"
               MOVE 1 TO WS-NEXT-ACCT-SEQ
           ELSE
               SET EOF-NO TO TRUE
               PERFORM UNTIL EOF-YES
                   READ ACCOUNT-FILE
                       AT END SET EOF-YES TO TRUE
                       NOT AT END
                           CONTINUE
                   END-READ
               END-PERFORM
               ADD 1 TO WS-NEXT-ACCT-SEQ
           END-IF
           CLOSE ACCOUNT-FILE

           OPEN INPUT TRANSACTION-FILE
           IF WS-TXN-FS NOT = "00"
               MOVE 1 TO WS-NEXT-TXN-SEQ
           ELSE
               SET EOF-NO TO TRUE
               PERFORM UNTIL EOF-YES
                   READ TRANSACTION-FILE
                       AT END SET EOF-YES TO TRUE
                       NOT AT END
                           IF TXN-SEQ-NUM > WS-NEXT-TXN-SEQ
                               MOVE TXN-SEQ-NUM
                                   TO WS-NEXT-TXN-SEQ
                           END-IF
                   END-READ
               END-PERFORM
               ADD 1 TO WS-NEXT-TXN-SEQ
           END-IF
           CLOSE TRANSACTION-FILE
           .

       2000-MENU.
           DISPLAY " "
           DISPLAY "============================================="
           DISPLAY "          MAIN MENU"
           DISPLAY "============================================="
           DISPLAY "  1. Account Management"
           DISPLAY "  2. Transaction Processing"
           DISPLAY "  3. Generate Reports"
           DISPLAY "  4. Account Inquiry"
           DISPLAY "  0. Exit System"
           DISPLAY "============================================="
           DISPLAY "  Enter choice: " WITH NO ADVANCING
           ACCEPT WS-MENU-CHOICE

           EVALUATE WS-MENU-CHOICE
               WHEN 1 PERFORM 3000-ACCT-MGMT
               WHEN 2 PERFORM 4000-TXN-PROC
               WHEN 3 PERFORM 5000-REPORTS
               WHEN 4 PERFORM 6000-INQUIRY
               WHEN 0 DISPLAY "  Exiting system..."
               WHEN OTHER DISPLAY "  *** INVALID SELECTION ***"
           END-EVALUATE
           .

       3000-ACCT-MGMT.
           DISPLAY " "
           DISPLAY "---------------------------------------------"
           DISPLAY "       ACCOUNT MANAGEMENT"
           DISPLAY "---------------------------------------------"
           DISPLAY "  1. Open New Account"
           DISPLAY "  2. Close Account"
           DISPLAY "  3. Update Account Holder Name"
           DISPLAY "  4. List All Accounts"
           DISPLAY "  0. Return to Main Menu"
           DISPLAY "---------------------------------------------"
           DISPLAY "  Enter choice: " WITH NO ADVANCING
           ACCEPT WS-SUB-CHOICE

           EVALUATE WS-SUB-CHOICE
               WHEN 1 PERFORM 3100-OPEN-ACCT
               WHEN 2 PERFORM 3200-CLOSE-ACCT
               WHEN 3 PERFORM 3300-UPDATE-ACCT
               WHEN 4 PERFORM 3400-LIST-ACCTS
               WHEN 0 CONTINUE
               WHEN OTHER DISPLAY "  *** INVALID SELECTION ***"
           END-EVALUATE
           .

       3100-OPEN-ACCT.
           DISPLAY " "
           DISPLAY "  --- OPEN NEW ACCOUNT ---"
           DISPLAY "  Last name:  " WITH NO ADVANCING
           ACCEPT WS-INPUT-NAME
           DISPLAY "  First name: " WITH NO ADVANCING
           ACCEPT WS-INPUT-FNAME
           DISPLAY "  Product (DDA1/SAV1/MMA1/CD01/HEL1/MTG1):"
               WITH NO ADVANCING
           DISPLAY " " WITH NO ADVANCING
           ACCEPT WS-INPUT-PROD
           DISPLAY "  Initial deposit: " WITH NO ADVANCING
           ACCEPT WS-INPUT-AMT
           DISPLAY "  Branch code: " WITH NO ADVANCING
           ACCEPT WS-INPUT-BRANCH

           INSPECT WS-INPUT-PROD
               CONVERTING
                  "abcdefghijklmnopqrstuvwxyz"
               TO "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

           IF WS-INPUT-PROD NOT = "DDA1"
              AND WS-INPUT-PROD NOT = "DDA2"
              AND WS-INPUT-PROD NOT = "SAV1"
              AND WS-INPUT-PROD NOT = "MMA1"
              AND WS-INPUT-PROD NOT = "CD01"
              AND WS-INPUT-PROD NOT = "HEL1"
              AND WS-INPUT-PROD NOT = "MTG1"
              AND WS-INPUT-PROD NOT = "COM1"
               DISPLAY "  *** ERROR: Invalid product code ***"
           ELSE
           IF WS-INPUT-AMT < 0
               DISPLAY "  *** ERROR: Invalid deposit amount ***"
           ELSE
               PERFORM 3110-CREATE-CIF
               PERFORM 3120-CREATE-ACCT
               IF WS-INPUT-AMT > 0
                   PERFORM 3130-LOG-OPEN-TXN
               END-IF
           END-IF
           END-IF
           .

       3110-CREATE-CIF.
           OPEN EXTEND CUSTOMER-FILE
           IF WS-CUST-FS NOT = "00"
               OPEN OUTPUT CUSTOMER-FILE
           END-IF
           MOVE WS-NEXT-CIF-SEQ TO WS-GEN-CIF-ID
           INITIALIZE CUST-REC
           MOVE "CI"              TO CUST-REC-TYPE
           MOVE WS-GEN-CIF-ID    TO CUST-CIF-ID
           MOVE "S"               TO CUST-TAX-ID-TYPE
           MOVE "***-**-0000"     TO CUST-TAX-ID
           MOVE WS-INPUT-NAME     TO CUST-LAST-NAME
           MOVE WS-INPUT-FNAME    TO CUST-FIRST-NAME
           MOVE SPACES            TO CUST-MI
           MOVE SPACES            TO CUST-SUFFIX
           MOVE "NEW ACCOUNT HOLDER" TO CUST-ADDR-LINE1
           MOVE SPACES            TO CUST-ADDR-LINE2
           MOVE "NEW YORK"        TO CUST-CITY
           MOVE "NY"              TO CUST-STATE
           MOVE "10001"           TO CUST-ZIP
           MOVE "0001"            TO CUST-ZIP4
           MOVE "USA"             TO CUST-COUNTRY
           MOVE SPACES            TO CUST-PHONE-PRIMARY
           MOVE SPACES            TO CUST-PHONE-ALT
           MOVE SPACES            TO CUST-EMAIL
           MOVE "P"               TO CUST-KYC-STATUS
           MOVE WS-CURR-DATE      TO CUST-KYC-DATE
           MOVE "L"               TO CUST-AML-RISK
           MOVE "00"              TO CUST-CRA-CODE
           MOVE "OFC001"          TO CUST-REL-OFFICER
           MOVE "RT"              TO CUST-SEGMENT
           MOVE "AC"              TO CUST-STATUS
           MOVE WS-CURR-DATE      TO CUST-OPEN-DATE
           MOVE WS-CURR-DATE      TO CUST-MAINT-DATE
           MOVE "SYSTEM  "        TO CUST-MAINT-USER
           MOVE SPACES            TO CUST-FILLER
           WRITE CUST-REC
           CLOSE CUSTOMER-FILE
           ADD 1 TO WS-NEXT-CIF-SEQ
           .

       3120-CREATE-ACCT.
           OPEN EXTEND ACCOUNT-FILE
           IF WS-ACCT-FS NOT = "00"
               OPEN OUTPUT ACCOUNT-FILE
           END-IF
           MOVE WS-NEXT-ACCT-SEQ TO WS-GEN-ACCT-NUM
           INITIALIZE ACCT-REC
           MOVE "AM"              TO ACCT-REC-TYPE
           MOVE WS-GEN-ACCT-NUM  TO ACCT-NUM
           MOVE WS-GEN-CIF-ID    TO ACCT-CIF-ID
           MOVE WS-INPUT-PROD    TO ACCT-PROD-CODE
           EVALUATE WS-INPUT-PROD
               WHEN "DDA1" MOVE "CHECKING STANDARD   "
                               TO ACCT-PROD-DESC
               WHEN "DDA2" MOVE "CHECKING PREMIUM    "
                               TO ACCT-PROD-DESC
               WHEN "SAV1" MOVE "SAVINGS STANDARD    "
                               TO ACCT-PROD-DESC
               WHEN "MMA1" MOVE "MONEY MARKET ACCT   "
                               TO ACCT-PROD-DESC
               WHEN "CD01" MOVE "CERT OF DEPOSIT 12M "
                               TO ACCT-PROD-DESC
               WHEN "HEL1" MOVE "HOME EQUITY LINE    "
                               TO ACCT-PROD-DESC
               WHEN "MTG1" MOVE "MORTGAGE 30YR FIXED "
                               TO ACCT-PROD-DESC
               WHEN "COM1" MOVE "COMMERCIAL DDA      "
                               TO ACCT-PROD-DESC
           END-EVALUATE
           MOVE "AC"              TO ACCT-STATUS
           MOVE WS-CURR-DATE      TO ACCT-OPEN-DT
           MOVE 0                 TO ACCT-CLOSE-DT
           MOVE WS-INPUT-AMT      TO ACCT-LEDGER-BAL
           MOVE WS-INPUT-AMT      TO ACCT-AVAIL-BAL
           MOVE 0                 TO ACCT-HOLD-BAL
           MOVE 0                 TO ACCT-UNCOLL-BAL
           MOVE 0                 TO ACCT-OD-LIMIT
           MOVE 0                 TO ACCT-CREDIT-LIMIT
           MOVE 0                 TO ACCT-INT-RATE
           MOVE 0                 TO ACCT-INT-ACCRUED
           MOVE 0                 TO ACCT-INT-YTD
           MOVE 0                 TO ACCT-INT-PRIOR-YR
           MOVE 0                 TO ACCT-INT-LAST-CALC
           MOVE 0                 TO ACCT-MATURITY-DT
           MOVE "CC0001"          TO ACCT-COST-CENTER
           MOVE "1001000100"      TO ACCT-GL-ACCT
           MOVE "OFC001"          TO ACCT-OFFICER
           MOVE "USD"             TO ACCT-CURRENCY
           MOVE "MO"              TO ACCT-STMT-CYCLE
           MOVE 0                 TO ACCT-STMT-LAST-DT
           MOVE "  "              TO ACCT-RESTRICT-CD
           MOVE 0                 TO ACCT-REGD-CTR
           MOVE "Y"               TO ACCT-TAX-RPTG
           MOVE WS-INPUT-BRANCH   TO ACCT-BRANCH
           MOVE WS-CURR-DATE      TO ACCT-LAST-ACTIVITY-DT
           MOVE WS-CURR-DATE      TO ACCT-MAINT-DT
           MOVE "SYSTEM  "        TO ACCT-MAINT-USER
           MOVE SPACES            TO ACCT-FILLER
           WRITE ACCT-REC
           CLOSE ACCOUNT-FILE

           MOVE WS-INPUT-AMT TO WS-FMT-AMT
           DISPLAY " "
           DISPLAY "  *** ACCOUNT OPENED SUCCESSFULLY ***"
           DISPLAY "  Account Number: " WS-GEN-ACCT-NUM
           DISPLAY "  CIF ID:         " WS-GEN-CIF-ID
           DISPLAY "  Holder:         " WS-INPUT-NAME
           DISPLAY "  Product:        " WS-INPUT-PROD
           DISPLAY "  Ledger Bal:     " WS-FMT-AMT
           DISPLAY "  Branch:         " WS-INPUT-BRANCH

           ADD 1 TO WS-NEXT-ACCT-SEQ
           .

       3130-LOG-OPEN-TXN.
           OPEN EXTEND TRANSACTION-FILE
           IF WS-TXN-FS NOT = "00"
               OPEN OUTPUT TRANSACTION-FILE
           END-IF
           MOVE WS-NEXT-TXN-SEQ   TO WS-GEN-TXN-SEQ
           INITIALIZE TXN-REC
           MOVE "TJ"              TO TXN-REC-TYPE
           MOVE WS-GEN-TXN-SEQ   TO TXN-SEQ-NUM
           MOVE WS-GEN-ACCT-NUM  TO TXN-ACCT-NUM
           MOVE WS-GEN-CIF-ID    TO TXN-CIF-ID
           MOVE WS-CURR-DATE      TO TXN-EFFDT
           MOVE WS-CURR-DATE      TO TXN-POST-DT
           MOVE WS-CURR-TIME(1:6) TO TXN-POST-TIME
           MOVE "DEP"             TO TXN-TYPE-CD
           MOVE "C"               TO TXN-DR-CR
           MOVE WS-INPUT-AMT      TO TXN-AMT
           MOVE WS-INPUT-AMT      TO TXN-RUN-BAL
           MOVE "BRN"             TO TXN-CHANNEL
           MOVE "TLR001"          TO TXN-TELLER-ID
           MOVE "TERM0001"        TO TXN-TERMINAL-ID
           MOVE SPACES            TO TXN-AUTH-CODE
           MOVE SPACES            TO TXN-REF-NUM
           MOVE "OPENING DEPOSIT"
                                  TO TXN-DESC
           MOVE "00000001"        TO TXN-BATCH-NUM
           MOVE "0001"            TO TXN-CYCLE-NUM
           MOVE "1001000100"      TO TXN-GL-DR-ACCT
           MOVE "2001000100"      TO TXN-GL-CR-ACCT
           MOVE " "               TO TXN-REV-FLAG
           MOVE SPACES            TO TXN-REV-ORIG-SEQ
           MOVE "P"               TO TXN-STATUS
           MOVE SPACES            TO TXN-FILLER
           WRITE TXN-REC
           CLOSE TRANSACTION-FILE
           ADD 1 TO WS-NEXT-TXN-SEQ
           .

       3200-CLOSE-ACCT.
           DISPLAY " "
           DISPLAY "  --- CLOSE ACCOUNT ---"
           DISPLAY "  Account number: " WITH NO ADVANCING
           ACCEPT WS-SEARCH-ACCT

           SET FOUND-NO TO TRUE
           OPEN INPUT ACCOUNT-FILE
           OPEN OUTPUT ACCT-TEMP-FILE
           SET EOF-NO TO TRUE

           PERFORM UNTIL EOF-YES
               READ ACCOUNT-FILE
                   AT END SET EOF-YES TO TRUE
                   NOT AT END
                       IF ACCT-NUM = WS-SEARCH-ACCT
                           SET FOUND-YES TO TRUE
                           IF ACCT-ACTIVE
                               MOVE "CL" TO ACCT-STATUS
                               MOVE WS-CURR-DATE
                                   TO ACCT-CLOSE-DT
                               MOVE WS-CURR-DATE
                                   TO ACCT-MAINT-DT
                               MOVE "SYSTEM  "
                                   TO ACCT-MAINT-USER
                               MOVE ACCT-LEDGER-BAL
                                   TO WS-FMT-BAL
                               DISPLAY "  Account closed"
                               DISPLAY "  Final balance: "
                                   WS-FMT-BAL
                           ELSE
                               DISPLAY
                                 "  *** Account not active ***"
                           END-IF
                       END-IF
                       MOVE ACCT-REC TO ATMP-REC
                       WRITE ATMP-REC
               END-READ
           END-PERFORM
           CLOSE ACCOUNT-FILE
           CLOSE ACCT-TEMP-FILE

           IF FOUND-NO
               DISPLAY "  *** ACCOUNT NOT FOUND ***"
           ELSE
               PERFORM 8000-SWAP-ACCT-FILE
           END-IF
           .

       3300-UPDATE-ACCT.
           DISPLAY " "
           DISPLAY "  --- UPDATE ACCOUNT HOLDER ---"
           DISPLAY "  Account number: " WITH NO ADVANCING
           ACCEPT WS-SEARCH-ACCT
           DISPLAY "  New last name: " WITH NO ADVANCING
           ACCEPT WS-INPUT-NAME

           SET FOUND-NO TO TRUE
           MOVE SPACES TO WS-FOUND-CIF-ID

           OPEN INPUT ACCOUNT-FILE
           OPEN OUTPUT ACCT-TEMP-FILE
           SET EOF-NO TO TRUE

           PERFORM UNTIL EOF-YES
               READ ACCOUNT-FILE
                   AT END SET EOF-YES TO TRUE
                   NOT AT END
                       IF ACCT-NUM = WS-SEARCH-ACCT
                           SET FOUND-YES TO TRUE
                           MOVE ACCT-CIF-ID
                               TO WS-FOUND-CIF-ID
                       END-IF
                       MOVE ACCT-REC TO ATMP-REC
                       WRITE ATMP-REC
               END-READ
           END-PERFORM
           CLOSE ACCOUNT-FILE
           CLOSE ACCT-TEMP-FILE
           PERFORM 8000-SWAP-ACCT-FILE

           IF FOUND-NO
               DISPLAY "  *** ACCOUNT NOT FOUND ***"
           ELSE
               PERFORM 3310-UPDATE-CIF-NAME
           END-IF
           .

       3310-UPDATE-CIF-NAME.
           SET FOUND-NO TO TRUE
           OPEN INPUT CUSTOMER-FILE
           OPEN OUTPUT CUST-TEMP-FILE
           SET EOF-NO TO TRUE

           PERFORM UNTIL EOF-YES
               READ CUSTOMER-FILE
                   AT END SET EOF-YES TO TRUE
                   NOT AT END
                       IF CUST-CIF-ID = WS-FOUND-CIF-ID
                           SET FOUND-YES TO TRUE
                           MOVE WS-INPUT-NAME
                               TO CUST-LAST-NAME
                           MOVE WS-CURR-DATE
                               TO CUST-MAINT-DATE
                           MOVE "SYSTEM  "
                               TO CUST-MAINT-USER
                           DISPLAY "  CIF updated: "
                               WS-FOUND-CIF-ID
                       END-IF
                       MOVE CUST-REC TO CTMP-REC
                       WRITE CTMP-REC
               END-READ
           END-PERFORM
           CLOSE CUSTOMER-FILE
           CLOSE CUST-TEMP-FILE

           IF FOUND-YES
               PERFORM 8100-SWAP-CUST-FILE
               DISPLAY "  Account updated"
           END-IF
           .

       3400-LIST-ACCTS.
           DISPLAY " "
           DISPLAY "  --- ALL ACCOUNTS ---"
           DISPLAY WS-RPT-COL-HDR
           DISPLAY WS-RPT-SEP

           OPEN INPUT ACCOUNT-FILE
           IF WS-ACCT-FS NOT = "00"
               DISPLAY "  (no accounts on file)"
               CLOSE ACCOUNT-FILE
           ELSE
               MOVE 0 TO WS-ACCT-CNT
               MOVE 0 TO WS-TOT-BAL
               SET EOF-NO TO TRUE
               PERFORM UNTIL EOF-YES
                   READ ACCOUNT-FILE
                       AT END SET EOF-YES TO TRUE
                       NOT AT END
                           ADD 1 TO WS-ACCT-CNT
                           ADD ACCT-LEDGER-BAL TO WS-TOT-BAL
                           PERFORM 3410-SHOW-ACCT-LINE
                   END-READ
               END-PERFORM
               CLOSE ACCOUNT-FILE
               DISPLAY WS-RPT-SEP
               DISPLAY "  Total accounts: " WS-ACCT-CNT
               MOVE WS-TOT-BAL TO WS-FMT-BAL
               DISPLAY "  Total ledger:   " WS-FMT-BAL
           END-IF
           .

       3410-SHOW-ACCT-LINE.
           MOVE SPACES TO WS-RPT-DTL
           MOVE ACCT-NUM        TO WS-RD-ACCT
           PERFORM 3411-LOOKUP-HOLDER-NAME
           MOVE ACCT-PROD-CODE  TO WS-RD-PROD
           MOVE ACCT-STATUS     TO WS-RD-STAT
           MOVE ACCT-LEDGER-BAL TO WS-RD-LEDGER
           MOVE ACCT-AVAIL-BAL  TO WS-RD-AVAIL
           MOVE ACCT-INT-RATE   TO WS-RD-RATE
           MOVE ACCT-BRANCH     TO WS-RD-BRNCH
           MOVE ACCT-RESTRICT-CD TO WS-RD-RESTRICT
           DISPLAY WS-RPT-DTL
           .

       3411-LOOKUP-HOLDER-NAME.
           MOVE WS-EOF TO WS-SAVE-EOF
           OPEN INPUT CUSTOMER-FILE
           IF WS-CUST-FS NOT = "00"
               MOVE "(UNKNOWN)"  TO WS-RD-NAME
               CLOSE CUSTOMER-FILE
           ELSE
               MOVE "(UNKNOWN)" TO WS-RD-NAME
               SET EOF-NO TO TRUE
               PERFORM UNTIL EOF-YES
                   READ CUSTOMER-FILE
                       AT END SET EOF-YES TO TRUE
                       NOT AT END
                           IF CUST-CIF-ID = ACCT-CIF-ID
                               MOVE CUST-LAST-NAME
                                   TO WS-RD-NAME
                           END-IF
                   END-READ
               END-PERFORM
               CLOSE CUSTOMER-FILE
           END-IF
           MOVE WS-SAVE-EOF TO WS-EOF
           .

       4000-TXN-PROC.
           DISPLAY " "
           DISPLAY "---------------------------------------------"
           DISPLAY "       TRANSACTION PROCESSING"
           DISPLAY "---------------------------------------------"
           DISPLAY "  1. Deposit"
           DISPLAY "  2. Withdrawal"
           DISPLAY "  3. Transfer Between Accounts"
           DISPLAY "  4. Apply Monthly Service Fees"
           DISPLAY "  0. Return to Main Menu"
           DISPLAY "---------------------------------------------"
           DISPLAY "  Enter choice: " WITH NO ADVANCING
           ACCEPT WS-SUB-CHOICE

           EVALUATE WS-SUB-CHOICE
               WHEN 1 PERFORM 4100-DEPOSIT
               WHEN 2 PERFORM 4200-WITHDRAW
               WHEN 3 PERFORM 4300-TRANSFER
               WHEN 4 PERFORM 4400-BATCH-FEES
               WHEN 0 CONTINUE
               WHEN OTHER DISPLAY "  *** INVALID SELECTION ***"
           END-EVALUATE
           .

       4100-DEPOSIT.
           DISPLAY " "
           DISPLAY "  --- DEPOSIT ---"
           DISPLAY "  Account number: " WITH NO ADVANCING
           ACCEPT WS-SEARCH-ACCT
           DISPLAY "  Deposit amount: " WITH NO ADVANCING
           ACCEPT WS-INPUT-AMT

           IF WS-INPUT-AMT <= 0
               DISPLAY "  *** Amount must be positive ***"
           ELSE
               SET FOUND-NO TO TRUE
               MOVE SPACES TO WS-FOUND-CIF-ID
               OPEN INPUT ACCOUNT-FILE
               OPEN OUTPUT ACCT-TEMP-FILE
               SET EOF-NO TO TRUE

               PERFORM UNTIL EOF-YES
                   READ ACCOUNT-FILE
                       AT END SET EOF-YES TO TRUE
                       NOT AT END
                           IF ACCT-NUM = WS-SEARCH-ACCT
                               SET FOUND-YES TO TRUE
                               IF NOT ACCT-ACTIVE
                                   DISPLAY
                                     "  *** Account not active ***"
                                   SET FOUND-NO TO TRUE
                               ELSE
                                   ADD WS-INPUT-AMT
                                       TO ACCT-LEDGER-BAL
                                   ADD WS-INPUT-AMT
                                       TO ACCT-AVAIL-BAL
                                   MOVE WS-CURR-DATE
                                       TO ACCT-LAST-ACTIVITY-DT
                                   MOVE ACCT-CIF-ID
                                       TO WS-FOUND-CIF-ID
                                   MOVE ACCT-LEDGER-BAL
                                       TO WS-FOUND-LEDGER
                                   MOVE ACCT-LEDGER-BAL
                                       TO WS-FMT-BAL
                                   DISPLAY "  Deposit successful"
                                   DISPLAY "  New ledger bal: "
                                       WS-FMT-BAL
                               END-IF
                           END-IF
                           MOVE ACCT-REC TO ATMP-REC
                           WRITE ATMP-REC
                   END-READ
               END-PERFORM
               CLOSE ACCOUNT-FILE
               CLOSE ACCT-TEMP-FILE

               IF FOUND-NO
                   DISPLAY "  *** ACCOUNT NOT FOUND ***"
               ELSE
                   PERFORM 8000-SWAP-ACCT-FILE
                   PERFORM 4110-LOG-DEP
               END-IF
           END-IF
           .

       4110-LOG-DEP.
           OPEN EXTEND TRANSACTION-FILE
           IF WS-TXN-FS NOT = "00"
               OPEN OUTPUT TRANSACTION-FILE
           END-IF
           MOVE WS-NEXT-TXN-SEQ   TO WS-GEN-TXN-SEQ
           INITIALIZE TXN-REC
           MOVE "TJ"              TO TXN-REC-TYPE
           MOVE WS-GEN-TXN-SEQ   TO TXN-SEQ-NUM
           MOVE WS-SEARCH-ACCT    TO TXN-ACCT-NUM
           MOVE WS-FOUND-CIF-ID   TO TXN-CIF-ID
           MOVE WS-CURR-DATE      TO TXN-EFFDT
           MOVE WS-CURR-DATE      TO TXN-POST-DT
           MOVE WS-CURR-TIME(1:6) TO TXN-POST-TIME
           MOVE "DEP"             TO TXN-TYPE-CD
           MOVE "C"               TO TXN-DR-CR
           MOVE WS-INPUT-AMT      TO TXN-AMT
           MOVE WS-FOUND-LEDGER   TO TXN-RUN-BAL
           MOVE "BRN"             TO TXN-CHANNEL
           MOVE "TLR001"          TO TXN-TELLER-ID
           MOVE "TERM0001"        TO TXN-TERMINAL-ID
           MOVE SPACES            TO TXN-AUTH-CODE
           MOVE SPACES            TO TXN-REF-NUM
           MOVE "CASH DEPOSIT"
                                  TO TXN-DESC
           MOVE "00000001"        TO TXN-BATCH-NUM
           MOVE "0001"            TO TXN-CYCLE-NUM
           MOVE "1001000100"      TO TXN-GL-DR-ACCT
           MOVE "2001000100"      TO TXN-GL-CR-ACCT
           MOVE " "               TO TXN-REV-FLAG
           MOVE SPACES            TO TXN-REV-ORIG-SEQ
           MOVE "P"               TO TXN-STATUS
           MOVE SPACES            TO TXN-FILLER
           WRITE TXN-REC
           CLOSE TRANSACTION-FILE
           ADD 1 TO WS-NEXT-TXN-SEQ
           .

       4200-WITHDRAW.
           DISPLAY " "
           DISPLAY "  --- WITHDRAWAL ---"
           DISPLAY "  Account number: " WITH NO ADVANCING
           ACCEPT WS-SEARCH-ACCT
           DISPLAY "  Withdrawal amount: " WITH NO ADVANCING
           ACCEPT WS-INPUT-AMT

           IF WS-INPUT-AMT <= 0
               DISPLAY "  *** Amount must be positive ***"
           ELSE
               SET FOUND-NO TO TRUE
               MOVE SPACES TO WS-FOUND-CIF-ID
               OPEN INPUT ACCOUNT-FILE
               OPEN OUTPUT ACCT-TEMP-FILE
               SET EOF-NO TO TRUE

               PERFORM UNTIL EOF-YES
                   READ ACCOUNT-FILE
                       AT END SET EOF-YES TO TRUE
                       NOT AT END
                           IF ACCT-NUM = WS-SEARCH-ACCT
                               SET FOUND-YES TO TRUE
                               IF NOT ACCT-ACTIVE
                                   DISPLAY
                                     "  *** Account not active ***"
                                   SET FOUND-NO TO TRUE
                               ELSE
                               IF ACCT-AVAIL-BAL < WS-INPUT-AMT
                                   MOVE ACCT-AVAIL-BAL
                                       TO WS-FMT-BAL
                                   DISPLAY
                                     "  *** INSUFFICIENT FUNDS ***"
                                   DISPLAY "  Available: "
                                       WS-FMT-BAL
                                   SET FOUND-NO TO TRUE
                               ELSE
                                   SUBTRACT WS-INPUT-AMT
                                       FROM ACCT-LEDGER-BAL
                                   SUBTRACT WS-INPUT-AMT
                                       FROM ACCT-AVAIL-BAL
                                   MOVE WS-CURR-DATE
                                       TO ACCT-LAST-ACTIVITY-DT
                                   MOVE ACCT-CIF-ID
                                       TO WS-FOUND-CIF-ID
                                   MOVE ACCT-LEDGER-BAL
                                       TO WS-FOUND-LEDGER
                                   MOVE ACCT-LEDGER-BAL
                                       TO WS-FMT-BAL
                                   DISPLAY
                                     "  Withdrawal successful"
                                   DISPLAY "  New ledger bal: "
                                       WS-FMT-BAL
                               END-IF
                               END-IF
                           END-IF
                           MOVE ACCT-REC TO ATMP-REC
                           WRITE ATMP-REC
                   END-READ
               END-PERFORM
               CLOSE ACCOUNT-FILE
               CLOSE ACCT-TEMP-FILE

               IF FOUND-NO
                   DISPLAY "  *** ACCOUNT NOT FOUND ***"
               ELSE
                   PERFORM 8000-SWAP-ACCT-FILE
                   PERFORM 4210-LOG-WDR
               END-IF
           END-IF
           .

       4210-LOG-WDR.
           OPEN EXTEND TRANSACTION-FILE
           IF WS-TXN-FS NOT = "00"
               OPEN OUTPUT TRANSACTION-FILE
           END-IF
           MOVE WS-NEXT-TXN-SEQ   TO WS-GEN-TXN-SEQ
           INITIALIZE TXN-REC
           MOVE "TJ"              TO TXN-REC-TYPE
           MOVE WS-GEN-TXN-SEQ   TO TXN-SEQ-NUM
           MOVE WS-SEARCH-ACCT    TO TXN-ACCT-NUM
           MOVE WS-FOUND-CIF-ID   TO TXN-CIF-ID
           MOVE WS-CURR-DATE      TO TXN-EFFDT
           MOVE WS-CURR-DATE      TO TXN-POST-DT
           MOVE WS-CURR-TIME(1:6) TO TXN-POST-TIME
           MOVE "WDL"             TO TXN-TYPE-CD
           MOVE "D"               TO TXN-DR-CR
           MOVE WS-INPUT-AMT      TO TXN-AMT
           MOVE WS-FOUND-LEDGER   TO TXN-RUN-BAL
           MOVE "BRN"             TO TXN-CHANNEL
           MOVE "TLR001"          TO TXN-TELLER-ID
           MOVE "TERM0001"        TO TXN-TERMINAL-ID
           MOVE SPACES            TO TXN-AUTH-CODE
           MOVE SPACES            TO TXN-REF-NUM
           MOVE "CASH WITHDRAWAL"
                                  TO TXN-DESC
           MOVE "00000001"        TO TXN-BATCH-NUM
           MOVE "0001"            TO TXN-CYCLE-NUM
           MOVE "2001000100"      TO TXN-GL-DR-ACCT
           MOVE "1001000100"      TO TXN-GL-CR-ACCT
           MOVE " "               TO TXN-REV-FLAG
           MOVE SPACES            TO TXN-REV-ORIG-SEQ
           MOVE "P"               TO TXN-STATUS
           MOVE SPACES            TO TXN-FILLER
           WRITE TXN-REC
           CLOSE TRANSACTION-FILE
           ADD 1 TO WS-NEXT-TXN-SEQ
           .

       4300-TRANSFER.
           DISPLAY " "
           DISPLAY "  --- TRANSFER ---"
           DISPLAY "  Source account: " WITH NO ADVANCING
           ACCEPT WS-SEARCH-ACCT
           DISPLAY "  Target account: " WITH NO ADVANCING
           ACCEPT WS-XFER-TARGET
           DISPLAY "  Transfer amount: " WITH NO ADVANCING
           ACCEPT WS-INPUT-AMT

           IF WS-INPUT-AMT <= 0
               DISPLAY "  *** Amount must be positive ***"
           ELSE
               SET FOUND-NO TO TRUE
               SET VALID-NO TO TRUE
               MOVE SPACES TO WS-FOUND-CIF-ID
               OPEN INPUT ACCOUNT-FILE
               OPEN OUTPUT ACCT-TEMP-FILE
               SET EOF-NO TO TRUE

               PERFORM UNTIL EOF-YES
                   READ ACCOUNT-FILE
                       AT END SET EOF-YES TO TRUE
                       NOT AT END
                           IF ACCT-NUM = WS-SEARCH-ACCT
                               SET FOUND-YES TO TRUE
                               IF ACCT-AVAIL-BAL >= WS-INPUT-AMT
                                   SUBTRACT WS-INPUT-AMT
                                       FROM ACCT-LEDGER-BAL
                                   SUBTRACT WS-INPUT-AMT
                                       FROM ACCT-AVAIL-BAL
                                   MOVE WS-CURR-DATE
                                       TO ACCT-LAST-ACTIVITY-DT
                                   MOVE ACCT-CIF-ID
                                       TO WS-FOUND-CIF-ID
                                   MOVE ACCT-LEDGER-BAL
                                       TO WS-FOUND-LEDGER
                               ELSE
                                   DISPLAY
                                     "  *** INSUFFICIENT FUNDS ***"
                               END-IF
                           END-IF
                           IF ACCT-NUM = WS-XFER-TARGET
                               SET VALID-YES TO TRUE
                               ADD WS-INPUT-AMT
                                   TO ACCT-LEDGER-BAL
                               ADD WS-INPUT-AMT
                                   TO ACCT-AVAIL-BAL
                               MOVE WS-CURR-DATE
                                   TO ACCT-LAST-ACTIVITY-DT
                           END-IF
                           MOVE ACCT-REC TO ATMP-REC
                           WRITE ATMP-REC
                   END-READ
               END-PERFORM
               CLOSE ACCOUNT-FILE
               CLOSE ACCT-TEMP-FILE

               IF FOUND-NO
                   DISPLAY "  *** SOURCE ACCOUNT NOT FOUND ***"
               ELSE
               IF VALID-NO
                   DISPLAY "  *** TARGET ACCOUNT NOT FOUND ***"
               ELSE
                   PERFORM 8000-SWAP-ACCT-FILE
                   MOVE WS-INPUT-AMT TO WS-FMT-AMT
                   DISPLAY "  Transfer completed"
                   DISPLAY "  Amount: " WS-FMT-AMT
                   PERFORM 4310-LOG-XFR
               END-IF
               END-IF
           END-IF
           .

       4310-LOG-XFR.
           OPEN EXTEND TRANSACTION-FILE
           IF WS-TXN-FS NOT = "00"
               OPEN OUTPUT TRANSACTION-FILE
           END-IF

           MOVE WS-NEXT-TXN-SEQ   TO WS-GEN-TXN-SEQ
           INITIALIZE TXN-REC
           MOVE "TJ"              TO TXN-REC-TYPE
           MOVE WS-GEN-TXN-SEQ   TO TXN-SEQ-NUM
           MOVE WS-SEARCH-ACCT    TO TXN-ACCT-NUM
           MOVE WS-FOUND-CIF-ID   TO TXN-CIF-ID
           MOVE WS-CURR-DATE      TO TXN-EFFDT
           MOVE WS-CURR-DATE      TO TXN-POST-DT
           MOVE WS-CURR-TIME(1:6) TO TXN-POST-TIME
           MOVE "XDR"             TO TXN-TYPE-CD
           MOVE "D"               TO TXN-DR-CR
           MOVE WS-INPUT-AMT      TO TXN-AMT
           MOVE WS-FOUND-LEDGER   TO TXN-RUN-BAL
           MOVE "INT"             TO TXN-CHANNEL
           MOVE "SYSTEM"          TO TXN-TELLER-ID
           MOVE "INTERNAL"        TO TXN-TERMINAL-ID
           MOVE SPACES            TO TXN-AUTH-CODE
           MOVE SPACES            TO TXN-REF-NUM
           STRING "XFER TO " WS-XFER-TARGET
               DELIMITED BY SIZE INTO TXN-DESC
           END-STRING
           MOVE "00000001"        TO TXN-BATCH-NUM
           MOVE "0001"            TO TXN-CYCLE-NUM
           MOVE "2001000100"      TO TXN-GL-DR-ACCT
           MOVE "1001000200"      TO TXN-GL-CR-ACCT
           MOVE " "               TO TXN-REV-FLAG
           MOVE SPACES            TO TXN-REV-ORIG-SEQ
           MOVE "P"               TO TXN-STATUS
           MOVE SPACES            TO TXN-FILLER
           WRITE TXN-REC
           ADD 1 TO WS-NEXT-TXN-SEQ

           MOVE WS-NEXT-TXN-SEQ   TO WS-GEN-TXN-SEQ
           INITIALIZE TXN-REC
           MOVE "TJ"              TO TXN-REC-TYPE
           MOVE WS-GEN-TXN-SEQ   TO TXN-SEQ-NUM
           MOVE WS-XFER-TARGET    TO TXN-ACCT-NUM
           MOVE SPACES            TO TXN-CIF-ID
           MOVE WS-CURR-DATE      TO TXN-EFFDT
           MOVE WS-CURR-DATE      TO TXN-POST-DT
           MOVE WS-CURR-TIME(1:6) TO TXN-POST-TIME
           MOVE "XCR"             TO TXN-TYPE-CD
           MOVE "C"               TO TXN-DR-CR
           MOVE WS-INPUT-AMT      TO TXN-AMT
           MOVE 0                 TO TXN-RUN-BAL
           MOVE "INT"             TO TXN-CHANNEL
           MOVE "SYSTEM"          TO TXN-TELLER-ID
           MOVE "INTERNAL"        TO TXN-TERMINAL-ID
           MOVE SPACES            TO TXN-AUTH-CODE
           MOVE SPACES            TO TXN-REF-NUM
           STRING "XFER FR " WS-SEARCH-ACCT
               DELIMITED BY SIZE INTO TXN-DESC
           END-STRING
           MOVE "00000001"        TO TXN-BATCH-NUM
           MOVE "0001"            TO TXN-CYCLE-NUM
           MOVE "1001000200"      TO TXN-GL-DR-ACCT
           MOVE "2001000100"      TO TXN-GL-CR-ACCT
           MOVE " "               TO TXN-REV-FLAG
           MOVE SPACES            TO TXN-REV-ORIG-SEQ
           MOVE "P"               TO TXN-STATUS
           MOVE SPACES            TO TXN-FILLER
           WRITE TXN-REC
           ADD 1 TO WS-NEXT-TXN-SEQ

           CLOSE TRANSACTION-FILE
           .

       4400-BATCH-FEES.
           DISPLAY " "
           DISPLAY "  --- MONTHLY SERVICE FEE BATCH ---"
           DISPLAY "  Processing..."
           MOVE 0 TO WS-ACCT-CNT

           OPEN INPUT ACCOUNT-FILE
           IF WS-ACCT-FS NOT = "00"
               DISPLAY "  (no accounts on file)"
               CLOSE ACCOUNT-FILE
           ELSE
               OPEN OUTPUT ACCT-TEMP-FILE
               SET EOF-NO TO TRUE
               PERFORM UNTIL EOF-YES
                   READ ACCOUNT-FILE
                       AT END SET EOF-YES TO TRUE
                       NOT AT END
                           IF ACCT-ACTIVE
                              AND (ACCT-IS-DDA1
                                OR ACCT-IS-DDA2
                                OR ACCT-IS-COM1)
                               SUBTRACT 12.50
                                   FROM ACCT-LEDGER-BAL
                               SUBTRACT 12.50
                                   FROM ACCT-AVAIL-BAL
                               ADD 1 TO WS-ACCT-CNT
                               MOVE WS-CURR-DATE
                                   TO ACCT-LAST-ACTIVITY-DT
                           END-IF
                           MOVE ACCT-REC TO ATMP-REC
                           WRITE ATMP-REC
                   END-READ
               END-PERFORM
               CLOSE ACCOUNT-FILE
               CLOSE ACCT-TEMP-FILE
               PERFORM 8000-SWAP-ACCT-FILE
               DISPLAY "  Fees applied to " WS-ACCT-CNT
                   " DDA/commercial accounts"
               DISPLAY "  Fee amount: $12.50 per account"
           END-IF
           .

       5000-REPORTS.
           DISPLAY " "
           DISPLAY "---------------------------------------------"
           DISPLAY "       REPORT GENERATION"
           DISPLAY "---------------------------------------------"
           DISPLAY "  1. Account Summary Report"
           DISPLAY "  2. Transaction Journal Report"
           DISPLAY "  3. Daily Balance Summary (Screen)"
           DISPLAY "  0. Return to Main Menu"
           DISPLAY "---------------------------------------------"
           DISPLAY "  Enter choice: " WITH NO ADVANCING
           ACCEPT WS-SUB-CHOICE

           EVALUATE WS-SUB-CHOICE
               WHEN 1 PERFORM 5100-ACCT-RPT
               WHEN 2 PERFORM 5200-TXN-RPT
               WHEN 3 PERFORM 5300-DAILY-BAL
               WHEN 0 CONTINUE
               WHEN OTHER DISPLAY "  *** INVALID SELECTION ***"
           END-EVALUATE
           .

       5100-ACCT-RPT.
           DISPLAY "  Generating Account Summary Report..."
           OPEN OUTPUT REPORT-FILE
           MOVE 1 TO WS-RPT-PAGES
           MOVE 0 TO WS-RPT-LINES
           MOVE 0 TO WS-ACCT-CNT
           MOVE 0 TO WS-TOT-BAL

           PERFORM 5110-RPT-HDR

           OPEN INPUT ACCOUNT-FILE
           IF WS-ACCT-FS NOT = "00"
               MOVE "  (no accounts on file)" TO RPT-LINE
               WRITE RPT-LINE
               CLOSE ACCOUNT-FILE
           ELSE
               SET EOF-NO TO TRUE
               PERFORM UNTIL EOF-YES
                   READ ACCOUNT-FILE
                       AT END SET EOF-YES TO TRUE
                       NOT AT END
                           ADD 1 TO WS-ACCT-CNT
                           ADD ACCT-LEDGER-BAL TO WS-TOT-BAL
                           PERFORM 5120-RPT-DTL
                   END-READ
               END-PERFORM
               CLOSE ACCOUNT-FILE
           END-IF

           MOVE SPACES TO RPT-LINE
           WRITE RPT-LINE
           MOVE WS-RPT-SEP TO RPT-LINE
           WRITE RPT-LINE
           MOVE WS-ACCT-CNT TO WS-RT-TOT-ACCT
           MOVE WS-RPT-TOT-LN1 TO RPT-LINE
           WRITE RPT-LINE
           MOVE WS-TOT-BAL TO WS-RT-TOT-BAL
           MOVE WS-RPT-TOT-LN2 TO RPT-LINE
           WRITE RPT-LINE
           CLOSE REPORT-FILE
           DISPLAY "  Report written to RPTFILE.txt"
           DISPLAY "  Total accounts: " WS-ACCT-CNT
           .

       5110-RPT-HDR.
           MOVE WS-RPT-PAGES TO WS-RT-PG
           MOVE WS-RPT-SEP TO RPT-LINE
           WRITE RPT-LINE
           MOVE WS-RPT-TITLE TO RPT-LINE
           WRITE RPT-LINE
           MOVE WS-CURR-DATE(1:4) TO WS-DD-YEAR
           MOVE WS-CURR-DATE(5:2) TO WS-DD-MONTH
           MOVE WS-CURR-DATE(7:2) TO WS-DD-DAY
           MOVE WS-DISP-DATE TO WS-RT-DT
           MOVE WS-RPT-DATE-LN TO RPT-LINE
           WRITE RPT-LINE
           MOVE WS-RPT-SEP TO RPT-LINE
           WRITE RPT-LINE
           MOVE WS-RPT-COL-HDR TO RPT-LINE
           WRITE RPT-LINE
           MOVE WS-RPT-SEP TO RPT-LINE
           WRITE RPT-LINE
           MOVE 6 TO WS-RPT-LINES
           .

       5120-RPT-DTL.
           IF WS-RPT-LINES > 55
               ADD 1 TO WS-RPT-PAGES
               PERFORM 5110-RPT-HDR
           END-IF
           MOVE SPACES TO WS-RPT-DTL
           MOVE ACCT-NUM        TO WS-RD-ACCT
           MOVE ACCT-PROD-DESC  TO WS-RD-NAME
           MOVE ACCT-PROD-CODE  TO WS-RD-PROD
           MOVE ACCT-STATUS     TO WS-RD-STAT
           MOVE ACCT-LEDGER-BAL TO WS-RD-LEDGER
           MOVE ACCT-AVAIL-BAL  TO WS-RD-AVAIL
           MOVE ACCT-INT-RATE   TO WS-RD-RATE
           MOVE ACCT-BRANCH     TO WS-RD-BRNCH
           MOVE ACCT-RESTRICT-CD TO WS-RD-RESTRICT
           MOVE WS-RPT-DTL TO RPT-LINE
           WRITE RPT-LINE
           ADD 1 TO WS-RPT-LINES
           .

       5200-TXN-RPT.
           DISPLAY "  Generating Transaction Journal Report..."
           OPEN OUTPUT REPORT-FILE
           MOVE 0 TO WS-TXN-CNT
           MOVE 0 TO WS-TOT-DEP
           MOVE 0 TO WS-TOT-WDR

           MOVE WS-RPT-SEP TO RPT-LINE
           WRITE RPT-LINE
           MOVE
             "  FIRST NATIONAL BANK - TRANSACTION JOURNAL"
               TO RPT-LINE
           WRITE RPT-LINE
           MOVE WS-RPT-SEP TO RPT-LINE
           WRITE RPT-LINE
           MOVE WS-TXN-COL-HDR TO RPT-LINE
           WRITE RPT-LINE
           MOVE WS-RPT-SEP TO RPT-LINE
           WRITE RPT-LINE

           OPEN INPUT TRANSACTION-FILE
           IF WS-TXN-FS NOT = "00"
               MOVE "  (no transactions on file)" TO RPT-LINE
               WRITE RPT-LINE
               CLOSE TRANSACTION-FILE
           ELSE
               SET EOF-NO TO TRUE
               PERFORM UNTIL EOF-YES
                   READ TRANSACTION-FILE
                       AT END SET EOF-YES TO TRUE
                       NOT AT END
                           ADD 1 TO WS-TXN-CNT
                           PERFORM 5210-TXN-DTL
                   END-READ
               END-PERFORM
               CLOSE TRANSACTION-FILE
           END-IF

           MOVE SPACES TO RPT-LINE
           WRITE RPT-LINE
           MOVE WS-RPT-SEP TO RPT-LINE
           WRITE RPT-LINE

           MOVE SPACES TO RPT-LINE
           STRING "  TOTAL TRANSACTIONS: " WS-TXN-CNT
               DELIMITED BY SIZE INTO RPT-LINE
           END-STRING
           WRITE RPT-LINE

           MOVE WS-TOT-DEP TO WS-FMT-AMT
           MOVE SPACES TO RPT-LINE
           STRING "  TOTAL CREDITS:      " WS-FMT-AMT
               DELIMITED BY SIZE INTO RPT-LINE
           END-STRING
           WRITE RPT-LINE

           MOVE WS-TOT-WDR TO WS-FMT-AMT
           MOVE SPACES TO RPT-LINE
           STRING "  TOTAL DEBITS:       " WS-FMT-AMT
               DELIMITED BY SIZE INTO RPT-LINE
           END-STRING
           WRITE RPT-LINE

           CLOSE REPORT-FILE
           DISPLAY "  Report written to RPTFILE.txt"
           DISPLAY "  Total transactions: " WS-TXN-CNT
           .

       5210-TXN-DTL.
           MOVE TXN-SEQ-NUM   TO WS-TD-SEQ
           MOVE TXN-ACCT-NUM  TO WS-TD-ACCT
           MOVE TXN-TYPE-CD   TO WS-TD-TYPE
           MOVE TXN-DR-CR     TO WS-TD-DRCR
           IF TXN-CREDIT
               ADD TXN-AMT TO WS-TOT-DEP
           END-IF
           IF TXN-DEBIT
               ADD TXN-AMT TO WS-TOT-WDR
           END-IF
           MOVE TXN-AMT TO WS-TD-AMT
           MOVE TXN-EFFDT(1:4) TO WS-DD-YEAR
           MOVE TXN-EFFDT(5:2) TO WS-DD-MONTH
           MOVE TXN-EFFDT(7:2) TO WS-DD-DAY
           MOVE WS-DISP-DATE TO WS-TD-EFFDT
           MOVE TXN-CHANNEL TO WS-TD-CHAN
           MOVE TXN-DESC TO WS-TD-DESC
           MOVE TXN-STATUS TO WS-TD-STAT
           MOVE WS-TXN-DTL TO RPT-LINE
           WRITE RPT-LINE
           .

       5300-DAILY-BAL.
           DISPLAY "  Computing daily balance summary..."
           MOVE 0 TO WS-ACCT-CNT
           MOVE 0 TO WS-TOT-BAL
           MOVE 0 TO WS-TOT-DEP
           MOVE 0 TO WS-TOT-WDR

           OPEN INPUT ACCOUNT-FILE
           IF WS-ACCT-FS NOT = "00"
               DISPLAY "  (no accounts on file)"
               CLOSE ACCOUNT-FILE
           ELSE
               SET EOF-NO TO TRUE
               PERFORM UNTIL EOF-YES
                   READ ACCOUNT-FILE
                       AT END SET EOF-YES TO TRUE
                       NOT AT END
                           IF ACCT-ACTIVE
                               ADD 1 TO WS-ACCT-CNT
                               ADD ACCT-LEDGER-BAL
                                   TO WS-TOT-BAL
                               IF ACCT-IS-SAV1
                                  OR ACCT-IS-MMA1
                                  OR ACCT-IS-CD01
                                   ADD ACCT-LEDGER-BAL
                                       TO WS-TOT-DEP
                               END-IF
                               IF ACCT-IS-HEL1
                                  OR ACCT-IS-MTG1
                                   ADD ACCT-LEDGER-BAL
                                       TO WS-TOT-WDR
                               END-IF
                           END-IF
                   END-READ
               END-PERFORM
               CLOSE ACCOUNT-FILE

               MOVE WS-CURR-DATE(1:4) TO WS-DD-YEAR
               MOVE WS-CURR-DATE(5:2) TO WS-DD-MONTH
               MOVE WS-CURR-DATE(7:2) TO WS-DD-DAY
               DISPLAY " "
               DISPLAY "  =================================="
               DISPLAY "   DAILY BALANCE SUMMARY"
               DISPLAY "   Date: " WS-DISP-DATE
               DISPLAY "  =================================="
               DISPLAY "   Active Accounts: " WS-ACCT-CNT
               MOVE WS-TOT-BAL TO WS-FMT-BAL
               DISPLAY "   Total Assets:    " WS-FMT-BAL
               MOVE WS-TOT-DEP TO WS-FMT-BAL
               DISPLAY "   Savings Pool:    " WS-FMT-BAL
               MOVE WS-TOT-WDR TO WS-FMT-BAL
               DISPLAY "   Loan Portfolio:  " WS-FMT-BAL
               DISPLAY "  =================================="
           END-IF
           .

       6000-INQUIRY.
           DISPLAY " "
           DISPLAY "  --- ACCOUNT INQUIRY ---"
           DISPLAY "  Account number: " WITH NO ADVANCING
           ACCEPT WS-SEARCH-ACCT

           SET FOUND-NO TO TRUE
           OPEN INPUT ACCOUNT-FILE
           IF WS-ACCT-FS NOT = "00"
               DISPLAY "  *** No accounts on file ***"
               CLOSE ACCOUNT-FILE
           ELSE
               SET EOF-NO TO TRUE
               PERFORM UNTIL EOF-YES
                   READ ACCOUNT-FILE
                       AT END SET EOF-YES TO TRUE
                       NOT AT END
                           IF ACCT-NUM = WS-SEARCH-ACCT
                               SET FOUND-YES TO TRUE
                               PERFORM 6100-SHOW-DETAIL
                           END-IF
                   END-READ
               END-PERFORM
               CLOSE ACCOUNT-FILE
               IF FOUND-NO
                   DISPLAY "  *** ACCOUNT NOT FOUND ***"
               END-IF
           END-IF
           .

       6100-SHOW-DETAIL.
           DISPLAY " "
           DISPLAY "  ======================================="
           DISPLAY "   ACCOUNT DETAIL INQUIRY"
           DISPLAY "  ======================================="
           DISPLAY "   Account Number:  " ACCT-NUM
           DISPLAY "   CIF ID:          " ACCT-CIF-ID
           DISPLAY "   Product Code:    " ACCT-PROD-CODE
           DISPLAY "   Product Desc:    " ACCT-PROD-DESC
           DISPLAY "   Status:          " ACCT-STATUS
           MOVE ACCT-LEDGER-BAL TO WS-FMT-BAL
           DISPLAY "   Ledger Balance:  " WS-FMT-BAL
           MOVE ACCT-AVAIL-BAL TO WS-FMT-BAL
           DISPLAY "   Avail Balance:   " WS-FMT-BAL
           MOVE ACCT-HOLD-BAL TO WS-FMT-BAL
           DISPLAY "   Hold Amount:     " WS-FMT-BAL
           MOVE ACCT-UNCOLL-BAL TO WS-FMT-BAL
           DISPLAY "   Uncoll Float:    " WS-FMT-BAL
           MOVE ACCT-OD-LIMIT TO WS-FMT-AMT
           DISPLAY "   OD Limit:        " WS-FMT-AMT
           MOVE ACCT-INT-RATE TO WS-FMT-RATE
           DISPLAY "   Interest Rate:   " WS-FMT-RATE
           MOVE ACCT-INT-ACCRUED TO WS-FMT-AMT
           DISPLAY "   Accrued Int:     " WS-FMT-AMT
           MOVE ACCT-INT-YTD TO WS-FMT-AMT
           DISPLAY "   YTD Interest:    " WS-FMT-AMT
           MOVE ACCT-OPEN-DT(1:4) TO WS-DD-YEAR
           MOVE ACCT-OPEN-DT(5:2) TO WS-DD-MONTH
           MOVE ACCT-OPEN-DT(7:2) TO WS-DD-DAY
           DISPLAY "   Opened:          " WS-DISP-DATE
           IF ACCT-CLOSE-DT > 0
               MOVE ACCT-CLOSE-DT(1:4) TO WS-DD-YEAR
               MOVE ACCT-CLOSE-DT(5:2) TO WS-DD-MONTH
               MOVE ACCT-CLOSE-DT(7:2) TO WS-DD-DAY
               DISPLAY "   Closed:          " WS-DISP-DATE
           END-IF
           DISPLAY "   Branch:          " ACCT-BRANCH
           DISPLAY "   Cost Center:     " ACCT-COST-CENTER
           DISPLAY "   GL Account:      " ACCT-GL-ACCT
           DISPLAY "   Currency:        " ACCT-CURRENCY
           DISPLAY "   Stmt Cycle:      " ACCT-STMT-CYCLE
           DISPLAY "   Restriction:     " ACCT-RESTRICT-CD
           DISPLAY "   Reg-D Counter:   " ACCT-REGD-CTR
           DISPLAY "   Tax Reporting:   " ACCT-TAX-RPTG
           DISPLAY "   Officer:         " ACCT-OFFICER
           MOVE ACCT-LAST-ACTIVITY-DT(1:4) TO WS-DD-YEAR
           MOVE ACCT-LAST-ACTIVITY-DT(5:2) TO WS-DD-MONTH
           MOVE ACCT-LAST-ACTIVITY-DT(7:2) TO WS-DD-DAY
           DISPLAY "   Last Activity:   " WS-DISP-DATE
           DISPLAY "   Maint User:      " ACCT-MAINT-USER
           DISPLAY "  ======================================="

           PERFORM 6200-SHOW-CIF-DETAIL
           .

       6200-SHOW-CIF-DETAIL.
           OPEN INPUT CUSTOMER-FILE
           IF WS-CUST-FS NOT = "00"
               CLOSE CUSTOMER-FILE
           ELSE
               SET EOF-NO TO TRUE
               PERFORM UNTIL EOF-YES
                   READ CUSTOMER-FILE
                       AT END SET EOF-YES TO TRUE
                       NOT AT END
                           IF CUST-CIF-ID = ACCT-CIF-ID
                               DISPLAY " "
                               DISPLAY
                              "  --- CUSTOMER (CIF) DETAIL ---"
                               DISPLAY "   CIF ID:      "
                                   CUST-CIF-ID
                               DISPLAY "   Name:        "
                                   CUST-LAST-NAME ", "
                                   CUST-FIRST-NAME
                               DISPLAY "   Tax ID Type: "
                                   CUST-TAX-ID-TYPE
                               DISPLAY "   Tax ID:      "
                                   CUST-TAX-ID
                               DISPLAY "   Address:     "
                                   CUST-ADDR-LINE1
                               IF CUST-ADDR-LINE2
                                   NOT = SPACES
                                   DISPLAY
                                     "                "
                                       CUST-ADDR-LINE2
                               END-IF
                               DISPLAY "   City/St/Zip: "
                                   CUST-CITY " "
                                   CUST-STATE " "
                                   CUST-ZIP "-"
                                   CUST-ZIP4
                               DISPLAY "   Country:     "
                                   CUST-COUNTRY
                               DISPLAY "   KYC Status:  "
                                   CUST-KYC-STATUS
                               DISPLAY "   AML Risk:    "
                                   CUST-AML-RISK
                               DISPLAY "   Segment:     "
                                   CUST-SEGMENT
                               DISPLAY "   CIF Status:  "
                                   CUST-STATUS
                               DISPLAY "   Rel Officer: "
                                   CUST-REL-OFFICER
                               DISPLAY "  =================="
                           END-IF
                   END-READ
               END-PERFORM
               CLOSE CUSTOMER-FILE
           END-IF
           .

       8000-SWAP-ACCT-FILE.
           OPEN INPUT ACCT-TEMP-FILE
           OPEN OUTPUT ACCOUNT-FILE
           SET EOF-NO TO TRUE
           PERFORM UNTIL EOF-YES
               READ ACCT-TEMP-FILE
                   AT END SET EOF-YES TO TRUE
                   NOT AT END
                       MOVE ATMP-REC TO ACCT-REC
                       WRITE ACCT-REC
               END-READ
           END-PERFORM
           CLOSE ACCT-TEMP-FILE
           CLOSE ACCOUNT-FILE
           .

       8100-SWAP-CUST-FILE.
           OPEN INPUT CUST-TEMP-FILE
           OPEN OUTPUT CUSTOMER-FILE
           SET EOF-NO TO TRUE
           PERFORM UNTIL EOF-YES
               READ CUST-TEMP-FILE
                   AT END SET EOF-YES TO TRUE
                   NOT AT END
                       MOVE CTMP-REC TO CUST-REC
                       WRITE CUST-REC
               END-READ
           END-PERFORM
           CLOSE CUST-TEMP-FILE
           CLOSE CUSTOMER-FILE
           .

       9000-TERM.
           DISPLAY " "
           DISPLAY "============================================="
           DISPLAY "  SYSTEM SHUTDOWN COMPLETE"
           DISPLAY "  THANK YOU FOR USING FIRST NATIONAL BANK"
           DISPLAY "  CORE BANKING SYSTEM"
           DISPLAY "============================================="
           DISPLAY " "
           .
