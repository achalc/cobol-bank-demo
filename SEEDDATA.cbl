       IDENTIFICATION DIVISION.
       PROGRAM-ID. SEEDDATA.
      *============================================================*
      * DATA SEEDER - Populates CUSTMSTR, ACCTMSTR, TXNJRNL        *
      * with production-representative sample data                  *
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

       DATA DIVISION.
       FILE SECTION.

       FD CUSTOMER-FILE.
       01 CUST-REC.
           05 CUST-REC-TYPE           PIC X(02).
           05 CUST-CIF-ID             PIC X(10).
           05 CUST-TAX-ID-TYPE        PIC X(01).
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
           05 CUST-KYC-DATE          PIC 9(08).
           05 CUST-AML-RISK          PIC X(01).
           05 CUST-CRA-CODE          PIC X(02).
           05 CUST-REL-OFFICER       PIC X(06).
           05 CUST-SEGMENT           PIC X(02).
           05 CUST-STATUS            PIC X(02).
           05 CUST-OPEN-DATE         PIC 9(08).
           05 CUST-MAINT-DATE        PIC 9(08).
           05 CUST-MAINT-USER        PIC X(08).
           05 CUST-FILLER            PIC X(04).

       FD ACCOUNT-FILE.
       01 ACCT-REC.
           05 ACCT-REC-TYPE           PIC X(02).
           05 ACCT-NUM                PIC X(16).
           05 ACCT-CIF-ID            PIC X(10).
           05 ACCT-PROD-CODE         PIC X(04).
           05 ACCT-PROD-DESC         PIC X(20).
           05 ACCT-STATUS            PIC X(02).
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
           05 ACCT-REGD-CTR          PIC 9(02).
           05 ACCT-TAX-RPTG          PIC X(01).
           05 ACCT-BRANCH            PIC X(06).
           05 ACCT-LAST-ACTIVITY-DT  PIC 9(08).
           05 ACCT-MAINT-DT          PIC 9(08).
           05 ACCT-MAINT-USER        PIC X(08).
           05 ACCT-FILLER            PIC X(20).

       FD TRANSACTION-FILE.
       01 TXN-REC.
           05 TXN-REC-TYPE           PIC X(02).
           05 TXN-SEQ-NUM            PIC X(12).
           05 TXN-ACCT-NUM           PIC X(16).
           05 TXN-CIF-ID             PIC X(10).
           05 TXN-EFFDT              PIC 9(08).
           05 TXN-POST-DT            PIC 9(08).
           05 TXN-POST-TIME          PIC 9(06).
           05 TXN-TYPE-CD            PIC X(03).
           05 TXN-DR-CR              PIC X(01).
           05 TXN-AMT                PIC S9(11)V99.
           05 TXN-RUN-BAL            PIC S9(11)V99.
           05 TXN-CHANNEL            PIC X(03).
           05 TXN-TELLER-ID          PIC X(06).
           05 TXN-TERMINAL-ID        PIC X(08).
           05 TXN-AUTH-CODE          PIC X(06).
           05 TXN-REF-NUM            PIC X(20).
           05 TXN-DESC               PIC X(40).
           05 TXN-BATCH-NUM          PIC X(08).
           05 TXN-CYCLE-NUM          PIC X(04).
           05 TXN-GL-DR-ACCT        PIC X(10).
           05 TXN-GL-CR-ACCT        PIC X(10).
           05 TXN-REV-FLAG           PIC X(01).
           05 TXN-REV-ORIG-SEQ      PIC X(12).
           05 TXN-STATUS             PIC X(01).
           05 TXN-FILLER             PIC X(13).

       WORKING-STORAGE SECTION.
       01 WS-CUST-FS                 PIC XX.
       01 WS-ACCT-FS                 PIC XX.
       01 WS-TXN-FS                  PIC XX.

       PROCEDURE DIVISION.

       0000-MAIN.
           DISPLAY "Seeding sample data..."
           PERFORM 1000-SEED-CUSTOMERS
           PERFORM 2000-SEED-ACCOUNTS
           PERFORM 3000-SEED-TXNS
           DISPLAY "Sample data created successfully."
           DISPLAY "  - CUSTMSTR.dat (8 customers)"
           DISPLAY "  - ACCTMSTR.dat (10 accounts)"
           DISPLAY "  - TXNJRNL.dat  (15 transactions)"
           STOP RUN
           .

      *------------------------------------------------------------*
      * 1000 - SEED CUSTOMER INFORMATION FILE                       *
      *------------------------------------------------------------*
       1000-SEED-CUSTOMERS.
           OPEN OUTPUT CUSTOMER-FILE

           INITIALIZE CUST-REC
           MOVE "CI"           TO CUST-REC-TYPE
           MOVE "0000000001"   TO CUST-CIF-ID
           MOVE "S"            TO CUST-TAX-ID-TYPE
           MOVE "***-**-4721"  TO CUST-TAX-ID
           MOVE "JOHNSON"      TO CUST-LAST-NAME
           MOVE "ROBERT"       TO CUST-FIRST-NAME
           MOVE "A"            TO CUST-MI
           MOVE SPACES         TO CUST-SUFFIX
           MOVE "1247 PARK AVENUE"
                               TO CUST-ADDR-LINE1
           MOVE "APT 14B"      TO CUST-ADDR-LINE2
           MOVE "NEW YORK"     TO CUST-CITY
           MOVE "NY"           TO CUST-STATE
           MOVE "10128"        TO CUST-ZIP
           MOVE "3341"         TO CUST-ZIP4
           MOVE "USA"          TO CUST-COUNTRY
           MOVE "212-555-0147" TO CUST-PHONE-PRIMARY
           MOVE "917-555-0382" TO CUST-PHONE-ALT
           MOVE "rjohnson@email.com"
                               TO CUST-EMAIL
           MOVE "V"            TO CUST-KYC-STATUS
           MOVE 20190301       TO CUST-KYC-DATE
           MOVE "L"            TO CUST-AML-RISK
           MOVE "01"           TO CUST-CRA-CODE
           MOVE "OFC042"       TO CUST-REL-OFFICER
           MOVE "RT"           TO CUST-SEGMENT
           MOVE "AC"           TO CUST-STATUS
           MOVE 20190315       TO CUST-OPEN-DATE
           MOVE 20260115       TO CUST-MAINT-DATE
           MOVE "JMURPHY "     TO CUST-MAINT-USER
           MOVE SPACES         TO CUST-FILLER
           WRITE CUST-REC

           INITIALIZE CUST-REC
           MOVE "CI"           TO CUST-REC-TYPE
           MOVE "0000000002"   TO CUST-CIF-ID
           MOVE "S"            TO CUST-TAX-ID-TYPE
           MOVE "***-**-8834"  TO CUST-TAX-ID
           MOVE "WILLIAMS"     TO CUST-LAST-NAME
           MOVE "SARAH"        TO CUST-FIRST-NAME
           MOVE "M"            TO CUST-MI
           MOVE SPACES         TO CUST-SUFFIX
           MOVE "89 EAST 72ND STREET"
                               TO CUST-ADDR-LINE1
           MOVE SPACES         TO CUST-ADDR-LINE2
           MOVE "NEW YORK"     TO CUST-CITY
           MOVE "NY"           TO CUST-STATE
           MOVE "10021"        TO CUST-ZIP
           MOVE "4102"         TO CUST-ZIP4
           MOVE "USA"          TO CUST-COUNTRY
           MOVE "212-555-0293" TO CUST-PHONE-PRIMARY
           MOVE SPACES         TO CUST-PHONE-ALT
           MOVE "swilliams@privatebank.com"
                               TO CUST-EMAIL
           MOVE "V"            TO CUST-KYC-STATUS
           MOVE 20180610       TO CUST-KYC-DATE
           MOVE "L"            TO CUST-AML-RISK
           MOVE "01"           TO CUST-CRA-CODE
           MOVE "OFC017"       TO CUST-REL-OFFICER
           MOVE "WM"           TO CUST-SEGMENT
           MOVE "AC"           TO CUST-STATUS
           MOVE 20180622       TO CUST-OPEN-DATE
           MOVE 20260110       TO CUST-MAINT-DATE
           MOVE "SOKONKW "     TO CUST-MAINT-USER
           MOVE SPACES         TO CUST-FILLER
           WRITE CUST-REC

           INITIALIZE CUST-REC
           MOVE "CI"           TO CUST-REC-TYPE
           MOVE "0000000003"   TO CUST-CIF-ID
           MOVE "S"            TO CUST-TAX-ID-TYPE
           MOVE "***-**-2156"  TO CUST-TAX-ID
           MOVE "CHEN"         TO CUST-LAST-NAME
           MOVE "DAVID"        TO CUST-FIRST-NAME
           MOVE "L"            TO CUST-MI
           MOVE SPACES         TO CUST-SUFFIX
           MOVE "4501 BROADWAY"
                               TO CUST-ADDR-LINE1
           MOVE "SUITE 200"    TO CUST-ADDR-LINE2
           MOVE "NEW YORK"     TO CUST-CITY
           MOVE "NY"           TO CUST-STATE
           MOVE "10040"        TO CUST-ZIP
           MOVE "2587"         TO CUST-ZIP4
           MOVE "USA"          TO CUST-COUNTRY
           MOVE "646-555-0184" TO CUST-PHONE-PRIMARY
           MOVE "646-555-0199" TO CUST-PHONE-ALT
           MOVE "dchen@corp.com"
                               TO CUST-EMAIL
           MOVE "V"            TO CUST-KYC-STATUS
           MOVE 20200801       TO CUST-KYC-DATE
           MOVE "L"            TO CUST-AML-RISK
           MOVE "02"           TO CUST-CRA-CODE
           MOVE "OFC042"       TO CUST-REL-OFFICER
           MOVE "RT"           TO CUST-SEGMENT
           MOVE "AC"           TO CUST-STATUS
           MOVE 20200810       TO CUST-OPEN-DATE
           MOVE 20260125       TO CUST-MAINT-DATE
           MOVE "PZHANG  "     TO CUST-MAINT-USER
           MOVE SPACES         TO CUST-FILLER
           WRITE CUST-REC

           INITIALIZE CUST-REC
           MOVE "CI"           TO CUST-REC-TYPE
           MOVE "0000000004"   TO CUST-CIF-ID
           MOVE "S"            TO CUST-TAX-ID-TYPE
           MOVE "***-**-6743"  TO CUST-TAX-ID
           MOVE "MARTINEZ"     TO CUST-LAST-NAME
           MOVE "ANA"          TO CUST-FIRST-NAME
           MOVE "P"            TO CUST-MI
           MOVE SPACES         TO CUST-SUFFIX
           MOVE "200 CENTRAL PARK SOUTH"
                               TO CUST-ADDR-LINE1
           MOVE "UNIT 31F"     TO CUST-ADDR-LINE2
           MOVE "NEW YORK"     TO CUST-CITY
           MOVE "NY"           TO CUST-STATE
           MOVE "10019"        TO CUST-ZIP
           MOVE "1001"         TO CUST-ZIP4
           MOVE "USA"          TO CUST-COUNTRY
           MOVE "212-555-0456" TO CUST-PHONE-PRIMARY
           MOVE SPACES         TO CUST-PHONE-ALT
           MOVE "amartinez@wealth.com"
                               TO CUST-EMAIL
           MOVE "V"            TO CUST-KYC-STATUS
           MOVE 20210315       TO CUST-KYC-DATE
           MOVE "L"            TO CUST-AML-RISK
           MOVE "01"           TO CUST-CRA-CODE
           MOVE "OFC017"       TO CUST-REL-OFFICER
           MOVE "PB"           TO CUST-SEGMENT
           MOVE "AC"           TO CUST-STATUS
           MOVE 20210401       TO CUST-OPEN-DATE
           MOVE 20260101       TO CUST-MAINT-DATE
           MOVE "KDAVIES "     TO CUST-MAINT-USER
           MOVE SPACES         TO CUST-FILLER
           WRITE CUST-REC

           INITIALIZE CUST-REC
           MOVE "CI"           TO CUST-REC-TYPE
           MOVE "0000000005"   TO CUST-CIF-ID
           MOVE "S"            TO CUST-TAX-ID-TYPE
           MOVE "***-**-9018"  TO CUST-TAX-ID
           MOVE "THOMPSON"     TO CUST-LAST-NAME
           MOVE "MICHAEL"      TO CUST-FIRST-NAME
           MOVE "J"            TO CUST-MI
           MOVE SPACES         TO CUST-SUFFIX
           MOVE "3820 JEROME AVENUE"
                               TO CUST-ADDR-LINE1
           MOVE SPACES         TO CUST-ADDR-LINE2
           MOVE "BRONX"        TO CUST-CITY
           MOVE "NY"           TO CUST-STATE
           MOVE "10467"        TO CUST-ZIP
           MOVE "5590"         TO CUST-ZIP4
           MOVE "USA"          TO CUST-COUNTRY
           MOVE "718-555-0337" TO CUST-PHONE-PRIMARY
           MOVE SPACES         TO CUST-PHONE-ALT
           MOVE "mthompson@mail.com"
                               TO CUST-EMAIL
           MOVE "V"            TO CUST-KYC-STATUS
           MOVE 20220110       TO CUST-KYC-DATE
           MOVE "L"            TO CUST-AML-RISK
           MOVE "03"           TO CUST-CRA-CODE
           MOVE "OFC098"       TO CUST-REL-OFFICER
           MOVE "RT"           TO CUST-SEGMENT
           MOVE "AC"           TO CUST-STATUS
           MOVE 20220115       TO CUST-OPEN-DATE
           MOVE 20260128       TO CUST-MAINT-DATE
           MOVE "RGUPTA  "     TO CUST-MAINT-USER
           MOVE SPACES         TO CUST-FILLER
           WRITE CUST-REC

           INITIALIZE CUST-REC
           MOVE "CI"           TO CUST-REC-TYPE
           MOVE "0000000006"   TO CUST-CIF-ID
           MOVE "S"            TO CUST-TAX-ID-TYPE
           MOVE "***-**-3407"  TO CUST-TAX-ID
           MOVE "PATEL"        TO CUST-LAST-NAME
           MOVE "PRIYA"        TO CUST-FIRST-NAME
           MOVE "S"            TO CUST-MI
           MOVE SPACES         TO CUST-SUFFIX
           MOVE "15 BROAD STREET"
                               TO CUST-ADDR-LINE1
           MOVE "FL 28"        TO CUST-ADDR-LINE2
           MOVE "NEW YORK"     TO CUST-CITY
           MOVE "NY"           TO CUST-STATE
           MOVE "10005"        TO CUST-ZIP
           MOVE "1092"         TO CUST-ZIP4
           MOVE "USA"          TO CUST-COUNTRY
           MOVE "212-555-0741" TO CUST-PHONE-PRIMARY
           MOVE "347-555-0622" TO CUST-PHONE-ALT
           MOVE "ppatel@invest.com"
                               TO CUST-EMAIL
           MOVE "V"            TO CUST-KYC-STATUS
           MOVE 20170815       TO CUST-KYC-DATE
           MOVE "M"            TO CUST-AML-RISK
           MOVE "01"           TO CUST-CRA-CODE
           MOVE "OFC017"       TO CUST-REL-OFFICER
           MOVE "WM"           TO CUST-SEGMENT
           MOVE "AC"           TO CUST-STATUS
           MOVE 20170903       TO CUST-OPEN-DATE
           MOVE 20260203       TO CUST-MAINT-DATE
           MOVE "SOKONKW "     TO CUST-MAINT-USER
           MOVE SPACES         TO CUST-FILLER
           WRITE CUST-REC

           INITIALIZE CUST-REC
           MOVE "CI"           TO CUST-REC-TYPE
           MOVE "0000000007"   TO CUST-CIF-ID
           MOVE "S"            TO CUST-TAX-ID-TYPE
           MOVE "***-**-5562"  TO CUST-TAX-ID
           MOVE "OBRIEN"       TO CUST-LAST-NAME
           MOVE "KATHERINE"    TO CUST-FIRST-NAME
           MOVE "E"            TO CUST-MI
           MOVE SPACES         TO CUST-SUFFIX
           MOVE "447 WEST 18TH STREET"
                               TO CUST-ADDR-LINE1
           MOVE SPACES         TO CUST-ADDR-LINE2
           MOVE "NEW YORK"     TO CUST-CITY
           MOVE "NY"           TO CUST-STATE
           MOVE "10011"        TO CUST-ZIP
           MOVE "6320"         TO CUST-ZIP4
           MOVE "USA"          TO CUST-COUNTRY
           MOVE "212-555-0889" TO CUST-PHONE-PRIMARY
           MOVE SPACES         TO CUST-PHONE-ALT
           MOVE "kobrien@law.com"
                               TO CUST-EMAIL
           MOVE "X"            TO CUST-KYC-STATUS
           MOVE 20230701       TO CUST-KYC-DATE
           MOVE "L"            TO CUST-AML-RISK
           MOVE "01"           TO CUST-CRA-CODE
           MOVE "OFC098"       TO CUST-REL-OFFICER
           MOVE "RT"           TO CUST-SEGMENT
           MOVE "IN"           TO CUST-STATUS
           MOVE 20230718       TO CUST-OPEN-DATE
           MOVE 20251215       TO CUST-MAINT-DATE
           MOVE "PZHANG  "     TO CUST-MAINT-USER
           MOVE SPACES         TO CUST-FILLER
           WRITE CUST-REC

           INITIALIZE CUST-REC
           MOVE "CI"           TO CUST-REC-TYPE
           MOVE "0000000008"   TO CUST-CIF-ID
           MOVE "E"            TO CUST-TAX-ID-TYPE
           MOVE "**-***-8901"  TO CUST-TAX-ID
           MOVE "NAKAMURA INTL LLC"
                               TO CUST-LAST-NAME
           MOVE SPACES         TO CUST-FIRST-NAME
           MOVE SPACES         TO CUST-MI
           MOVE SPACES         TO CUST-SUFFIX
           MOVE "350 5TH AVENUE"
                               TO CUST-ADDR-LINE1
           MOVE "SUITE 7800"   TO CUST-ADDR-LINE2
           MOVE "NEW YORK"     TO CUST-CITY
           MOVE "NY"           TO CUST-STATE
           MOVE "10118"        TO CUST-ZIP
           MOVE "0110"         TO CUST-ZIP4
           MOVE "USA"          TO CUST-COUNTRY
           MOVE "212-555-0901" TO CUST-PHONE-PRIMARY
           MOVE "212-555-0902" TO CUST-PHONE-ALT
           MOVE "ops@nakamuraintl.com"
                               TO CUST-EMAIL
           MOVE "V"            TO CUST-KYC-STATUS
           MOVE 20240115       TO CUST-KYC-DATE
           MOVE "H"            TO CUST-AML-RISK
           MOVE "00"           TO CUST-CRA-CODE
           MOVE "OFC042"       TO CUST-REL-OFFICER
           MOVE "CB"           TO CUST-SEGMENT
           MOVE "AC"           TO CUST-STATUS
           MOVE 20240201       TO CUST-OPEN-DATE
           MOVE 20260105       TO CUST-MAINT-DATE
           MOVE "SOKONKW "     TO CUST-MAINT-USER
           MOVE SPACES         TO CUST-FILLER
           WRITE CUST-REC

           CLOSE CUSTOMER-FILE
           .

      *------------------------------------------------------------*
      * 2000 - SEED ACCOUNT MASTER FILE                             *
      *------------------------------------------------------------*
       2000-SEED-ACCOUNTS.
           OPEN OUTPUT ACCOUNT-FILE

      *--- JOHNSON DDA (checking) ---
           INITIALIZE ACCT-REC
           MOVE "AM"              TO ACCT-REC-TYPE
           MOVE "0000010000014721" TO ACCT-NUM
           MOVE "0000000001"      TO ACCT-CIF-ID
           MOVE "DDA1"            TO ACCT-PROD-CODE
           MOVE "CHECKING STANDARD   " TO ACCT-PROD-DESC
           MOVE "AC"              TO ACCT-STATUS
           MOVE 20190315          TO ACCT-OPEN-DT
           MOVE 0                 TO ACCT-CLOSE-DT
           MOVE 15234.50          TO ACCT-LEDGER-BAL
           MOVE 14734.50          TO ACCT-AVAIL-BAL
           MOVE 500.00            TO ACCT-HOLD-BAL
           MOVE 0                 TO ACCT-UNCOLL-BAL
           MOVE 500.00            TO ACCT-OD-LIMIT
           MOVE 0                 TO ACCT-CREDIT-LIMIT
           MOVE 0.000100          TO ACCT-INT-RATE
           MOVE 1.27              TO ACCT-INT-ACCRUED
           MOVE 18.44             TO ACCT-INT-YTD
           MOVE 142.30            TO ACCT-INT-PRIOR-YR
           MOVE 20260201          TO ACCT-INT-LAST-CALC
           MOVE 0                 TO ACCT-MATURITY-DT
           MOVE "CC1001"          TO ACCT-COST-CENTER
           MOVE "1001000100"      TO ACCT-GL-ACCT
           MOVE "OFC042"          TO ACCT-OFFICER
           MOVE "USD"             TO ACCT-CURRENCY
           MOVE "MO"              TO ACCT-STMT-CYCLE
           MOVE 20260131          TO ACCT-STMT-LAST-DT
           MOVE "  "              TO ACCT-RESTRICT-CD
           MOVE 00                TO ACCT-REGD-CTR
           MOVE "Y"               TO ACCT-TAX-RPTG
           MOVE "BR0001"          TO ACCT-BRANCH
           MOVE 20260201          TO ACCT-LAST-ACTIVITY-DT
           MOVE 20260115          TO ACCT-MAINT-DT
           MOVE "JMURPHY "        TO ACCT-MAINT-USER
           MOVE SPACES            TO ACCT-FILLER
           WRITE ACCT-REC

      *--- WILLIAMS SAVINGS ---
           INITIALIZE ACCT-REC
           MOVE "AM"              TO ACCT-REC-TYPE
           MOVE "0000020000028834" TO ACCT-NUM
           MOVE "0000000002"      TO ACCT-CIF-ID
           MOVE "SAV1"            TO ACCT-PROD-CODE
           MOVE "SAVINGS STANDARD    " TO ACCT-PROD-DESC
           MOVE "AC"              TO ACCT-STATUS
           MOVE 20180622          TO ACCT-OPEN-DT
           MOVE 0                 TO ACCT-CLOSE-DT
           MOVE 87562.33          TO ACCT-LEDGER-BAL
           MOVE 87562.33          TO ACCT-AVAIL-BAL
           MOVE 0                 TO ACCT-HOLD-BAL
           MOVE 0                 TO ACCT-UNCOLL-BAL
           MOVE 0                 TO ACCT-OD-LIMIT
           MOVE 0                 TO ACCT-CREDIT-LIMIT
           MOVE 4.250000          TO ACCT-INT-RATE
           MOVE 307.41            TO ACCT-INT-ACCRUED
           MOVE 3812.09           TO ACCT-INT-YTD
           MOVE 3247.81           TO ACCT-INT-PRIOR-YR
           MOVE 20260201          TO ACCT-INT-LAST-CALC
           MOVE 0                 TO ACCT-MATURITY-DT
           MOVE "CC1002"          TO ACCT-COST-CENTER
           MOVE "1002000100"      TO ACCT-GL-ACCT
           MOVE "OFC017"          TO ACCT-OFFICER
           MOVE "USD"             TO ACCT-CURRENCY
           MOVE "QT"              TO ACCT-STMT-CYCLE
           MOVE 20251231          TO ACCT-STMT-LAST-DT
           MOVE "  "              TO ACCT-RESTRICT-CD
           MOVE 02                TO ACCT-REGD-CTR
           MOVE "Y"               TO ACCT-TAX-RPTG
           MOVE "BR0001"          TO ACCT-BRANCH
           MOVE 20260115          TO ACCT-LAST-ACTIVITY-DT
           MOVE 20260110          TO ACCT-MAINT-DT
           MOVE "SOKONKW "        TO ACCT-MAINT-USER
           MOVE SPACES            TO ACCT-FILLER
           WRITE ACCT-REC

      *--- WILLIAMS MMA ---
           INITIALIZE ACCT-REC
           MOVE "AM"              TO ACCT-REC-TYPE
           MOVE "0000020000038834" TO ACCT-NUM
           MOVE "0000000002"      TO ACCT-CIF-ID
           MOVE "MMA1"            TO ACCT-PROD-CODE
           MOVE "MONEY MARKET ACCT   " TO ACCT-PROD-DESC
           MOVE "AC"              TO ACCT-STATUS
           MOVE 20200115          TO ACCT-OPEN-DT
           MOVE 0                 TO ACCT-CLOSE-DT
           MOVE 250000.00         TO ACCT-LEDGER-BAL
           MOVE 250000.00         TO ACCT-AVAIL-BAL
           MOVE 0                 TO ACCT-HOLD-BAL
           MOVE 0                 TO ACCT-UNCOLL-BAL
           MOVE 0                 TO ACCT-OD-LIMIT
           MOVE 0                 TO ACCT-CREDIT-LIMIT
           MOVE 5.100000          TO ACCT-INT-RATE
           MOVE 1054.79           TO ACCT-INT-ACCRUED
           MOVE 13050.00          TO ACCT-INT-YTD
           MOVE 10125.00          TO ACCT-INT-PRIOR-YR
           MOVE 20260201          TO ACCT-INT-LAST-CALC
           MOVE 0                 TO ACCT-MATURITY-DT
           MOVE "CC1002"          TO ACCT-COST-CENTER
           MOVE "1003000100"      TO ACCT-GL-ACCT
           MOVE "OFC017"          TO ACCT-OFFICER
           MOVE "USD"             TO ACCT-CURRENCY
           MOVE "MO"              TO ACCT-STMT-CYCLE
           MOVE 20260131          TO ACCT-STMT-LAST-DT
           MOVE "  "              TO ACCT-RESTRICT-CD
           MOVE 01                TO ACCT-REGD-CTR
           MOVE "Y"               TO ACCT-TAX-RPTG
           MOVE "BR0001"          TO ACCT-BRANCH
           MOVE 20260120          TO ACCT-LAST-ACTIVITY-DT
           MOVE 20260110          TO ACCT-MAINT-DT
           MOVE "SOKONKW "        TO ACCT-MAINT-USER
           MOVE SPACES            TO ACCT-FILLER
           WRITE ACCT-REC

      *--- CHEN DDA (checking) ---
           INITIALIZE ACCT-REC
           MOVE "AM"              TO ACCT-REC-TYPE
           MOVE "0000030000042156" TO ACCT-NUM
           MOVE "0000000003"      TO ACCT-CIF-ID
           MOVE "DDA1"            TO ACCT-PROD-CODE
           MOVE "CHECKING STANDARD   " TO ACCT-PROD-DESC
           MOVE "AC"              TO ACCT-STATUS
           MOVE 20200810          TO ACCT-OPEN-DT
           MOVE 0                 TO ACCT-CLOSE-DT
           MOVE 3421.09           TO ACCT-LEDGER-BAL
           MOVE 3421.09           TO ACCT-AVAIL-BAL
           MOVE 0                 TO ACCT-HOLD-BAL
           MOVE 0                 TO ACCT-UNCOLL-BAL
           MOVE 200.00            TO ACCT-OD-LIMIT
           MOVE 0                 TO ACCT-CREDIT-LIMIT
           MOVE 0.000100          TO ACCT-INT-RATE
           MOVE 0.03              TO ACCT-INT-ACCRUED
           MOVE 0.42              TO ACCT-INT-YTD
           MOVE 3.18              TO ACCT-INT-PRIOR-YR
           MOVE 20260201          TO ACCT-INT-LAST-CALC
           MOVE 0                 TO ACCT-MATURITY-DT
           MOVE "CC2001"          TO ACCT-COST-CENTER
           MOVE "1001000200"      TO ACCT-GL-ACCT
           MOVE "OFC042"          TO ACCT-OFFICER
           MOVE "USD"             TO ACCT-CURRENCY
           MOVE "MO"              TO ACCT-STMT-CYCLE
           MOVE 20260131          TO ACCT-STMT-LAST-DT
           MOVE "  "              TO ACCT-RESTRICT-CD
           MOVE 00                TO ACCT-REGD-CTR
           MOVE "Y"               TO ACCT-TAX-RPTG
           MOVE "BR0002"          TO ACCT-BRANCH
           MOVE 20260130          TO ACCT-LAST-ACTIVITY-DT
           MOVE 20260125          TO ACCT-MAINT-DT
           MOVE "PZHANG  "        TO ACCT-MAINT-USER
           MOVE SPACES            TO ACCT-FILLER
           WRITE ACCT-REC

      *--- MARTINEZ MORTGAGE ---
           INITIALIZE ACCT-REC
           MOVE "AM"              TO ACCT-REC-TYPE
           MOVE "0000040000056743" TO ACCT-NUM
           MOVE "0000000004"      TO ACCT-CIF-ID
           MOVE "MTG1"            TO ACCT-PROD-CODE
           MOVE "MORTGAGE 30YR FIXED " TO ACCT-PROD-DESC
           MOVE "AC"              TO ACCT-STATUS
           MOVE 20210401          TO ACCT-OPEN-DT
           MOVE 0                 TO ACCT-CLOSE-DT
           MOVE 245000.00         TO ACCT-LEDGER-BAL
           MOVE 0                 TO ACCT-AVAIL-BAL
           MOVE 0                 TO ACCT-HOLD-BAL
           MOVE 0                 TO ACCT-UNCOLL-BAL
           MOVE 0                 TO ACCT-OD-LIMIT
           MOVE 325000.00         TO ACCT-CREDIT-LIMIT
           MOVE 6.875000          TO ACCT-INT-RATE
           MOVE 1402.08           TO ACCT-INT-ACCRUED
           MOVE 16825.00          TO ACCT-INT-YTD
           MOVE 17062.50          TO ACCT-INT-PRIOR-YR
           MOVE 20260201          TO ACCT-INT-LAST-CALC
           MOVE 20510401          TO ACCT-MATURITY-DT
           MOVE "CC3001"          TO ACCT-COST-CENTER
           MOVE "1501000100"      TO ACCT-GL-ACCT
           MOVE "OFC017"          TO ACCT-OFFICER
           MOVE "USD"             TO ACCT-CURRENCY
           MOVE "MO"              TO ACCT-STMT-CYCLE
           MOVE 20260131          TO ACCT-STMT-LAST-DT
           MOVE "  "              TO ACCT-RESTRICT-CD
           MOVE 00                TO ACCT-REGD-CTR
           MOVE "Y"               TO ACCT-TAX-RPTG
           MOVE "BR0002"          TO ACCT-BRANCH
           MOVE 20260101          TO ACCT-LAST-ACTIVITY-DT
           MOVE 20260101          TO ACCT-MAINT-DT
           MOVE "KDAVIES "        TO ACCT-MAINT-USER
           MOVE SPACES            TO ACCT-FILLER
           WRITE ACCT-REC

      *--- THOMPSON DDA ---
           INITIALIZE ACCT-REC
           MOVE "AM"              TO ACCT-REC-TYPE
           MOVE "0000050000069018" TO ACCT-NUM
           MOVE "0000000005"      TO ACCT-CIF-ID
           MOVE "DDA1"            TO ACCT-PROD-CODE
           MOVE "CHECKING STANDARD   " TO ACCT-PROD-DESC
           MOVE "AC"              TO ACCT-STATUS
           MOVE 20220115          TO ACCT-OPEN-DT
           MOVE 0                 TO ACCT-CLOSE-DT
           MOVE 452.78            TO ACCT-LEDGER-BAL
           MOVE 452.78            TO ACCT-AVAIL-BAL
           MOVE 0                 TO ACCT-HOLD-BAL
           MOVE 0                 TO ACCT-UNCOLL-BAL
           MOVE 0                 TO ACCT-OD-LIMIT
           MOVE 0                 TO ACCT-CREDIT-LIMIT
           MOVE 0.000100          TO ACCT-INT-RATE
           MOVE 0                 TO ACCT-INT-ACCRUED
           MOVE 0.06              TO ACCT-INT-YTD
           MOVE 0.51              TO ACCT-INT-PRIOR-YR
           MOVE 20260201          TO ACCT-INT-LAST-CALC
           MOVE 0                 TO ACCT-MATURITY-DT
           MOVE "CC4001"          TO ACCT-COST-CENTER
           MOVE "1001000300"      TO ACCT-GL-ACCT
           MOVE "OFC098"          TO ACCT-OFFICER
           MOVE "USD"             TO ACCT-CURRENCY
           MOVE "MO"              TO ACCT-STMT-CYCLE
           MOVE 20260131          TO ACCT-STMT-LAST-DT
           MOVE "  "              TO ACCT-RESTRICT-CD
           MOVE 00                TO ACCT-REGD-CTR
           MOVE "Y"               TO ACCT-TAX-RPTG
           MOVE "BR0003"          TO ACCT-BRANCH
           MOVE 20260128          TO ACCT-LAST-ACTIVITY-DT
           MOVE 20260128          TO ACCT-MAINT-DT
           MOVE "RGUPTA  "        TO ACCT-MAINT-USER
           MOVE SPACES            TO ACCT-FILLER
           WRITE ACCT-REC

      *--- PATEL SAVINGS ---
           INITIALIZE ACCT-REC
           MOVE "AM"              TO ACCT-REC-TYPE
           MOVE "0000060000073407" TO ACCT-NUM
           MOVE "0000000006"      TO ACCT-CIF-ID
           MOVE "SAV1"            TO ACCT-PROD-CODE
           MOVE "SAVINGS STANDARD    " TO ACCT-PROD-DESC
           MOVE "AC"              TO ACCT-STATUS
           MOVE 20170903          TO ACCT-OPEN-DT
           MOVE 0                 TO ACCT-CLOSE-DT
           MOVE 156789.44         TO ACCT-LEDGER-BAL
           MOVE 156789.44         TO ACCT-AVAIL-BAL
           MOVE 0                 TO ACCT-HOLD-BAL
           MOVE 0                 TO ACCT-UNCOLL-BAL
           MOVE 0                 TO ACCT-OD-LIMIT
           MOVE 0                 TO ACCT-CREDIT-LIMIT
           MOVE 4.250000          TO ACCT-INT-RATE
           MOVE 550.63            TO ACCT-INT-ACCRUED
           MOVE 6825.12           TO ACCT-INT-YTD
           MOVE 5934.00           TO ACCT-INT-PRIOR-YR
           MOVE 20260201          TO ACCT-INT-LAST-CALC
           MOVE 0                 TO ACCT-MATURITY-DT
           MOVE "CC1002"          TO ACCT-COST-CENTER
           MOVE "1002000200"      TO ACCT-GL-ACCT
           MOVE "OFC017"          TO ACCT-OFFICER
           MOVE "USD"             TO ACCT-CURRENCY
           MOVE "MO"              TO ACCT-STMT-CYCLE
           MOVE 20260131          TO ACCT-STMT-LAST-DT
           MOVE "  "              TO ACCT-RESTRICT-CD
           MOVE 03                TO ACCT-REGD-CTR
           MOVE "Y"               TO ACCT-TAX-RPTG
           MOVE "BR0001"          TO ACCT-BRANCH
           MOVE 20260203          TO ACCT-LAST-ACTIVITY-DT
           MOVE 20260203          TO ACCT-MAINT-DT
           MOVE "SOKONKW "        TO ACCT-MAINT-USER
           MOVE SPACES            TO ACCT-FILLER
           WRITE ACCT-REC

      *--- PATEL CD ---
           INITIALIZE ACCT-REC
           MOVE "AM"              TO ACCT-REC-TYPE
           MOVE "0000060000083407" TO ACCT-NUM
           MOVE "0000000006"      TO ACCT-CIF-ID
           MOVE "CD01"            TO ACCT-PROD-CODE
           MOVE "CERT OF DEPOSIT 12M " TO ACCT-PROD-DESC
           MOVE "AC"              TO ACCT-STATUS
           MOVE 20250601          TO ACCT-OPEN-DT
           MOVE 0                 TO ACCT-CLOSE-DT
           MOVE 100000.00         TO ACCT-LEDGER-BAL
           MOVE 0                 TO ACCT-AVAIL-BAL
           MOVE 0                 TO ACCT-HOLD-BAL
           MOVE 0                 TO ACCT-UNCOLL-BAL
           MOVE 0                 TO ACCT-OD-LIMIT
           MOVE 0                 TO ACCT-CREDIT-LIMIT
           MOVE 5.350000          TO ACCT-INT-RATE
           MOVE 3567.12           TO ACCT-INT-ACCRUED
           MOVE 3567.12           TO ACCT-INT-YTD
           MOVE 0                 TO ACCT-INT-PRIOR-YR
           MOVE 20260201          TO ACCT-INT-LAST-CALC
           MOVE 20260601          TO ACCT-MATURITY-DT
           MOVE "CC1003"          TO ACCT-COST-CENTER
           MOVE "1004000100"      TO ACCT-GL-ACCT
           MOVE "OFC017"          TO ACCT-OFFICER
           MOVE "USD"             TO ACCT-CURRENCY
           MOVE "MO"              TO ACCT-STMT-CYCLE
           MOVE 20260131          TO ACCT-STMT-LAST-DT
           MOVE "ND"              TO ACCT-RESTRICT-CD
           MOVE 00                TO ACCT-REGD-CTR
           MOVE "Y"               TO ACCT-TAX-RPTG
           MOVE "BR0001"          TO ACCT-BRANCH
           MOVE 20250601          TO ACCT-LAST-ACTIVITY-DT
           MOVE 20250601          TO ACCT-MAINT-DT
           MOVE "SOKONKW "        TO ACCT-MAINT-USER
           MOVE SPACES            TO ACCT-FILLER
           WRITE ACCT-REC

      *--- OBRIEN DDA (CLOSED) ---
           INITIALIZE ACCT-REC
           MOVE "AM"              TO ACCT-REC-TYPE
           MOVE "0000070000095562" TO ACCT-NUM
           MOVE "0000000007"      TO ACCT-CIF-ID
           MOVE "DDA1"            TO ACCT-PROD-CODE
           MOVE "CHECKING STANDARD   " TO ACCT-PROD-DESC
           MOVE "CL"              TO ACCT-STATUS
           MOVE 20230718          TO ACCT-OPEN-DT
           MOVE 20251215          TO ACCT-CLOSE-DT
           MOVE 28910.66          TO ACCT-LEDGER-BAL
           MOVE 28910.66          TO ACCT-AVAIL-BAL
           MOVE 0                 TO ACCT-HOLD-BAL
           MOVE 0                 TO ACCT-UNCOLL-BAL
           MOVE 500.00            TO ACCT-OD-LIMIT
           MOVE 0                 TO ACCT-CREDIT-LIMIT
           MOVE 0.000100          TO ACCT-INT-RATE
           MOVE 0                 TO ACCT-INT-ACCRUED
           MOVE 0                 TO ACCT-INT-YTD
           MOVE 24.18             TO ACCT-INT-PRIOR-YR
           MOVE 20251215          TO ACCT-INT-LAST-CALC
           MOVE 0                 TO ACCT-MATURITY-DT
           MOVE "CC4001"          TO ACCT-COST-CENTER
           MOVE "1001000300"      TO ACCT-GL-ACCT
           MOVE "OFC098"          TO ACCT-OFFICER
           MOVE "USD"             TO ACCT-CURRENCY
           MOVE "MO"              TO ACCT-STMT-CYCLE
           MOVE 20251215          TO ACCT-STMT-LAST-DT
           MOVE "  "              TO ACCT-RESTRICT-CD
           MOVE 00                TO ACCT-REGD-CTR
           MOVE "Y"               TO ACCT-TAX-RPTG
           MOVE "BR0003"          TO ACCT-BRANCH
           MOVE 20251215          TO ACCT-LAST-ACTIVITY-DT
           MOVE 20251215          TO ACCT-MAINT-DT
           MOVE "PZHANG  "        TO ACCT-MAINT-USER
           MOVE SPACES            TO ACCT-FILLER
           WRITE ACCT-REC

      *--- NAKAMURA COMMERCIAL DDA (FROZEN) ---
           INITIALIZE ACCT-REC
           MOVE "AM"              TO ACCT-REC-TYPE
           MOVE "0000080000108901" TO ACCT-NUM
           MOVE "0000000008"      TO ACCT-CIF-ID
           MOVE "COM1"            TO ACCT-PROD-CODE
           MOVE "COMMERCIAL DDA      " TO ACCT-PROD-DESC
           MOVE "FZ"              TO ACCT-STATUS
           MOVE 20240201          TO ACCT-OPEN-DT
           MOVE 0                 TO ACCT-CLOSE-DT
           MOVE 42100.00          TO ACCT-LEDGER-BAL
           MOVE 0                 TO ACCT-AVAIL-BAL
           MOVE 42100.00          TO ACCT-HOLD-BAL
           MOVE 0                 TO ACCT-UNCOLL-BAL
           MOVE 5000.00           TO ACCT-OD-LIMIT
           MOVE 100000.00         TO ACCT-CREDIT-LIMIT
           MOVE 0.000500          TO ACCT-INT-RATE
           MOVE 1.75              TO ACCT-INT-ACCRUED
           MOVE 21.88             TO ACCT-INT-YTD
           MOVE 0                 TO ACCT-INT-PRIOR-YR
           MOVE 20260105          TO ACCT-INT-LAST-CALC
           MOVE 0                 TO ACCT-MATURITY-DT
           MOVE "CC5001"          TO ACCT-COST-CENTER
           MOVE "1006000100"      TO ACCT-GL-ACCT
           MOVE "OFC042"          TO ACCT-OFFICER
           MOVE "USD"             TO ACCT-CURRENCY
           MOVE "MO"              TO ACCT-STMT-CYCLE
           MOVE 20260105          TO ACCT-STMT-LAST-DT
           MOVE "LH"              TO ACCT-RESTRICT-CD
           MOVE 00                TO ACCT-REGD-CTR
           MOVE "Y"               TO ACCT-TAX-RPTG
           MOVE "BR0002"          TO ACCT-BRANCH
           MOVE 20260105          TO ACCT-LAST-ACTIVITY-DT
           MOVE 20260105          TO ACCT-MAINT-DT
           MOVE "SOKONKW "        TO ACCT-MAINT-USER
           MOVE SPACES            TO ACCT-FILLER
           WRITE ACCT-REC

           CLOSE ACCOUNT-FILE
           .

      *------------------------------------------------------------*
      * 3000 - SEED TRANSACTION JOURNAL                             *
      *------------------------------------------------------------*
       3000-SEED-TXNS.
           OPEN OUTPUT TRANSACTION-FILE

      *--- Johnson payroll deposit ---
           INITIALIZE TXN-REC
           MOVE "TJ"              TO TXN-REC-TYPE
           MOVE "000000000001"    TO TXN-SEQ-NUM
           MOVE "0000010000014721" TO TXN-ACCT-NUM
           MOVE "0000000001"      TO TXN-CIF-ID
           MOVE 20260120          TO TXN-EFFDT
           MOVE 20260120          TO TXN-POST-DT
           MOVE 091523            TO TXN-POST-TIME
           MOVE "DEP"             TO TXN-TYPE-CD
           MOVE "C"               TO TXN-DR-CR
           MOVE 5000.00           TO TXN-AMT
           MOVE 18234.50          TO TXN-RUN-BAL
           MOVE "ACH"             TO TXN-CHANNEL
           MOVE SPACES            TO TXN-TELLER-ID
           MOVE "ACHGW001"        TO TXN-TERMINAL-ID
           MOVE SPACES            TO TXN-AUTH-CODE
           MOVE "PPD 091523CORP01    " TO TXN-REF-NUM
           MOVE "PAYROLL DIRECT DEPOSIT - ACME CORP"
                                  TO TXN-DESC
           MOVE "20260120"        TO TXN-BATCH-NUM
           MOVE "0001"            TO TXN-CYCLE-NUM
           MOVE "1001000100"      TO TXN-GL-DR-ACCT
           MOVE "2001000100"      TO TXN-GL-CR-ACCT
           MOVE " "               TO TXN-REV-FLAG
           MOVE SPACES            TO TXN-REV-ORIG-SEQ
           MOVE "P"               TO TXN-STATUS
           MOVE SPACES            TO TXN-FILLER
           WRITE TXN-REC

      *--- Johnson ATM withdrawal ---
           INITIALIZE TXN-REC
           MOVE "TJ"              TO TXN-REC-TYPE
           MOVE "000000000002"    TO TXN-SEQ-NUM
           MOVE "0000010000014721" TO TXN-ACCT-NUM
           MOVE "0000000001"      TO TXN-CIF-ID
           MOVE 20260121          TO TXN-EFFDT
           MOVE 20260121          TO TXN-POST-DT
           MOVE 143022            TO TXN-POST-TIME
           MOVE "WDL"             TO TXN-TYPE-CD
           MOVE "D"               TO TXN-DR-CR
           MOVE 1200.00           TO TXN-AMT
           MOVE 17034.50          TO TXN-RUN-BAL
           MOVE "ATM"             TO TXN-CHANNEL
           MOVE SPACES            TO TXN-TELLER-ID
           MOVE "ATM04721"        TO TXN-TERMINAL-ID
           MOVE "A14302"          TO TXN-AUTH-CODE
           MOVE SPACES            TO TXN-REF-NUM
           MOVE "ATM WITHDRAWAL - 3RD AVE & 86TH"
                                  TO TXN-DESC
           MOVE "20260121"        TO TXN-BATCH-NUM
           MOVE "0001"            TO TXN-CYCLE-NUM
           MOVE "2001000100"      TO TXN-GL-DR-ACCT
           MOVE "1001000100"      TO TXN-GL-CR-ACCT
           MOVE " "               TO TXN-REV-FLAG
           MOVE SPACES            TO TXN-REV-ORIG-SEQ
           MOVE "P"               TO TXN-STATUS
           MOVE SPACES            TO TXN-FILLER
           WRITE TXN-REC

      *--- Williams wire in ---
           INITIALIZE TXN-REC
           MOVE "TJ"              TO TXN-REC-TYPE
           MOVE "000000000003"    TO TXN-SEQ-NUM
           MOVE "0000020000028834" TO TXN-ACCT-NUM
           MOVE "0000000002"      TO TXN-CIF-ID
           MOVE 20260115          TO TXN-EFFDT
           MOVE 20260115          TO TXN-POST-DT
           MOVE 100045            TO TXN-POST-TIME
           MOVE "DEP"             TO TXN-TYPE-CD
           MOVE "C"               TO TXN-DR-CR
           MOVE 12500.00          TO TXN-AMT
           MOVE 97562.33          TO TXN-RUN-BAL
           MOVE "WIR"             TO TXN-CHANNEL
           MOVE "TLR042"          TO TXN-TELLER-ID
           MOVE "WIREGATE"        TO TXN-TERMINAL-ID
           MOVE "W10004"          TO TXN-AUTH-CODE
           MOVE "FW021000089-00147" TO TXN-REF-NUM
           MOVE "INCOMING WIRE - MORGAN STANLEY"
                                  TO TXN-DESC
           MOVE "20260115"        TO TXN-BATCH-NUM
           MOVE "0001"            TO TXN-CYCLE-NUM
           MOVE "1002000100"      TO TXN-GL-DR-ACCT
           MOVE "2002000100"      TO TXN-GL-CR-ACCT
           MOVE " "               TO TXN-REV-FLAG
           MOVE SPACES            TO TXN-REV-ORIG-SEQ
           MOVE "P"               TO TXN-STATUS
           MOVE SPACES            TO TXN-FILLER
           WRITE TXN-REC

      *--- Chen check withdrawal ---
           INITIALIZE TXN-REC
           MOVE "TJ"              TO TXN-REC-TYPE
           MOVE "000000000004"    TO TXN-SEQ-NUM
           MOVE "0000030000042156" TO TXN-ACCT-NUM
           MOVE "0000000003"      TO TXN-CIF-ID
           MOVE 20260125          TO TXN-EFFDT
           MOVE 20260126          TO TXN-POST-DT
           MOVE 112233            TO TXN-POST-TIME
           MOVE "CKP"             TO TXN-TYPE-CD
           MOVE "D"               TO TXN-DR-CR
           MOVE 500.00            TO TXN-AMT
           MOVE 3421.09           TO TXN-RUN-BAL
           MOVE "BRN"             TO TXN-CHANNEL
           MOVE "TLR098"          TO TXN-TELLER-ID
           MOVE "TERM0002"        TO TXN-TERMINAL-ID
           MOVE SPACES            TO TXN-AUTH-CODE
           MOVE "CHK#001042          " TO TXN-REF-NUM
           MOVE "CHECK PRESENTED - LANDLORD MGMT CO"
                                  TO TXN-DESC
           MOVE "20260126"        TO TXN-BATCH-NUM
           MOVE "0001"            TO TXN-CYCLE-NUM
           MOVE "2001000200"      TO TXN-GL-DR-ACCT
           MOVE "1001000200"      TO TXN-GL-CR-ACCT
           MOVE " "               TO TXN-REV-FLAG
           MOVE SPACES            TO TXN-REV-ORIG-SEQ
           MOVE "P"               TO TXN-STATUS
           MOVE SPACES            TO TXN-FILLER
           WRITE TXN-REC

      *--- Martinez mortgage payment ---
           INITIALIZE TXN-REC
           MOVE "TJ"              TO TXN-REC-TYPE
           MOVE "000000000005"    TO TXN-SEQ-NUM
           MOVE "0000040000056743" TO TXN-ACCT-NUM
           MOVE "0000000004"      TO TXN-CIF-ID
           MOVE 20260101          TO TXN-EFFDT
           MOVE 20260102          TO TXN-POST-DT
           MOVE 080000            TO TXN-POST-TIME
           MOVE "DEP"             TO TXN-TYPE-CD
           MOVE "C"               TO TXN-DR-CR
           MOVE 2450.00           TO TXN-AMT
           MOVE 245000.00         TO TXN-RUN-BAL
           MOVE "ACH"             TO TXN-CHANNEL
           MOVE SPACES            TO TXN-TELLER-ID
           MOVE "ACHGW001"        TO TXN-TERMINAL-ID
           MOVE SPACES            TO TXN-AUTH-CODE
           MOVE "PPD MTG-PMT 202601 " TO TXN-REF-NUM
           MOVE "MONTHLY MORTGAGE PAYMENT"
                                  TO TXN-DESC
           MOVE "20260102"        TO TXN-BATCH-NUM
           MOVE "0001"            TO TXN-CYCLE-NUM
           MOVE "1501000100"      TO TXN-GL-DR-ACCT
           MOVE "2001000300"      TO TXN-GL-CR-ACCT
           MOVE " "               TO TXN-REV-FLAG
           MOVE SPACES            TO TXN-REV-ORIG-SEQ
           MOVE "P"               TO TXN-STATUS
           MOVE SPACES            TO TXN-FILLER
           WRITE TXN-REC

      *--- Thompson payroll deposit ---
           INITIALIZE TXN-REC
           MOVE "TJ"              TO TXN-REC-TYPE
           MOVE "000000000006"    TO TXN-SEQ-NUM
           MOVE "0000050000069018" TO TXN-ACCT-NUM
           MOVE "0000000005"      TO TXN-CIF-ID
           MOVE 20260128          TO TXN-EFFDT
           MOVE 20260128          TO TXN-POST-DT
           MOVE 090000            TO TXN-POST-TIME
           MOVE "DEP"             TO TXN-TYPE-CD
           MOVE "C"               TO TXN-DR-CR
           MOVE 3200.00           TO TXN-AMT
           MOVE 3252.78           TO TXN-RUN-BAL
           MOVE "ACH"             TO TXN-CHANNEL
           MOVE SPACES            TO TXN-TELLER-ID
           MOVE "ACHGW001"        TO TXN-TERMINAL-ID
           MOVE SPACES            TO TXN-AUTH-CODE
           MOVE "PPD 090000EMPL42   " TO TXN-REF-NUM
           MOVE "PAYROLL DIRECT DEPOSIT - SMITH INC"
                                  TO TXN-DESC
           MOVE "20260128"        TO TXN-BATCH-NUM
           MOVE "0001"            TO TXN-CYCLE-NUM
           MOVE "1001000300"      TO TXN-GL-DR-ACCT
           MOVE "2001000300"      TO TXN-GL-CR-ACCT
           MOVE " "               TO TXN-REV-FLAG
           MOVE SPACES            TO TXN-REV-ORIG-SEQ
           MOVE "P"               TO TXN-STATUS
           MOVE SPACES            TO TXN-FILLER
           WRITE TXN-REC

      *--- Thompson rent payment ---
           INITIALIZE TXN-REC
           MOVE "TJ"              TO TXN-REC-TYPE
           MOVE "000000000007"    TO TXN-SEQ-NUM
           MOVE "0000050000069018" TO TXN-ACCT-NUM
           MOVE "0000000005"      TO TXN-CIF-ID
           MOVE 20260128          TO TXN-EFFDT
           MOVE 20260128          TO TXN-POST-DT
           MOVE 141500            TO TXN-POST-TIME
           MOVE "WDL"             TO TXN-TYPE-CD
           MOVE "D"               TO TXN-DR-CR
           MOVE 2800.00           TO TXN-AMT
           MOVE 452.78            TO TXN-RUN-BAL
           MOVE "ONL"             TO TXN-CHANNEL
           MOVE SPACES            TO TXN-TELLER-ID
           MOVE "ONLBNK01"        TO TXN-TERMINAL-ID
           MOVE "B14150"          TO TXN-AUTH-CODE
           MOVE "BP-20260128-RENT   " TO TXN-REF-NUM
           MOVE "ONLINE BILL PAY - BRONX REALTY LLC"
                                  TO TXN-DESC
           MOVE "20260128"        TO TXN-BATCH-NUM
           MOVE "0001"            TO TXN-CYCLE-NUM
           MOVE "2001000300"      TO TXN-GL-DR-ACCT
           MOVE "1001000300"      TO TXN-GL-CR-ACCT
           MOVE " "               TO TXN-REV-FLAG
           MOVE SPACES            TO TXN-REV-ORIG-SEQ
           MOVE "P"               TO TXN-STATUS
           MOVE SPACES            TO TXN-FILLER
           WRITE TXN-REC

      *--- Patel transfer (XDR) ---
           INITIALIZE TXN-REC
           MOVE "TJ"              TO TXN-REC-TYPE
           MOVE "000000000008"    TO TXN-SEQ-NUM
           MOVE "0000060000073407" TO TXN-ACCT-NUM
           MOVE "0000000006"      TO TXN-CIF-ID
           MOVE 20260203          TO TXN-EFFDT
           MOVE 20260203          TO TXN-POST-DT
           MOVE 160030            TO TXN-POST-TIME
           MOVE "XDR"             TO TXN-TYPE-CD
           MOVE "D"               TO TXN-DR-CR
           MOVE 10000.00          TO TXN-AMT
           MOVE 156789.44         TO TXN-RUN-BAL
           MOVE "MOB"             TO TXN-CHANNEL
           MOVE SPACES            TO TXN-TELLER-ID
           MOVE "MOBAPP01"        TO TXN-TERMINAL-ID
           MOVE "M16003"          TO TXN-AUTH-CODE
           MOVE "XFER-20260203-001  " TO TXN-REF-NUM
           MOVE "XFER TO 0000020000028834"
                                  TO TXN-DESC
           MOVE "20260203"        TO TXN-BATCH-NUM
           MOVE "0001"            TO TXN-CYCLE-NUM
           MOVE "2002000200"      TO TXN-GL-DR-ACCT
           MOVE "1002000100"      TO TXN-GL-CR-ACCT
           MOVE " "               TO TXN-REV-FLAG
           MOVE SPACES            TO TXN-REV-ORIG-SEQ
           MOVE "P"               TO TXN-STATUS
           MOVE SPACES            TO TXN-FILLER
           WRITE TXN-REC

      *--- Patel transfer (XCR - contra) ---
           INITIALIZE TXN-REC
           MOVE "TJ"              TO TXN-REC-TYPE
           MOVE "000000000009"    TO TXN-SEQ-NUM
           MOVE "0000020000028834" TO TXN-ACCT-NUM
           MOVE "0000000002"      TO TXN-CIF-ID
           MOVE 20260203          TO TXN-EFFDT
           MOVE 20260203          TO TXN-POST-DT
           MOVE 160030            TO TXN-POST-TIME
           MOVE "XCR"             TO TXN-TYPE-CD
           MOVE "C"               TO TXN-DR-CR
           MOVE 10000.00          TO TXN-AMT
           MOVE 97562.33          TO TXN-RUN-BAL
           MOVE "INT"             TO TXN-CHANNEL
           MOVE "SYSTEM"          TO TXN-TELLER-ID
           MOVE "INTERNAL"        TO TXN-TERMINAL-ID
           MOVE "M16003"          TO TXN-AUTH-CODE
           MOVE "XFER-20260203-001  " TO TXN-REF-NUM
           MOVE "XFER FR 0000060000073407"
                                  TO TXN-DESC
           MOVE "20260203"        TO TXN-BATCH-NUM
           MOVE "0001"            TO TXN-CYCLE-NUM
           MOVE "1002000100"      TO TXN-GL-DR-ACCT
           MOVE "2002000200"      TO TXN-GL-CR-ACCT
           MOVE " "               TO TXN-REV-FLAG
           MOVE SPACES            TO TXN-REV-ORIG-SEQ
           MOVE "P"               TO TXN-STATUS
           MOVE SPACES            TO TXN-FILLER
           WRITE TXN-REC

      *--- Johnson monthly service fee ---
           INITIALIZE TXN-REC
           MOVE "TJ"              TO TXN-REC-TYPE
           MOVE "000000000010"    TO TXN-SEQ-NUM
           MOVE "0000010000014721" TO TXN-ACCT-NUM
           MOVE "0000000001"      TO TXN-CIF-ID
           MOVE 20260201          TO TXN-EFFDT
           MOVE 20260201          TO TXN-POST-DT
           MOVE 000100            TO TXN-POST-TIME
           MOVE "SVC"             TO TXN-TYPE-CD
           MOVE "D"               TO TXN-DR-CR
           MOVE 12.50             TO TXN-AMT
           MOVE 15234.50          TO TXN-RUN-BAL
           MOVE "BAT"             TO TXN-CHANNEL
           MOVE "SYSTEM"          TO TXN-TELLER-ID
           MOVE "BATCHSVC"        TO TXN-TERMINAL-ID
           MOVE SPACES            TO TXN-AUTH-CODE
           MOVE "SVC-FEE-202602     " TO TXN-REF-NUM
           MOVE "MONTHLY SERVICE CHARGE - DDA"
                                  TO TXN-DESC
           MOVE "20260201"        TO TXN-BATCH-NUM
           MOVE "0099"            TO TXN-CYCLE-NUM
           MOVE "2001000100"      TO TXN-GL-DR-ACCT
           MOVE "4001000100"      TO TXN-GL-CR-ACCT
           MOVE " "               TO TXN-REV-FLAG
           MOVE SPACES            TO TXN-REV-ORIG-SEQ
           MOVE "P"               TO TXN-STATUS
           MOVE SPACES            TO TXN-FILLER
           WRITE TXN-REC

      *--- Chen mobile deposit ---
           INITIALIZE TXN-REC
           MOVE "TJ"              TO TXN-REC-TYPE
           MOVE "000000000011"    TO TXN-SEQ-NUM
           MOVE "0000030000042156" TO TXN-ACCT-NUM
           MOVE "0000000003"      TO TXN-CIF-ID
           MOVE 20260130          TO TXN-EFFDT
           MOVE 20260131          TO TXN-POST-DT
           MOVE 113000            TO TXN-POST-TIME
           MOVE "CKD"             TO TXN-TYPE-CD
           MOVE "C"               TO TXN-DR-CR
           MOVE 750.00            TO TXN-AMT
           MOVE 3671.09           TO TXN-RUN-BAL
           MOVE "MOB"             TO TXN-CHANNEL
           MOVE SPACES            TO TXN-TELLER-ID
           MOVE "MOBAPP01"        TO TXN-TERMINAL-ID
           MOVE "M11300"          TO TXN-AUTH-CODE
           MOVE "MDEP-20260130-4721 " TO TXN-REF-NUM
           MOVE "MOBILE CHECK DEPOSIT - FREELANCE"
                                  TO TXN-DESC
           MOVE "20260131"        TO TXN-BATCH-NUM
           MOVE "0001"            TO TXN-CYCLE-NUM
           MOVE "1001000200"      TO TXN-GL-DR-ACCT
           MOVE "2001000200"      TO TXN-GL-CR-ACCT
           MOVE " "               TO TXN-REV-FLAG
           MOVE SPACES            TO TXN-REV-ORIG-SEQ
           MOVE "H"               TO TXN-STATUS
           MOVE SPACES            TO TXN-FILLER
           WRITE TXN-REC

      *--- Patel CD interest accrual ---
           INITIALIZE TXN-REC
           MOVE "TJ"              TO TXN-REC-TYPE
           MOVE "000000000012"    TO TXN-SEQ-NUM
           MOVE "0000060000083407" TO TXN-ACCT-NUM
           MOVE "0000000006"      TO TXN-CIF-ID
           MOVE 20260201          TO TXN-EFFDT
           MOVE 20260201          TO TXN-POST-DT
           MOVE 000500            TO TXN-POST-TIME
           MOVE "ACR"             TO TXN-TYPE-CD
           MOVE "C"               TO TXN-DR-CR
           MOVE 445.83            TO TXN-AMT
           MOVE 100445.83         TO TXN-RUN-BAL
           MOVE "BAT"             TO TXN-CHANNEL
           MOVE "SYSTEM"          TO TXN-TELLER-ID
           MOVE "BATCHINT"        TO TXN-TERMINAL-ID
           MOVE SPACES            TO TXN-AUTH-CODE
           MOVE "INT-CD01-20260201  " TO TXN-REF-NUM
           MOVE "INTEREST ACCRUAL - CD 5.35% APY"
                                  TO TXN-DESC
           MOVE "20260201"        TO TXN-BATCH-NUM
           MOVE "0099"            TO TXN-CYCLE-NUM
           MOVE "5001000100"      TO TXN-GL-DR-ACCT
           MOVE "1004000100"      TO TXN-GL-CR-ACCT
           MOVE " "               TO TXN-REV-FLAG
           MOVE SPACES            TO TXN-REV-ORIG-SEQ
           MOVE "P"               TO TXN-STATUS
           MOVE SPACES            TO TXN-FILLER
           WRITE TXN-REC

      *--- Williams wire out (pending) ---
           INITIALIZE TXN-REC
           MOVE "TJ"              TO TXN-REC-TYPE
           MOVE "000000000013"    TO TXN-SEQ-NUM
           MOVE "0000020000028834" TO TXN-ACCT-NUM
           MOVE "0000000002"      TO TXN-CIF-ID
           MOVE 20260205          TO TXN-EFFDT
           MOVE 20260205          TO TXN-POST-DT
           MOVE 154500            TO TXN-POST-TIME
           MOVE "WFE"             TO TXN-TYPE-CD
           MOVE "D"               TO TXN-DR-CR
           MOVE 25.00             TO TXN-AMT
           MOVE 87537.33          TO TXN-RUN-BAL
           MOVE "WIR"             TO TXN-CHANNEL
           MOVE "TLR042"          TO TXN-TELLER-ID
           MOVE "WIREGATE"        TO TXN-TERMINAL-ID
           MOVE SPACES            TO TXN-AUTH-CODE
           MOVE "WF-20260205-SAV1   " TO TXN-REF-NUM
           MOVE "OUTGOING WIRE FEE"
                                  TO TXN-DESC
           MOVE "20260205"        TO TXN-BATCH-NUM
           MOVE "0001"            TO TXN-CYCLE-NUM
           MOVE "2002000100"      TO TXN-GL-DR-ACCT
           MOVE "4002000100"      TO TXN-GL-CR-ACCT
           MOVE " "               TO TXN-REV-FLAG
           MOVE SPACES            TO TXN-REV-ORIG-SEQ
           MOVE "P"               TO TXN-STATUS
           MOVE SPACES            TO TXN-FILLER
           WRITE TXN-REC

      *--- Williams wire out (pending hold) ---
           INITIALIZE TXN-REC
           MOVE "TJ"              TO TXN-REC-TYPE
           MOVE "000000000014"    TO TXN-SEQ-NUM
           MOVE "0000020000028834" TO TXN-ACCT-NUM
           MOVE "0000000002"      TO TXN-CIF-ID
           MOVE 20260205          TO TXN-EFFDT
           MOVE 20260205          TO TXN-POST-DT
           MOVE 154500            TO TXN-POST-TIME
           MOVE "WDL"             TO TXN-TYPE-CD
           MOVE "D"               TO TXN-DR-CR
           MOVE 5000.00           TO TXN-AMT
           MOVE 82537.33          TO TXN-RUN-BAL
           MOVE "WIR"             TO TXN-CHANNEL
           MOVE "TLR042"          TO TXN-TELLER-ID
           MOVE "WIREGATE"        TO TXN-TERMINAL-ID
           MOVE "W20520"          TO TXN-AUTH-CODE
           MOVE "FW026009593-00289  " TO TXN-REF-NUM
           MOVE "OUTGOING WIRE - SCHWAB BROKERAGE"
                                  TO TXN-DESC
           MOVE "20260205"        TO TXN-BATCH-NUM
           MOVE "0001"            TO TXN-CYCLE-NUM
           MOVE "2002000100"      TO TXN-GL-DR-ACCT
           MOVE "1002000100"      TO TXN-GL-CR-ACCT
           MOVE " "               TO TXN-REV-FLAG
           MOVE SPACES            TO TXN-REV-ORIG-SEQ
           MOVE "H"               TO TXN-STATUS
           MOVE SPACES            TO TXN-FILLER
           WRITE TXN-REC

      *--- Johnson overdraft fee (reversed) ---
           INITIALIZE TXN-REC
           MOVE "TJ"              TO TXN-REC-TYPE
           MOVE "000000000015"    TO TXN-SEQ-NUM
           MOVE "0000010000014721" TO TXN-ACCT-NUM
           MOVE "0000000001"      TO TXN-CIF-ID
           MOVE 20260115          TO TXN-EFFDT
           MOVE 20260115          TO TXN-POST-DT
           MOVE 235900            TO TXN-POST-TIME
           MOVE "ODF"             TO TXN-TYPE-CD
           MOVE "D"               TO TXN-DR-CR
           MOVE 35.00             TO TXN-AMT
           MOVE 15199.50          TO TXN-RUN-BAL
           MOVE "BAT"             TO TXN-CHANNEL
           MOVE "SYSTEM"          TO TXN-TELLER-ID
           MOVE "BATCHODF"        TO TXN-TERMINAL-ID
           MOVE SPACES            TO TXN-AUTH-CODE
           MOVE "ODF-20260115-001   " TO TXN-REF-NUM
           MOVE "NSF/OVERDRAFT FEE - REVERSED"
                                  TO TXN-DESC
           MOVE "20260115"        TO TXN-BATCH-NUM
           MOVE "0099"            TO TXN-CYCLE-NUM
           MOVE "2001000100"      TO TXN-GL-DR-ACCT
           MOVE "4003000100"      TO TXN-GL-CR-ACCT
           MOVE "V"               TO TXN-REV-FLAG
           MOVE SPACES            TO TXN-REV-ORIG-SEQ
           MOVE "R"               TO TXN-STATUS
           MOVE SPACES            TO TXN-FILLER
           WRITE TXN-REC

           CLOSE TRANSACTION-FILE
           .
