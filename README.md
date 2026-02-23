# COBOL Bank Demo — Legacy Modernization Showcase

A production-representative COBOL batch/online banking system with realistic flat-file schemas modeled after enterprise core banking platforms. Built for demoing legacy modernization scenarios to banks.

## What It Demonstrates

| Legacy Pattern | Where It Appears |
|---|---|
| Separate CIF / Account master design | `CUSTMSTR.dat` (customer) vs `ACCTMSTR.dat` (account) with FK relationship |
| Multi-balance accounting | Ledger, available, hold, and uncollected float balances on every account |
| Product-code-driven account types | `DDA1`, `DDA2`, `SAV1`, `MMA1`, `CD01`, `HEL1`, `MTG1`, `COM1` with 88-levels |
| 2-char lifecycle status codes | `AC`/`DO`/`CL`/`FZ`/`CO`/`ES` (active, dormant, closed, frozen, charged-off, escheat) |
| Effective date vs posting date | Transaction journal carries both `TXN-EFFDT` and `TXN-POST-DT` |
| Dual-sided GL posting | Every transaction carries `TXN-GL-DR-ACCT` and `TXN-GL-CR-ACCT` |
| Channel / teller / terminal tracking | `BRN`/`ATM`/`ONL`/`MOB`/`WIR`/`ACH`/`BAT`/`INT` channel codes |
| Paired transfer entries | Transfers create both `XDR` (debit) and `XCR` (credit) journal records |
| KYC/AML/CRA compliance fields | On CIF: KYC status + date, AML risk rating, CRA code |
| Interest accrual infrastructure | Rate, accrued, YTD, prior-year (1099), last-calc date per account |
| Restriction codes | `ND` (no-debit), `NC` (no-credit), `LH` (legal hold), `GR` (garnishment) |
| Reg D withdrawal counter | Savings/MMA accounts track withdrawal count |
| Record-type indicators | `CI` (customer), `AM` (account), `TJ` (transaction) at position 1 |
| Copybook-style data layouts | Hierarchical `05`/`10` level structures with group items |
| Reversal flag + original sequence | Transaction journal supports `R` (reversal) and `V` (has-reversal) flags |
| Temp-file swap pattern | Update-in-place via sequential read → temp → swap (no indexed I/O) |
| Maintenance audit trail | `MAINT-DT` + `MAINT-USER` on every master record |
| Batch fee processing | Monthly service fee sweep across all DDA/commercial accounts |
| Report generation with page breaks | Paginated account summary report → `RPTFILE.txt` |

## Prerequisites

```
brew install gnucobol
```

## Quick Start

```bash
cd cobol-bank-demo
make run        # compile, seed sample data, launch interactive menu
```

## Commands

| Command | Description |
|---|---|
| `make build` | Compile both programs |
| `make seed` | Build + populate sample data (8 customers, 10 accounts, 15 transactions) |
| `make run` | Build + seed + launch interactive banking system |
| `make test` | Clean build + run 53 automated tests |
| `make clean` | Remove all binaries and data files |

## Project Structure

```
BANKMAIN.cbl   — Core banking application (~1770 lines)
SEEDDATA.cbl   — Sample data generator (~1270 lines)
test.sh        — Automated test suite (53 assertions)
Makefile       — Build, seed, run, test, clean targets
```

## Data Files

### CUSTMSTR.dat — Customer Information File (CIF), 256 bytes/record

```
Pos  Len  Field                Description
1    2    CUST-REC-TYPE        Record type ("CI")
3    10   CUST-CIF-ID          Customer ID
13   1    CUST-TAX-ID-TYPE     S=SSN, E=EIN, I=ITIN
14   11   CUST-TAX-ID          Masked tax ID (***-**-XXXX)
25   25   CUST-LAST-NAME       Last name / business name
50   20   CUST-FIRST-NAME      First name
70   1    CUST-MI              Middle initial
71   4    CUST-SUFFIX          Jr/Sr/III/etc
75   30   CUST-ADDR-LINE1      Street address
105  30   CUST-ADDR-LINE2      Suite/apt
135  20   CUST-CITY            City
155  2    CUST-STATE           State
157  5    CUST-ZIP             ZIP code
162  4    CUST-ZIP4            ZIP+4
166  3    CUST-COUNTRY         Country code
169  12   CUST-PHONE-PRIMARY   Primary phone
181  12   CUST-PHONE-ALT       Alternate phone
193  40   CUST-EMAIL           Email address
233  1    CUST-KYC-STATUS      V=Verified, P=Pending, X=Expired
234  8    CUST-KYC-DATE        KYC verification date
242  1    CUST-AML-RISK        L=Low, M=Medium, H=High, P=Prohibited
243  2    CUST-CRA-CODE        CRA assessment area code
245  6    CUST-REL-OFFICER     Relationship officer ID
251  2    CUST-SEGMENT         RT/PB/WM/CB/CP
253  2    CUST-STATUS          AC=Active, IN=Inactive, DC=Deceased
255  8    CUST-OPEN-DATE       Relationship open date
263  8    CUST-MAINT-DATE      Last maintenance date
271  8    CUST-MAINT-USER      Last maintenance user ID
279  4    CUST-FILLER          Reserved
```

### ACCTMSTR.dat — Account Master, 300 bytes/record

```
Pos  Len  Field                Description
1    2    ACCT-REC-TYPE        Record type ("AM")
3    16   ACCT-NUM             Account number (with embedded check digit)
19   10   ACCT-CIF-ID          FK → CUSTMSTR
29   4    ACCT-PROD-CODE       DDA1/DDA2/SAV1/MMA1/CD01/HEL1/MTG1/COM1
33   20   ACCT-PROD-DESC       Product description
53   2    ACCT-STATUS          AC/DO/CL/FZ/CO/ES
55   8    ACCT-OPEN-DT         Account open date
63   8    ACCT-CLOSE-DT        Account close date (0 if open)
71   13   ACCT-LEDGER-BAL      Ledger balance (S9(11)V99)
84   13   ACCT-AVAIL-BAL       Available balance
97   13   ACCT-HOLD-BAL        Hold amount
110  13   ACCT-UNCOLL-BAL      Uncollected float
123  11   ACCT-OD-LIMIT        Overdraft limit
134  13   ACCT-CREDIT-LIMIT    Credit/loan limit
147  9    ACCT-INT-RATE         Interest rate (S9(3)V9(6))
156  11   ACCT-INT-ACCRUED     Accrued interest
167  11   ACCT-INT-YTD         Year-to-date interest
178  11   ACCT-INT-PRIOR-YR    Prior year interest (1099)
189  8    ACCT-INT-LAST-CALC   Last interest calculation date
197  8    ACCT-MATURITY-DT     Maturity date (CDs/loans)
205  6    ACCT-COST-CENTER     Cost center
211  10   ACCT-GL-ACCT         General ledger account
221  6    ACCT-OFFICER         Account officer
227  3    ACCT-CURRENCY        Currency code (USD)
230  2    ACCT-STMT-CYCLE      Statement cycle (MO/QT/AN)
232  8    ACCT-STMT-LAST-DT    Last statement date
240  2    ACCT-RESTRICT-CD     ND/NC/LH/GR (restriction)
242  2    ACCT-REGD-CTR        Reg D withdrawal counter
244  1    ACCT-TAX-RPTG        Y/N/E (tax reporting)
245  6    ACCT-BRANCH          Branch code
251  8    ACCT-LAST-ACTIVITY   Last activity date
259  8    ACCT-MAINT-DT        Maintenance date
267  8    ACCT-MAINT-USER      Maintenance user ID
275  20   ACCT-FILLER          Reserved
```

### TXNJRNL.dat — Transaction Journal, 256 bytes/record

```
Pos  Len  Field                Description
1    2    TXN-REC-TYPE         Record type ("TJ")
3    12   TXN-SEQ-NUM          Journal sequence number
15   16   TXN-ACCT-NUM         Account number
31   10   TXN-CIF-ID           Customer ID
41   8    TXN-EFFDT            Effective date
49   8    TXN-POST-DT          Posting date
57   6    TXN-POST-TIME        Posting time (HHMMSS)
63   3    TXN-TYPE-CD          DEP/WDL/XDR/XCR/SVC/INT/ICH/ODF/WFE/ACR/ADR/CKD/CKP
66   1    TXN-DR-CR            D=Debit, C=Credit
67   13   TXN-AMT              Transaction amount
80   13   TXN-RUN-BAL          Running balance after transaction
93   3    TXN-CHANNEL          BRN/ATM/ONL/MOB/WIR/ACH/BAT/INT
96   6    TXN-TELLER-ID        Teller/operator ID
102  8    TXN-TERMINAL-ID      Terminal/device ID
110  6    TXN-AUTH-CODE         Authorization code
116  20   TXN-REF-NUM          Reference (check#, wire ref, ACH trace)
136  40   TXN-DESC             Description
176  8    TXN-BATCH-NUM        Batch number
184  4    TXN-CYCLE-NUM        Processing cycle
188  10   TXN-GL-DR-ACCT       GL debit account
198  10   TXN-GL-CR-ACCT       GL credit account
208  1    TXN-REV-FLAG         (space)=Normal, R=Reversal, V=Has-reversal
209  12   TXN-REV-ORIG-SEQ     Original sequence (if reversal)
221  1    TXN-STATUS           P=Posted, H=Hold, R=Reversed, X=Rejected
222  13   TXN-FILLER           Reserved
```

## Sample Data

- **8 customers** across segments: Retail, Private Banking, Wealth Management, Commercial
- **10 accounts**: 4 DDA (checking), 2 SAV (savings), 1 MMA, 1 CD, 1 MTG (mortgage), 1 COM (commercial)
- **15 transactions**: ACH payroll, ATM withdrawal, wire transfer, check presentment, mobile deposit, interest accrual, service fees, overdraft fee (reversed), paired transfer entries

Includes accounts in various states: active, closed, frozen with legal hold; and transactions in various states: posted, pending hold, reversed.

## Demo Walkthrough

1. `make run` — launches the mainframe-style menu system
2. Press `4` → inquiry on `0000010000014721` (Johnson's checking — shows full account detail + CIF with KYC/AML)
3. Press `2` → `1` → deposit $5,000 into Johnson's checking
4. Press `2` → `3` → transfer $1,000 from Johnson to Williams (creates paired XDR/XCR journal entries)
5. Press `2` → `4` → run monthly service fee batch ($12.50 per DDA/commercial account)
6. Press `3` → `1` → generate account summary report, open `RPTFILE.txt`
7. Press `3` → `2` → generate transaction journal, open `RPTFILE.txt` (shows channels, GL codes, D/C indicators)
8. Press `1` → `4` → list all accounts (shows product codes, dual balances, restriction codes)
9. Press `0` to exit

Then run `make test` to show the 53 automated tests passing.
# cobol-bank-demo
