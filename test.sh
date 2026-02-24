#!/bin/bash
set -e

cd "$(dirname "$0")"

echo "========================================"
echo "  COBOL Bank Demo - Automated Test Suite"
echo "========================================"
echo ""

PASS=0
FAIL=0
TMPOUT=$(mktemp)
trap "rm -f $TMPOUT" EXIT

check_file() {
    local desc="$1"
    local pattern="$2"
    local file="$3"
    if grep -q "$pattern" "$file" 2>/dev/null; then
        echo "  [PASS] $desc"
        PASS=$((PASS + 1))
    else
        echo "  [FAIL] $desc"
        FAIL=$((FAIL + 1))
    fi
}

check_cond() {
    local desc="$1"
    shift
    if "$@" 2>/dev/null; then
        echo "  [PASS] $desc"
        PASS=$((PASS + 1))
    else
        echo "  [FAIL] $desc"
        FAIL=$((FAIL + 1))
    fi
}

run_bank() {
    printf '%s\n' "$@" | ./bankmain > "$TMPOUT" 2>&1 || true
}

ACCT_JOHNSON="0000010000014721"
ACCT_WILLIAMS_SAV="0000020000028834"
ACCT_CHEN="0000030000042156"
ACCT_THOMPSON="0000050000069018"

rm -f CUSTMSTR.dat ACCTMSTR.dat TXNJRNL.dat RPTFILE.txt ACCTTMP.dat CUSTTMP.dat

echo "--- Phase 1: Compile ---"
cobc -x -o seeddata SEEDDATA.cbl 2>&1
check_cond "SEEDDATA compiled" test -f seeddata

cobc -x -o bankmain BANKMAIN.cbl 2>&1
check_cond "BANKMAIN compiled" test -f bankmain

echo ""
echo "--- Phase 2: Seed Data ---"
./seeddata
check_cond "CUSTMSTR.dat created" test -s CUSTMSTR.dat
check_cond "ACCTMSTR.dat created" test -s ACCTMSTR.dat
check_cond "TXNJRNL.dat created" test -s TXNJRNL.dat

echo ""
echo "--- Phase 3: List All Accounts (option 1->4) ---"
run_bank 1 4 0 0
check_file "Lists JOHNSON account" "JOHNSON" "$TMPOUT"
check_file "Lists WILLIAMS account" "WILLIAMS" "$TMPOUT"
check_file "Lists 10 accounts" "Total accounts: 00010" "$TMPOUT"
check_file "Shows CL status (closed)" "CL" "$TMPOUT"
check_file "Shows FZ status (frozen)" "FZ" "$TMPOUT"
check_file "Shows DDA1 product code" "DDA1" "$TMPOUT"
check_file "Shows SAV1 product code" "SAV1" "$TMPOUT"
check_file "Shows MTG1 product code" "MTG1" "$TMPOUT"
check_file "Shows LH restriction" "LH" "$TMPOUT"

echo ""
echo "--- Phase 4: Account Inquiry (option 4) ---"
run_bank 4 "$ACCT_JOHNSON" 0
check_file "Shows ACCOUNT DETAIL" "ACCOUNT DETAIL" "$TMPOUT"
check_file "Shows account number" "$ACCT_JOHNSON" "$TMPOUT"
check_file "Shows CIF ID" "0000000001" "$TMPOUT"
check_file "Shows DDA1 product" "DDA1" "$TMPOUT"
check_file "Shows ledger balance" "Ledger Balance" "$TMPOUT"
check_file "Shows avail balance" "Avail Balance" "$TMPOUT"
check_file "Shows hold amount" "Hold Amount" "$TMPOUT"
check_file "Shows interest rate" "Interest Rate" "$TMPOUT"
check_file "Shows GL account" "GL Account" "$TMPOUT"
check_file "Shows currency" "USD" "$TMPOUT"
check_file "Shows CIF detail" "CUSTOMER" "$TMPOUT"
check_file "Shows KYC status" "KYC Status" "$TMPOUT"
check_file "Shows AML risk" "AML Risk" "$TMPOUT"

echo ""
echo "--- Phase 5: Deposit (option 2->1) ---"
run_bank 2 1 "$ACCT_JOHNSON" 500.00 0 0
check_file "Deposit successful" "Deposit successful" "$TMPOUT"

run_bank 4 "$ACCT_JOHNSON" 0
check_file "Ledger updated after deposit" "15,734.50" "$TMPOUT"

echo ""
echo "--- Phase 6: Withdrawal (option 2->2) ---"
run_bank 2 2 "$ACCT_JOHNSON" 234.50 0 0
check_file "Withdrawal successful" "Withdrawal successful" "$TMPOUT"

run_bank 4 "$ACCT_JOHNSON" 0
check_file "Ledger updated after withdrawal" "15,500.00" "$TMPOUT"

echo ""
echo "--- Phase 7: Insufficient Funds ---"
run_bank 2 2 "$ACCT_THOMPSON" 999999.00 0 0
check_file "Insufficient funds rejected" "INSUFFICIENT FUNDS" "$TMPOUT"

echo ""
echo "--- Phase 8: Transfer (option 2->3) ---"
run_bank 2 3 "$ACCT_JOHNSON" "$ACCT_WILLIAMS_SAV" 1000.00 0 0
check_file "Transfer completed" "Transfer completed" "$TMPOUT"

run_bank 4 "$ACCT_JOHNSON" 0
check_file "Source ledger decreased" "14,500.00" "$TMPOUT"

run_bank 4 "$ACCT_WILLIAMS_SAV" 0
check_file "Target ledger increased" "88,562.33" "$TMPOUT"

echo ""
echo "--- Phase 9: Batch Service Fees (option 2->4) ---"
run_bank 2 4 0 0
check_file "Fees applied to DDA accounts" "DDA/commercial accounts" "$TMPOUT"

run_bank 4 "$ACCT_JOHNSON" 0
check_file "Fee deducted from ledger" "14,487.50" "$TMPOUT"

echo ""
echo "--- Phase 10: Account Summary Report (option 3->1) ---"
run_bank 3 1 0 0
check_file "Report generated" "Report written to RPTFILE.txt" "$TMPOUT"
check_cond "RPTFILE.txt exists" test -s RPTFILE.txt
check_file "Report contains header" "FIRST NATIONAL BANK" RPTFILE.txt
check_file "Report contains totals" "TOTAL ACCOUNTS" RPTFILE.txt

echo ""
echo "--- Phase 11: Transaction Journal Report (option 3->2) ---"
run_bank 3 2 0 0
check_file "TXN report generated" "Report written to RPTFILE.txt" "$TMPOUT"
check_file "TXN report header" "TRANSACTION JOURNAL" RPTFILE.txt

echo ""
echo "--- Phase 12: Daily Balance Summary (option 3->3) ---"
run_bank 3 3 0 0
check_file "Daily summary displayed" "DAILY BALANCE SUMMARY" "$TMPOUT"
check_file "Shows active account count" "Active Accounts" "$TMPOUT"

echo ""
echo "--- Phase 13: Open New Account (option 1->1) ---"
run_bank 1 1 "SMITH" "JANE" DDA1 10000.00 BR0001 0 0
check_file "Account opened" "ACCOUNT OPENED SUCCESSFULLY" "$TMPOUT"

run_bank 1 4 0 0
check_file "New account appears in list" "SMITH" "$TMPOUT"
check_file "Now 11 accounts total" "Total accounts: 00011" "$TMPOUT"

echo ""
echo "--- Phase 14: Close Account (option 1->2) ---"
run_bank 1 2 "$ACCT_THOMPSON" 0 0
check_file "Account closed" "Account closed" "$TMPOUT"

run_bank 4 "$ACCT_THOMPSON" 0
check_file "Status shows CL" "CL" "$TMPOUT"

echo ""
echo "--- Phase 15: Invalid Input Handling ---"
run_bank 1 1 "BAD" "ACCT" ZZZZ 100.00 BR0001 0 0
check_file "Invalid product rejected" "Invalid product code" "$TMPOUT"

run_bank 4 9999999999999999 0
check_file "Nonexistent account handled" "ACCOUNT NOT FOUND" "$TMPOUT"

echo ""
echo "--- Phase 16: Exit ---"
run_bank 0
check_file "Clean shutdown" "SHUTDOWN COMPLETE" "$TMPOUT"

echo ""
echo "========================================"
echo "  Results: $PASS passed, $FAIL failed"
echo "========================================"

if [ "$FAIL" -gt 0 ]; then
    exit 1
fi
