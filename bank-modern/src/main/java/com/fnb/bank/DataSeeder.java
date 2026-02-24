package com.fnb.bank;

import com.fnb.bank.entity.*;
import com.fnb.bank.repository.*;
import org.springframework.boot.CommandLineRunner;
import org.springframework.stereotype.Component;
import java.math.BigDecimal;
import java.time.LocalDate;

@Component
public class DataSeeder implements CommandLineRunner {

    private final CustomerRepository customerRepo;
    private final AccountRepository accountRepo;
    private final TransactionRepository txnRepo;

    public DataSeeder(CustomerRepository customerRepo, AccountRepository accountRepo, TransactionRepository txnRepo) {
        this.customerRepo = customerRepo;
        this.accountRepo = accountRepo;
        this.txnRepo = txnRepo;
    }

    @Override
    public void run(String... args) {
        if (customerRepo.count() > 0) return;
        seedCustomers();
        seedAccounts();
        seedTransactions();
    }

    private Customer cust(String cifId, String taxIdType, String taxId, String last, String first,
                          String mi, String addr1, String addr2, String city, String state,
                          String zip, String zip4, String phone1, String phone2, String email,
                          String kyc, LocalDate kycDt, String aml, String cra, String officer,
                          String seg, String status, LocalDate openDt, LocalDate maintDt, String maintUser) {
        Customer c = new Customer();
        c.setCifId(cifId); c.setTaxIdType(taxIdType); c.setTaxId(taxId);
        c.setLastName(last); c.setFirstName(first); c.setMiddleInitial(mi);
        c.setAddrLine1(addr1); c.setAddrLine2(addr2); c.setCity(city); c.setState(state);
        c.setZip(zip); c.setZip4(zip4); c.setCountry("USA");
        c.setPhonePrimary(phone1); c.setPhoneAlt(phone2); c.setEmail(email);
        c.setKycStatus(kyc); c.setKycDate(kycDt); c.setAmlRisk(aml); c.setCraCode(cra);
        c.setRelOfficer(officer); c.setSegment(seg); c.setStatus(status);
        c.setOpenDate(openDt); c.setMaintDate(maintDt); c.setMaintUser(maintUser);
        return c;
    }

    private void seedCustomers() {
        customerRepo.save(cust("0000000001", "S", "***-**-4721", "JOHNSON", "ROBERT", "A",
                "1247 PARK AVENUE", "APT 14B", "NEW YORK", "NY", "10128", "3341",
                "212-555-0147", "917-555-0382", "rjohnson@email.com",
                "V", LocalDate.of(2019,3,1), "L", "01", "OFC042", "RT", "AC",
                LocalDate.of(2019,3,15), LocalDate.of(2026,1,15), "JMURPHY"));
        customerRepo.save(cust("0000000002", "S", "***-**-8834", "WILLIAMS", "SARAH", "M",
                "89 EAST 72ND STREET", null, "NEW YORK", "NY", "10021", "4102",
                "212-555-0293", null, "swilliams@privatebank.com",
                "V", LocalDate.of(2018,6,10), "L", "01", "OFC017", "WM", "AC",
                LocalDate.of(2018,6,22), LocalDate.of(2026,1,10), "SOKONKW"));
        customerRepo.save(cust("0000000003", "S", "***-**-2156", "CHEN", "DAVID", "L",
                "4501 BROADWAY", "SUITE 200", "NEW YORK", "NY", "10040", "2587",
                "646-555-0184", "646-555-0199", "dchen@corp.com",
                "V", LocalDate.of(2020,8,1), "L", "02", "OFC042", "RT", "AC",
                LocalDate.of(2020,8,10), LocalDate.of(2026,1,25), "PZHANG"));
        customerRepo.save(cust("0000000004", "S", "***-**-6743", "MARTINEZ", "ANA", "P",
                "200 CENTRAL PARK SOUTH", "UNIT 31F", "NEW YORK", "NY", "10019", "1001",
                "212-555-0456", null, "amartinez@wealth.com",
                "V", LocalDate.of(2021,3,15), "L", "01", "OFC017", "PB", "AC",
                LocalDate.of(2021,4,1), LocalDate.of(2026,1,1), "KDAVIES"));
        customerRepo.save(cust("0000000005", "S", "***-**-9018", "THOMPSON", "MICHAEL", "J",
                "3820 JEROME AVENUE", null, "BRONX", "NY", "10467", "5590",
                "718-555-0337", null, "mthompson@mail.com",
                "V", LocalDate.of(2022,1,10), "L", "03", "OFC098", "RT", "AC",
                LocalDate.of(2022,1,15), LocalDate.of(2026,1,28), "RGUPTA"));
        customerRepo.save(cust("0000000006", "S", "***-**-3407", "PATEL", "PRIYA", "S",
                "15 BROAD STREET", "FL 28", "NEW YORK", "NY", "10005", "1092",
                "212-555-0741", "347-555-0622", "ppatel@invest.com",
                "V", LocalDate.of(2017,8,15), "M", "01", "OFC017", "WM", "AC",
                LocalDate.of(2017,9,3), LocalDate.of(2026,2,3), "SOKONKW"));
        customerRepo.save(cust("0000000007", "S", "***-**-5562", "OBRIEN", "KATHERINE", "E",
                "447 WEST 18TH STREET", null, "NEW YORK", "NY", "10011", "6320",
                "212-555-0889", null, "kobrien@law.com",
                "X", LocalDate.of(2023,7,1), "L", "01", "OFC098", "RT", "IN",
                LocalDate.of(2023,7,18), LocalDate.of(2025,12,15), "PZHANG"));
        customerRepo.save(cust("0000000008", "E", "**-***-8901", "NAKAMURA INTL LLC", null, null,
                "350 5TH AVENUE", "SUITE 7800", "NEW YORK", "NY", "10118", "0110",
                "212-555-0901", "212-555-0902", "ops@nakamuraintl.com",
                "V", LocalDate.of(2024,1,15), "H", "00", "OFC042", "CB", "AC",
                LocalDate.of(2024,2,1), LocalDate.of(2026,1,5), "SOKONKW"));
    }

    private Account acct(String num, String cif, String prod, String prodDesc, String status,
                         LocalDate openDt, LocalDate closeDt,
                         String ledger, String avail, String hold, String uncoll,
                         String odLimit, String creditLimit,
                         String intRate, String intAccrued, String intYtd, String intPriorYr,
                         LocalDate intLastCalc, LocalDate maturityDt,
                         String cc, String gl, String officer, String stmtCycle,
                         LocalDate stmtLastDt, String restrictCd, int regdCtr,
                         String branch, LocalDate lastActivity, LocalDate maintDt, String maintUser) {
        Account a = new Account();
        a.setAcctNum(num); a.setCifId(cif); a.setProdCode(prod); a.setProdDesc(prodDesc);
        a.setStatus(status); a.setOpenDt(openDt); a.setCloseDt(closeDt);
        a.setLedgerBal(new BigDecimal(ledger)); a.setAvailBal(new BigDecimal(avail));
        a.setHoldBal(new BigDecimal(hold)); a.setUncollBal(new BigDecimal(uncoll));
        a.setOdLimit(new BigDecimal(odLimit)); a.setCreditLimit(new BigDecimal(creditLimit));
        a.setIntRate(new BigDecimal(intRate)); a.setIntAccrued(new BigDecimal(intAccrued));
        a.setIntYtd(new BigDecimal(intYtd)); a.setIntPriorYr(new BigDecimal(intPriorYr));
        a.setIntLastCalc(intLastCalc); a.setMaturityDt(maturityDt);
        a.setCostCenter(cc); a.setGlAcct(gl); a.setOfficer(officer);
        a.setCurrency("USD"); a.setStmtCycle(stmtCycle); a.setStmtLastDt(stmtLastDt);
        a.setRestrictCd(restrictCd); a.setRegdCtr(regdCtr); a.setTaxRptg("Y");
        a.setBranch(branch); a.setLastActivityDt(lastActivity);
        a.setMaintDt(maintDt); a.setMaintUser(maintUser);
        return a;
    }

    private void seedAccounts() {
        accountRepo.save(acct("0000010000014721", "0000000001", "DDA1", "CHECKING STANDARD", "AC",
                LocalDate.of(2019,3,15), null,
                "15234.50", "14734.50", "500.00", "0.00", "500.00", "0.00",
                "0.000100", "1.27", "18.44", "142.30",
                LocalDate.of(2026,2,1), null,
                "CC1001", "1001000100", "OFC042", "MO",
                LocalDate.of(2026,1,31), null, 0,
                "BR0001", LocalDate.of(2026,2,1), LocalDate.of(2026,1,15), "JMURPHY"));
        accountRepo.save(acct("0000020000028834", "0000000002", "SAV1", "SAVINGS STANDARD", "AC",
                LocalDate.of(2018,6,22), null,
                "87562.33", "87562.33", "0.00", "0.00", "0.00", "0.00",
                "4.250000", "307.41", "3812.09", "3247.81",
                LocalDate.of(2026,2,1), null,
                "CC1002", "1002000100", "OFC017", "QT",
                LocalDate.of(2025,12,31), null, 2,
                "BR0001", LocalDate.of(2026,1,15), LocalDate.of(2026,1,10), "SOKONKW"));
        accountRepo.save(acct("0000020000038834", "0000000002", "MMA1", "MONEY MARKET ACCT", "AC",
                LocalDate.of(2020,1,15), null,
                "250000.00", "250000.00", "0.00", "0.00", "0.00", "0.00",
                "5.100000", "1054.79", "13050.00", "10125.00",
                LocalDate.of(2026,2,1), null,
                "CC1002", "1003000100", "OFC017", "MO",
                LocalDate.of(2026,1,31), null, 1,
                "BR0001", LocalDate.of(2026,1,20), LocalDate.of(2026,1,10), "SOKONKW"));
        accountRepo.save(acct("0000030000042156", "0000000003", "DDA1", "CHECKING STANDARD", "AC",
                LocalDate.of(2020,8,10), null,
                "3421.09", "3421.09", "0.00", "0.00", "200.00", "0.00",
                "0.000100", "0.03", "0.42", "3.18",
                LocalDate.of(2026,2,1), null,
                "CC2001", "1001000200", "OFC042", "MO",
                LocalDate.of(2026,1,31), null, 0,
                "BR0002", LocalDate.of(2026,1,30), LocalDate.of(2026,1,25), "PZHANG"));
        accountRepo.save(acct("0000040000056743", "0000000004", "MTG1", "MORTGAGE 30YR FIXED", "AC",
                LocalDate.of(2021,4,1), null,
                "245000.00", "0.00", "0.00", "0.00", "0.00", "325000.00",
                "6.875000", "1402.08", "16825.00", "17062.50",
                LocalDate.of(2026,2,1), LocalDate.of(2051,4,1),
                "CC3001", "1501000100", "OFC017", "MO",
                LocalDate.of(2026,1,31), null, 0,
                "BR0002", LocalDate.of(2026,1,1), LocalDate.of(2026,1,1), "KDAVIES"));
        accountRepo.save(acct("0000050000069018", "0000000005", "DDA1", "CHECKING STANDARD", "AC",
                LocalDate.of(2022,1,15), null,
                "452.78", "452.78", "0.00", "0.00", "0.00", "0.00",
                "0.000100", "0.00", "0.06", "0.51",
                LocalDate.of(2026,2,1), null,
                "CC4001", "1001000300", "OFC098", "MO",
                LocalDate.of(2026,1,31), null, 0,
                "BR0003", LocalDate.of(2026,1,28), LocalDate.of(2026,1,28), "RGUPTA"));
        accountRepo.save(acct("0000060000073407", "0000000006", "SAV1", "SAVINGS STANDARD", "AC",
                LocalDate.of(2017,9,3), null,
                "156789.44", "156789.44", "0.00", "0.00", "0.00", "0.00",
                "4.250000", "550.63", "6825.12", "5934.00",
                LocalDate.of(2026,2,1), null,
                "CC1002", "1002000200", "OFC017", "MO",
                LocalDate.of(2026,1,31), null, 3,
                "BR0001", LocalDate.of(2026,2,3), LocalDate.of(2026,2,3), "SOKONKW"));
        accountRepo.save(acct("0000060000083407", "0000000006", "CD01", "CERT OF DEPOSIT 12M", "AC",
                LocalDate.of(2025,6,1), null,
                "100000.00", "0.00", "0.00", "0.00", "0.00", "0.00",
                "5.350000", "3567.12", "3567.12", "0.00",
                LocalDate.of(2026,2,1), LocalDate.of(2026,6,1),
                "CC1003", "1004000100", "OFC017", "MO",
                LocalDate.of(2026,1,31), "ND", 0,
                "BR0001", LocalDate.of(2025,6,1), LocalDate.of(2025,6,1), "SOKONKW"));
        accountRepo.save(acct("0000070000095562", "0000000007", "DDA1", "CHECKING STANDARD", "CL",
                LocalDate.of(2023,7,18), LocalDate.of(2025,12,15),
                "28910.66", "28910.66", "0.00", "0.00", "500.00", "0.00",
                "0.000100", "0.00", "0.00", "24.18",
                LocalDate.of(2025,12,15), null,
                "CC4001", "1001000300", "OFC098", "MO",
                LocalDate.of(2025,12,15), null, 0,
                "BR0003", LocalDate.of(2025,12,15), LocalDate.of(2025,12,15), "PZHANG"));
        accountRepo.save(acct("0000080000108901", "0000000008", "COM1", "COMMERCIAL DDA", "FZ",
                LocalDate.of(2024,2,1), null,
                "42100.00", "0.00", "42100.00", "0.00", "5000.00", "100000.00",
                "0.000500", "1.75", "21.88", "0.00",
                LocalDate.of(2026,1,5), null,
                "CC5001", "1006000100", "OFC042", "MO",
                LocalDate.of(2026,1,5), "LH", 0,
                "BR0002", LocalDate.of(2026,1,5), LocalDate.of(2026,1,5), "SOKONKW"));
    }

    private Transaction txn(String id, String acct, String type, String dc, String amt,
                            String runBal, LocalDate eff, LocalDate post, String channel,
                            String teller, String terminal, String ref, String desc,
                            String glDr, String glCr, String revFlag, String origSeq, String status) {
        Transaction t = new Transaction();
        t.setTxnId(id); t.setAcctNum(acct); t.setTxnType(type); t.setDcInd(dc);
        t.setAmount(new BigDecimal(amt)); t.setRunBal(new BigDecimal(runBal));
        t.setEffDt(eff); t.setPostDt(post); t.setChannel(channel);
        t.setTellerId(teller); t.setTerminalId(terminal); t.setRefNum(ref);
        t.setTxnDesc(desc); t.setGlDr(glDr); t.setGlCr(glCr);
        t.setReversalFlag(revFlag); t.setOrigTxnId(origSeq); t.setStatus(status);
        return t;
    }

    private void seedTransactions() {
        txnRepo.save(txn("000000000001", "0000010000014721", "DEP", "C", "5000.00", "18234.50",
                LocalDate.of(2026,1,20), LocalDate.of(2026,1,20), "ACH",
                null, "ACHGW001", "PPD 091523CORP01",
                "PAYROLL DIRECT DEPOSIT - ACME CORP",
                "1001000100", "2001000100", " ", null, "P"));
        txnRepo.save(txn("000000000002", "0000010000014721", "WDL", "D", "1200.00", "17034.50",
                LocalDate.of(2026,1,21), LocalDate.of(2026,1,21), "ATM",
                null, "ATM04721", null,
                "ATM WITHDRAWAL - 3RD AVE & 86TH",
                "2001000100", "1001000100", " ", null, "P"));
        txnRepo.save(txn("000000000003", "0000020000028834", "DEP", "C", "12500.00", "97562.33",
                LocalDate.of(2026,1,15), LocalDate.of(2026,1,15), "WIR",
                "TLR042", "WIREGATE", "FW021000089-00147",
                "INCOMING WIRE - MORGAN STANLEY",
                "1002000100", "2002000100", " ", null, "P"));
        txnRepo.save(txn("000000000004", "0000030000042156", "CKP", "D", "500.00", "3421.09",
                LocalDate.of(2026,1,25), LocalDate.of(2026,1,26), "BRN",
                "TLR098", "TERM0002", "CHK#001042",
                "CHECK PRESENTED - LANDLORD MGMT CO",
                "2001000200", "1001000200", " ", null, "P"));
        txnRepo.save(txn("000000000005", "0000040000056743", "DEP", "C", "2450.00", "245000.00",
                LocalDate.of(2026,1,1), LocalDate.of(2026,1,2), "ACH",
                null, "ACHGW001", "PPD MTG-PMT 202601",
                "MONTHLY MORTGAGE PAYMENT",
                "1501000100", "2001000300", " ", null, "P"));
        txnRepo.save(txn("000000000006", "0000050000069018", "DEP", "C", "3200.00", "3252.78",
                LocalDate.of(2026,1,28), LocalDate.of(2026,1,28), "ACH",
                null, "ACHGW001", "PPD 090000EMPL42",
                "PAYROLL DIRECT DEPOSIT - SMITH INC",
                "1001000300", "2001000300", " ", null, "P"));
        txnRepo.save(txn("000000000007", "0000050000069018", "WDL", "D", "2800.00", "452.78",
                LocalDate.of(2026,1,28), LocalDate.of(2026,1,28), "ONL",
                null, "ONLBNK01", "BP-20260128-RENT",
                "ONLINE BILL PAY - BRONX REALTY LLC",
                "2001000300", "1001000300", " ", null, "P"));
        txnRepo.save(txn("000000000008", "0000060000073407", "XDR", "D", "10000.00", "156789.44",
                LocalDate.of(2026,2,3), LocalDate.of(2026,2,3), "MOB",
                null, "MOBAPP01", "XFER-20260203-001",
                "XFER TO 0000020000028834",
                "2002000200", "1002000100", " ", null, "P"));
        txnRepo.save(txn("000000000009", "0000020000028834", "XCR", "C", "10000.00", "97562.33",
                LocalDate.of(2026,2,3), LocalDate.of(2026,2,3), "INT",
                "SYSTEM", "INTERNAL", "XFER-20260203-001",
                "XFER FR 0000060000073407",
                "1002000100", "2002000200", " ", null, "P"));
        txnRepo.save(txn("000000000010", "0000010000014721", "SVC", "D", "12.50", "15234.50",
                LocalDate.of(2026,2,1), LocalDate.of(2026,2,1), "BAT",
                "SYSTEM", "BATCHSVC", "SVC-FEE-202602",
                "MONTHLY SERVICE CHARGE - DDA",
                "2001000100", "4001000100", " ", null, "P"));
        txnRepo.save(txn("000000000011", "0000030000042156", "CKD", "C", "750.00", "3671.09",
                LocalDate.of(2026,1,30), LocalDate.of(2026,1,31), "MOB",
                null, "MOBAPP01", "MDEP-20260130-4721",
                "MOBILE CHECK DEPOSIT - FREELANCE",
                "1001000200", "2001000200", " ", null, "H"));
        txnRepo.save(txn("000000000012", "0000060000083407", "ACR", "C", "445.83", "100445.83",
                LocalDate.of(2026,2,1), LocalDate.of(2026,2,1), "BAT",
                "SYSTEM", "BATCHINT", "INT-CD01-20260201",
                "INTEREST ACCRUAL - CD 5.35% APY",
                "5001000100", "1004000100", " ", null, "P"));
        txnRepo.save(txn("000000000013", "0000020000028834", "WFE", "D", "25.00", "87537.33",
                LocalDate.of(2026,2,5), LocalDate.of(2026,2,5), "WIR",
                "TLR042", "WIREGATE", "WF-20260205-SAV1",
                "OUTGOING WIRE FEE",
                "2002000100", "4002000100", " ", null, "P"));
        txnRepo.save(txn("000000000014", "0000020000028834", "WDL", "D", "5000.00", "82537.33",
                LocalDate.of(2026,2,5), LocalDate.of(2026,2,5), "WIR",
                "TLR042", "WIREGATE", "FW026009593-00289",
                "OUTGOING WIRE - SCHWAB BROKERAGE",
                "2002000100", "1002000100", " ", null, "H"));
        txnRepo.save(txn("000000000015", "0000010000014721", "ODF", "D", "35.00", "15199.50",
                LocalDate.of(2026,1,15), LocalDate.of(2026,1,15), "BAT",
                "SYSTEM", "BATCHODF", "ODF-20260115-001",
                "NSF/OVERDRAFT FEE - REVERSED",
                "2001000100", "4003000100", "V", null, "R"));
    }
}
