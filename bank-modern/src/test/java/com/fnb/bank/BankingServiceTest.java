package com.fnb.bank;

import com.fnb.bank.entity.*;
import com.fnb.bank.service.BankingService;
import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.context.SpringBootTest;

import java.math.BigDecimal;
import java.util.List;

import static org.junit.jupiter.api.Assertions.*;

@SpringBootTest
class BankingServiceTest {

    @Autowired
    private BankingService service;

    @Test
    void seedDataLoaded() {
        assertEquals(8, service.getAllCustomers().size());
        assertEquals(10, service.getAllAccounts().size());
    }

    @Test
    void customerLookup() {
        Customer c = service.getCustomer("0000000001");
        assertEquals("JOHNSON", c.getLastName());
        assertEquals("ROBERT", c.getFirstName());
        assertEquals("RT", c.getSegment());
        assertEquals("V", c.getKycStatus());
        assertEquals("L", c.getAmlRisk());
    }

    @Test
    void accountLookup() {
        Account a = service.getAccount("0000010000014721");
        assertEquals("DDA1", a.getProdCode());
        assertEquals("AC", a.getStatus());
        assertEquals(0, new BigDecimal("15234.50").compareTo(a.getLedgerBal()));
        assertEquals(0, new BigDecimal("14734.50").compareTo(a.getAvailBal()));
        assertEquals(0, new BigDecimal("500.00").compareTo(a.getHoldBal()));
    }

    @Test
    void accountsByCustomer() {
        List<Account> accts = service.getAccountsForCustomer("0000000002");
        assertEquals(2, accts.size());
    }

    @Test
    void transactionHistory() {
        List<Transaction> txns = service.getTransactions("0000010000014721");
        assertEquals(4, txns.size());
    }

    @Test
    void deposit() {
        Account before = service.getAccount("0000030000042156");
        BigDecimal origBal = before.getLedgerBal();
        Transaction txn = service.deposit("0000030000042156", new BigDecimal("1000.00"), "ONL", "TEST DEPOSIT");
        Account after = service.getAccount("0000030000042156");
        assertEquals(0, origBal.add(new BigDecimal("1000.00")).compareTo(after.getLedgerBal()));
        assertEquals("DEP", txn.getTxnType());
        assertEquals("C", txn.getDcInd());
        assertEquals("P", txn.getStatus());
    }

    @Test
    void withdraw() {
        Account before = service.getAccount("0000050000069018");
        BigDecimal origBal = before.getLedgerBal();
        Transaction txn = service.withdraw("0000050000069018", new BigDecimal("100.00"), "ATM", "TEST WDL");
        Account after = service.getAccount("0000050000069018");
        assertEquals(0, origBal.subtract(new BigDecimal("100.00")).compareTo(after.getLedgerBal()));
        assertEquals("WDL", txn.getTxnType());
        assertEquals("D", txn.getDcInd());
    }

    @Test
    void transfer() {
        Account fromBefore = service.getAccount("0000060000073407");
        Account toBefore = service.getAccount("0000020000028834");
        BigDecimal fromOrig = fromBefore.getLedgerBal();
        BigDecimal toOrig = toBefore.getLedgerBal();

        List<Transaction> txns = service.transfer("0000060000073407", "0000020000028834", new BigDecimal("500.00"));
        assertEquals(2, txns.size());
        assertEquals("XDR", txns.get(0).getTxnType());
        assertEquals("XCR", txns.get(1).getTxnType());

        Account fromAfter = service.getAccount("0000060000073407");
        Account toAfter = service.getAccount("0000020000028834");
        assertEquals(0, fromOrig.subtract(new BigDecimal("500.00")).compareTo(fromAfter.getLedgerBal()));
        assertEquals(0, toOrig.add(new BigDecimal("500.00")).compareTo(toAfter.getLedgerBal()));
    }

    @Test
    void closedAccountRejectsDeposit() {
        assertThrows(IllegalStateException.class,
                () -> service.deposit("0000070000095562", BigDecimal.ONE, "ONL", "TEST"));
    }

    @Test
    void frozenAccountRejectsWithdraw() {
        assertThrows(IllegalStateException.class,
                () -> service.withdraw("0000080000108901", BigDecimal.ONE, "ONL", "TEST"));
    }

    @Test
    void insufficientFundsRejectsWithdraw() {
        assertThrows(IllegalStateException.class,
                () -> service.withdraw("0000050000069018", new BigDecimal("999999.00"), "ONL", "TEST"));
    }

    @Test
    void monthlyFees() {
        List<Transaction> fees = service.runMonthlyFees(new BigDecimal("12.50"));
        assertTrue(fees.size() >= 2);
        fees.forEach(t -> {
            assertEquals("SVC", t.getTxnType());
            assertEquals("D", t.getDcInd());
            assertEquals("BAT", t.getChannel());
        });
    }

    @Test
    void customerNotFound() {
        assertThrows(IllegalArgumentException.class,
                () -> service.getCustomer("9999999999"));
    }

    @Test
    void accountNotFound() {
        assertThrows(IllegalArgumentException.class,
                () -> service.getAccount("9999999999999999"));
    }

    @Test
    void commercialCustomer() {
        Customer c = service.getCustomer("0000000008");
        assertEquals("NAKAMURA INTL LLC", c.getLastName());
        assertEquals("CB", c.getSegment());
        assertEquals("E", c.getTaxIdType());
        assertEquals("H", c.getAmlRisk());
    }

    @Test
    void cdAccountHasRestriction() {
        Account a = service.getAccount("0000060000083407");
        assertEquals("CD01", a.getProdCode());
        assertEquals("ND", a.getRestrictCd());
        assertNotNull(a.getMaturityDt());
    }

    @Test
    void pairedTransferEntries() {
        List<Transaction> patelTxns = service.getTransactions("0000060000073407");
        long xdrCount = patelTxns.stream().filter(t -> "XDR".equals(t.getTxnType())).count();
        assertTrue(xdrCount >= 1);
    }

    @Test
    void reversedTransaction() {
        List<Transaction> johnsonTxns = service.getTransactions("0000010000014721");
        Transaction odf = johnsonTxns.stream()
                .filter(t -> "ODF".equals(t.getTxnType()))
                .findFirst().orElseThrow();
        assertEquals("V", odf.getReversalFlag());
        assertEquals("R", odf.getStatus());
    }
}
