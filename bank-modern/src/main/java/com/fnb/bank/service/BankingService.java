package com.fnb.bank.service;

import com.fnb.bank.entity.*;
import com.fnb.bank.repository.*;
import org.springframework.stereotype.Service;
import org.springframework.transaction.annotation.Transactional;

import java.math.BigDecimal;
import java.time.LocalDate;
import java.time.format.DateTimeFormatter;
import java.util.List;
import java.util.concurrent.atomic.AtomicLong;

@Service
public class BankingService {

    private final CustomerRepository customerRepo;
    private final AccountRepository accountRepo;
    private final TransactionRepository txnRepo;
    private final AtomicLong txnSeq = new AtomicLong(100000000000L);

    public BankingService(CustomerRepository customerRepo, AccountRepository accountRepo, TransactionRepository txnRepo) {
        this.customerRepo = customerRepo;
        this.accountRepo = accountRepo;
        this.txnRepo = txnRepo;
    }

    public Customer getCustomer(String cifId) {
        return customerRepo.findByCifId(cifId)
                .orElseThrow(() -> new IllegalArgumentException("Customer not found: " + cifId));
    }

    public List<Customer> getAllCustomers() {
        return customerRepo.findAll();
    }

    public Account getAccount(String acctNum) {
        return accountRepo.findByAcctNum(acctNum)
                .orElseThrow(() -> new IllegalArgumentException("Account not found: " + acctNum));
    }

    public List<Account> getAccountsForCustomer(String cifId) {
        return accountRepo.findByCifId(cifId);
    }

    public List<Account> getAllAccounts() {
        return accountRepo.findAll();
    }

    public List<Transaction> getTransactions(String acctNum) {
        return txnRepo.findByAcctNumOrderByPostDtDescTxnIdDesc(acctNum);
    }

    @Transactional
    public Transaction deposit(String acctNum, BigDecimal amount, String channel, String desc) {
        Account acct = getAccount(acctNum);
        validateAccountActive(acct);
        if ("NC".equals(acct.getRestrictCd()))
            throw new IllegalStateException("Account restricted: no credits allowed");

        acct.setLedgerBal(acct.getLedgerBal().add(amount));
        acct.setAvailBal(acct.getAvailBal().add(amount));
        acct.setLastActivityDt(LocalDate.now());
        acct.setMaintDt(LocalDate.now());
        acct.setMaintUser("APIONLN");
        accountRepo.save(acct);

        return postTransaction(acctNum, acct.getCifId(), "DEP", "C", amount,
                acct.getLedgerBal(), channel, desc, "1000000001", "2000000001");
    }

    @Transactional
    public Transaction withdraw(String acctNum, BigDecimal amount, String channel, String desc) {
        Account acct = getAccount(acctNum);
        validateAccountActive(acct);
        if ("ND".equals(acct.getRestrictCd()))
            throw new IllegalStateException("Account restricted: no debits allowed");
        if (acct.getAvailBal().compareTo(amount) < 0)
            throw new IllegalStateException("Insufficient funds");

        acct.setLedgerBal(acct.getLedgerBal().subtract(amount));
        acct.setAvailBal(acct.getAvailBal().subtract(amount));
        acct.setLastActivityDt(LocalDate.now());
        acct.setMaintDt(LocalDate.now());
        acct.setMaintUser("APIONLN");
        if (List.of("SAV1", "MMA1").contains(acct.getProdCode()) && acct.getRegdCtr() != null)
            acct.setRegdCtr(acct.getRegdCtr() + 1);
        accountRepo.save(acct);

        return postTransaction(acctNum, acct.getCifId(), "WDL", "D", amount,
                acct.getLedgerBal(), channel, desc, "2000000001", "1000000001");
    }

    @Transactional
    public List<Transaction> transfer(String fromAcct, String toAcct, BigDecimal amount) {
        Account from = getAccount(fromAcct);
        Account to = getAccount(toAcct);
        validateAccountActive(from);
        validateAccountActive(to);
        if (from.getAvailBal().compareTo(amount) < 0)
            throw new IllegalStateException("Insufficient funds in source account");

        from.setLedgerBal(from.getLedgerBal().subtract(amount));
        from.setAvailBal(from.getAvailBal().subtract(amount));
        from.setLastActivityDt(LocalDate.now());
        from.setMaintDt(LocalDate.now());
        from.setMaintUser("APIONLN");

        to.setLedgerBal(to.getLedgerBal().add(amount));
        to.setAvailBal(to.getAvailBal().add(amount));
        to.setLastActivityDt(LocalDate.now());
        to.setMaintDt(LocalDate.now());
        to.setMaintUser("APIONLN");

        accountRepo.save(from);
        accountRepo.save(to);

        Transaction xdr = postTransaction(fromAcct, from.getCifId(), "XDR", "D", amount,
                from.getLedgerBal(), "INT", "TRANSFER TO " + toAcct, "1000000001", "9000000001");
        Transaction xcr = postTransaction(toAcct, to.getCifId(), "XCR", "C", amount,
                to.getLedgerBal(), "INT", "TRANSFER FROM " + fromAcct, "9000000001", "1000000001");
        return List.of(xdr, xcr);
    }

    @Transactional
    public List<Transaction> runMonthlyFees(BigDecimal feeAmount) {
        List<Account> feeAccounts = accountRepo.findAll().stream()
                .filter(a -> "AC".equals(a.getStatus()))
                .filter(a -> List.of("DDA1", "DDA2", "COM1").contains(a.getProdCode()))
                .toList();

        return feeAccounts.stream().map(acct -> {
            acct.setLedgerBal(acct.getLedgerBal().subtract(feeAmount));
            acct.setAvailBal(acct.getAvailBal().subtract(feeAmount));
            acct.setLastActivityDt(LocalDate.now());
            acct.setMaintDt(LocalDate.now());
            acct.setMaintUser("BATSVC");
            accountRepo.save(acct);
            return postTransaction(acct.getAcctNum(), acct.getCifId(), "SVC", "D", feeAmount,
                    acct.getLedgerBal(), "BAT", "MONTHLY SERVICE FEE", "5100000001", "4000000001");
        }).toList();
    }

    private void validateAccountActive(Account acct) {
        if (!"AC".equals(acct.getStatus()))
            throw new IllegalStateException("Account is not active (status=" + acct.getStatus() + ")");
    }

    private Transaction postTransaction(String acctNum, String cifId, String type, String dcInd,
                                         BigDecimal amount, BigDecimal runBal, String channel,
                                         String desc, String glDr, String glCr) {
        Transaction txn = new Transaction();
        txn.setTxnId(String.valueOf(txnSeq.incrementAndGet()));
        txn.setAcctNum(acctNum);
        txn.setTxnType(type);
        txn.setTxnDesc(desc);
        txn.setDcInd(dcInd);
        txn.setAmount(amount);
        txn.setRunBal(runBal);
        txn.setEffDt(LocalDate.now());
        txn.setPostDt(LocalDate.now());
        txn.setChannel(channel);
        txn.setGlDr(glDr);
        txn.setGlCr(glCr);
        txn.setReversalFlag(" ");
        txn.setStatus("P");
        return txnRepo.save(txn);
    }
}
