package com.fnb.bank.controller;

import com.fnb.bank.entity.Account;
import com.fnb.bank.entity.Transaction;
import com.fnb.bank.service.BankingService;
import org.springframework.web.bind.annotation.*;
import java.math.BigDecimal;
import java.util.List;
import java.util.Map;

@RestController
@RequestMapping("/api/accounts")
public class AccountController {

    private final BankingService service;

    public AccountController(BankingService service) {
        this.service = service;
    }

    @GetMapping
    public List<Account> list() {
        return service.getAllAccounts();
    }

    @GetMapping("/{acctNum}")
    public Account get(@PathVariable String acctNum) {
        return service.getAccount(acctNum);
    }

    @GetMapping("/customer/{cifId}")
    public List<Account> byCustomer(@PathVariable String cifId) {
        return service.getAccountsForCustomer(cifId);
    }

    @GetMapping("/{acctNum}/transactions")
    public List<Transaction> transactions(@PathVariable String acctNum) {
        return service.getTransactions(acctNum);
    }

    @PostMapping("/{acctNum}/deposit")
    public Transaction deposit(@PathVariable String acctNum, @RequestBody Map<String, Object> body) {
        BigDecimal amount = new BigDecimal(body.get("amount").toString());
        String channel = (String) body.getOrDefault("channel", "ONL");
        String desc = (String) body.getOrDefault("description", "ONLINE DEPOSIT");
        return service.deposit(acctNum, amount, channel, desc);
    }

    @PostMapping("/{acctNum}/withdraw")
    public Transaction withdraw(@PathVariable String acctNum, @RequestBody Map<String, Object> body) {
        BigDecimal amount = new BigDecimal(body.get("amount").toString());
        String channel = (String) body.getOrDefault("channel", "ONL");
        String desc = (String) body.getOrDefault("description", "ONLINE WITHDRAWAL");
        return service.withdraw(acctNum, amount, channel, desc);
    }

    @PostMapping("/transfer")
    public List<Transaction> transfer(@RequestBody Map<String, Object> body) {
        String from = (String) body.get("fromAccount");
        String to = (String) body.get("toAccount");
        BigDecimal amount = new BigDecimal(body.get("amount").toString());
        return service.transfer(from, to, amount);
    }

    @PostMapping("/batch/monthly-fees")
    public List<Transaction> monthlyFees(@RequestBody(required = false) Map<String, Object> body) {
        BigDecimal fee = body != null && body.containsKey("amount")
                ? new BigDecimal(body.get("amount").toString())
                : new BigDecimal("12.50");
        return service.runMonthlyFees(fee);
    }
}
