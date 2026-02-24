package com.fnb.bank.controller;

import com.fnb.bank.entity.Customer;
import com.fnb.bank.service.BankingService;
import org.springframework.web.bind.annotation.*;
import java.util.List;

@RestController
@RequestMapping("/api/customers")
public class CustomerController {

    private final BankingService service;

    public CustomerController(BankingService service) {
        this.service = service;
    }

    @GetMapping
    public List<Customer> list() {
        return service.getAllCustomers();
    }

    @GetMapping("/{cifId}")
    public Customer get(@PathVariable String cifId) {
        return service.getCustomer(cifId);
    }
}
