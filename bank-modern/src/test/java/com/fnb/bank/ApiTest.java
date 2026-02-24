package com.fnb.bank;

import org.junit.jupiter.api.Test;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.boot.test.autoconfigure.web.servlet.AutoConfigureMockMvc;
import org.springframework.boot.test.context.SpringBootTest;
import org.springframework.http.MediaType;
import org.springframework.test.web.servlet.MockMvc;

import static org.springframework.test.web.servlet.request.MockMvcRequestBuilders.*;
import static org.springframework.test.web.servlet.result.MockMvcResultMatchers.*;

@SpringBootTest
@AutoConfigureMockMvc
class ApiTest {

    @Autowired
    private MockMvc mvc;

    @Test
    void listCustomers() throws Exception {
        mvc.perform(get("/api/customers"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.length()").value(8));
    }

    @Test
    void getCustomer() throws Exception {
        mvc.perform(get("/api/customers/0000000001"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.lastName").value("JOHNSON"))
                .andExpect(jsonPath("$.segment").value("RT"));
    }

    @Test
    void customerNotFound() throws Exception {
        mvc.perform(get("/api/customers/9999999999"))
                .andExpect(status().isNotFound());
    }

    @Test
    void listAccounts() throws Exception {
        mvc.perform(get("/api/accounts"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.length()").value(10));
    }

    @Test
    void getAccount() throws Exception {
        mvc.perform(get("/api/accounts/0000010000014721"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.prodCode").value("DDA1"))
                .andExpect(jsonPath("$.ledgerBal").value(15234.50));
    }

    @Test
    void accountsByCustomer() throws Exception {
        mvc.perform(get("/api/accounts/customer/0000000002"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.length()").value(2));
    }

    @Test
    void accountTransactions() throws Exception {
        mvc.perform(get("/api/accounts/0000010000014721/transactions"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.length()").value(4));
    }

    @Test
    void depositViaApi() throws Exception {
        mvc.perform(post("/api/accounts/0000030000042156/deposit")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content("{\"amount\": 250.00, \"channel\": \"ONL\", \"description\": \"API DEPOSIT\"}"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.txnType").value("DEP"))
                .andExpect(jsonPath("$.dcInd").value("C"));
    }

    @Test
    void transferViaApi() throws Exception {
        mvc.perform(post("/api/accounts/transfer")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content("{\"fromAccount\": \"0000060000073407\", \"toAccount\": \"0000020000028834\", \"amount\": 100.00}"))
                .andExpect(status().isOk())
                .andExpect(jsonPath("$.length()").value(2))
                .andExpect(jsonPath("$[0].txnType").value("XDR"))
                .andExpect(jsonPath("$[1].txnType").value("XCR"));
    }

    @Test
    void closedAccountRejectsViaApi() throws Exception {
        mvc.perform(post("/api/accounts/0000070000095562/deposit")
                        .contentType(MediaType.APPLICATION_JSON)
                        .content("{\"amount\": 100.00}"))
                .andExpect(status().isUnprocessableEntity());
    }
}
