package com.fnb.bank.repository;

import com.fnb.bank.entity.Account;
import org.springframework.data.jpa.repository.JpaRepository;
import java.util.Optional;
import java.util.List;

public interface AccountRepository extends JpaRepository<Account, Long> {
    Optional<Account> findByAcctNum(String acctNum);
    List<Account> findByCifId(String cifId);
    List<Account> findByProdCode(String prodCode);
    List<Account> findByStatus(String status);
}
