package com.fnb.bank.repository;

import com.fnb.bank.entity.Transaction;
import org.springframework.data.jpa.repository.JpaRepository;
import java.util.Optional;
import java.util.List;

public interface TransactionRepository extends JpaRepository<Transaction, Long> {
    Optional<Transaction> findByTxnId(String txnId);
    List<Transaction> findByAcctNumOrderByPostDtDescTxnIdDesc(String acctNum);
    List<Transaction> findByChannel(String channel);
    List<Transaction> findByReversalFlag(String reversalFlag);
}
