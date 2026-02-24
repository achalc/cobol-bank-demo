package com.fnb.bank.repository;

import com.fnb.bank.entity.Customer;
import org.springframework.data.jpa.repository.JpaRepository;
import java.util.Optional;
import java.util.List;

public interface CustomerRepository extends JpaRepository<Customer, Long> {
    Optional<Customer> findByCifId(String cifId);
    List<Customer> findByLastNameIgnoreCase(String lastName);
    List<Customer> findBySegment(String segment);
    List<Customer> findByStatus(String status);
}
