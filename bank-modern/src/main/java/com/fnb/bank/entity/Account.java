package com.fnb.bank.entity;

import jakarta.persistence.*;
import java.math.BigDecimal;
import java.time.LocalDate;

@Entity
@Table(name = "acctmstr")
public class Account {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @Column(name = "acct_num", unique = true, nullable = false, length = 16)
    private String acctNum;

    @Column(name = "cif_id", nullable = false, length = 10)
    private String cifId;

    @Column(name = "prod_code", nullable = false, length = 4)
    private String prodCode;

    @Column(name = "prod_desc", length = 20)
    private String prodDesc;

    @Column(name = "status", length = 2)
    private String status;

    @Column(name = "open_dt")
    private LocalDate openDt;

    @Column(name = "close_dt")
    private LocalDate closeDt;

    @Column(name = "ledger_bal", precision = 13, scale = 2)
    private BigDecimal ledgerBal;

    @Column(name = "avail_bal", precision = 13, scale = 2)
    private BigDecimal availBal;

    @Column(name = "hold_bal", precision = 13, scale = 2)
    private BigDecimal holdBal;

    @Column(name = "uncoll_bal", precision = 13, scale = 2)
    private BigDecimal uncollBal;

    @Column(name = "od_limit", precision = 11, scale = 2)
    private BigDecimal odLimit;

    @Column(name = "credit_limit", precision = 13, scale = 2)
    private BigDecimal creditLimit;

    @Column(name = "int_rate", precision = 9, scale = 6)
    private BigDecimal intRate;

    @Column(name = "int_accrued", precision = 11, scale = 2)
    private BigDecimal intAccrued;

    @Column(name = "int_ytd", precision = 11, scale = 2)
    private BigDecimal intYtd;

    @Column(name = "int_prior_yr", precision = 11, scale = 2)
    private BigDecimal intPriorYr;

    @Column(name = "int_last_calc")
    private LocalDate intLastCalc;

    @Column(name = "maturity_dt")
    private LocalDate maturityDt;

    @Column(name = "cost_center", length = 6)
    private String costCenter;

    @Column(name = "gl_acct", length = 10)
    private String glAcct;

    @Column(name = "officer", length = 6)
    private String officer;

    @Column(name = "currency", length = 3)
    private String currency;

    @Column(name = "stmt_cycle", length = 2)
    private String stmtCycle;

    @Column(name = "stmt_last_dt")
    private LocalDate stmtLastDt;

    @Column(name = "restrict_cd", length = 2)
    private String restrictCd;

    @Column(name = "regd_ctr")
    private Integer regdCtr;

    @Column(name = "tax_rptg", length = 1)
    private String taxRptg;

    @Column(name = "branch", length = 6)
    private String branch;

    @Column(name = "last_activity_dt")
    private LocalDate lastActivityDt;

    @Column(name = "maint_dt")
    private LocalDate maintDt;

    @Column(name = "maint_user", length = 8)
    private String maintUser;

    public Long getId() { return id; }
    public void setId(Long id) { this.id = id; }
    public String getAcctNum() { return acctNum; }
    public void setAcctNum(String v) { this.acctNum = v; }
    public String getCifId() { return cifId; }
    public void setCifId(String v) { this.cifId = v; }
    public String getProdCode() { return prodCode; }
    public void setProdCode(String v) { this.prodCode = v; }
    public String getProdDesc() { return prodDesc; }
    public void setProdDesc(String v) { this.prodDesc = v; }
    public String getStatus() { return status; }
    public void setStatus(String v) { this.status = v; }
    public LocalDate getOpenDt() { return openDt; }
    public void setOpenDt(LocalDate v) { this.openDt = v; }
    public LocalDate getCloseDt() { return closeDt; }
    public void setCloseDt(LocalDate v) { this.closeDt = v; }
    public BigDecimal getLedgerBal() { return ledgerBal; }
    public void setLedgerBal(BigDecimal v) { this.ledgerBal = v; }
    public BigDecimal getAvailBal() { return availBal; }
    public void setAvailBal(BigDecimal v) { this.availBal = v; }
    public BigDecimal getHoldBal() { return holdBal; }
    public void setHoldBal(BigDecimal v) { this.holdBal = v; }
    public BigDecimal getUncollBal() { return uncollBal; }
    public void setUncollBal(BigDecimal v) { this.uncollBal = v; }
    public BigDecimal getOdLimit() { return odLimit; }
    public void setOdLimit(BigDecimal v) { this.odLimit = v; }
    public BigDecimal getCreditLimit() { return creditLimit; }
    public void setCreditLimit(BigDecimal v) { this.creditLimit = v; }
    public BigDecimal getIntRate() { return intRate; }
    public void setIntRate(BigDecimal v) { this.intRate = v; }
    public BigDecimal getIntAccrued() { return intAccrued; }
    public void setIntAccrued(BigDecimal v) { this.intAccrued = v; }
    public BigDecimal getIntYtd() { return intYtd; }
    public void setIntYtd(BigDecimal v) { this.intYtd = v; }
    public BigDecimal getIntPriorYr() { return intPriorYr; }
    public void setIntPriorYr(BigDecimal v) { this.intPriorYr = v; }
    public LocalDate getIntLastCalc() { return intLastCalc; }
    public void setIntLastCalc(LocalDate v) { this.intLastCalc = v; }
    public LocalDate getMaturityDt() { return maturityDt; }
    public void setMaturityDt(LocalDate v) { this.maturityDt = v; }
    public String getCostCenter() { return costCenter; }
    public void setCostCenter(String v) { this.costCenter = v; }
    public String getGlAcct() { return glAcct; }
    public void setGlAcct(String v) { this.glAcct = v; }
    public String getOfficer() { return officer; }
    public void setOfficer(String v) { this.officer = v; }
    public String getCurrency() { return currency; }
    public void setCurrency(String v) { this.currency = v; }
    public String getStmtCycle() { return stmtCycle; }
    public void setStmtCycle(String v) { this.stmtCycle = v; }
    public LocalDate getStmtLastDt() { return stmtLastDt; }
    public void setStmtLastDt(LocalDate v) { this.stmtLastDt = v; }
    public String getRestrictCd() { return restrictCd; }
    public void setRestrictCd(String v) { this.restrictCd = v; }
    public Integer getRegdCtr() { return regdCtr; }
    public void setRegdCtr(Integer v) { this.regdCtr = v; }
    public String getTaxRptg() { return taxRptg; }
    public void setTaxRptg(String v) { this.taxRptg = v; }
    public String getBranch() { return branch; }
    public void setBranch(String v) { this.branch = v; }
    public LocalDate getLastActivityDt() { return lastActivityDt; }
    public void setLastActivityDt(LocalDate v) { this.lastActivityDt = v; }
    public LocalDate getMaintDt() { return maintDt; }
    public void setMaintDt(LocalDate v) { this.maintDt = v; }
    public String getMaintUser() { return maintUser; }
    public void setMaintUser(String v) { this.maintUser = v; }
}
