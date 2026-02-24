package com.fnb.bank.entity;

import jakarta.persistence.*;
import java.math.BigDecimal;
import java.time.LocalDate;

@Entity
@Table(name = "txnjrnl")
public class Transaction {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @Column(name = "txn_id", unique = true, nullable = false, length = 12)
    private String txnId;

    @Column(name = "acct_num", nullable = false, length = 16)
    private String acctNum;

    @Column(name = "txn_type", length = 3)
    private String txnType;

    @Column(name = "txn_desc", length = 40)
    private String txnDesc;

    @Column(name = "dc_ind", length = 1)
    private String dcInd;

    @Column(name = "amount", precision = 13, scale = 2)
    private BigDecimal amount;

    @Column(name = "run_bal", precision = 13, scale = 2)
    private BigDecimal runBal;

    @Column(name = "eff_dt")
    private LocalDate effDt;

    @Column(name = "post_dt")
    private LocalDate postDt;

    @Column(name = "channel", length = 3)
    private String channel;

    @Column(name = "teller_id", length = 6)
    private String tellerId;

    @Column(name = "terminal_id", length = 8)
    private String terminalId;

    @Column(name = "ref_num", length = 20)
    private String refNum;

    @Column(name = "status", length = 1)
    private String status;

    @Column(name = "reason_cd", length = 4)
    private String reasonCd;

    @Column(name = "gl_dr", length = 10)
    private String glDr;

    @Column(name = "gl_cr", length = 10)
    private String glCr;

    @Column(name = "reversal_flag", length = 1)
    private String reversalFlag;

    @Column(name = "orig_txn_id", length = 12)
    private String origTxnId;

    @Column(name = "batch_id", length = 10)
    private String batchId;

    public Long getId() { return id; }
    public void setId(Long id) { this.id = id; }
    public String getTxnId() { return txnId; }
    public void setTxnId(String v) { this.txnId = v; }
    public String getAcctNum() { return acctNum; }
    public void setAcctNum(String v) { this.acctNum = v; }
    public String getTxnType() { return txnType; }
    public void setTxnType(String v) { this.txnType = v; }
    public String getTxnDesc() { return txnDesc; }
    public void setTxnDesc(String v) { this.txnDesc = v; }
    public String getDcInd() { return dcInd; }
    public void setDcInd(String v) { this.dcInd = v; }
    public BigDecimal getAmount() { return amount; }
    public void setAmount(BigDecimal v) { this.amount = v; }
    public BigDecimal getRunBal() { return runBal; }
    public void setRunBal(BigDecimal v) { this.runBal = v; }
    public LocalDate getEffDt() { return effDt; }
    public void setEffDt(LocalDate v) { this.effDt = v; }
    public LocalDate getPostDt() { return postDt; }
    public void setPostDt(LocalDate v) { this.postDt = v; }
    public String getChannel() { return channel; }
    public void setChannel(String v) { this.channel = v; }
    public String getTellerId() { return tellerId; }
    public void setTellerId(String v) { this.tellerId = v; }
    public String getTerminalId() { return terminalId; }
    public void setTerminalId(String v) { this.terminalId = v; }
    public String getRefNum() { return refNum; }
    public void setRefNum(String v) { this.refNum = v; }
    public String getStatus() { return status; }
    public void setStatus(String v) { this.status = v; }
    public String getReasonCd() { return reasonCd; }
    public void setReasonCd(String v) { this.reasonCd = v; }
    public String getGlDr() { return glDr; }
    public void setGlDr(String v) { this.glDr = v; }
    public String getGlCr() { return glCr; }
    public void setGlCr(String v) { this.glCr = v; }
    public String getReversalFlag() { return reversalFlag; }
    public void setReversalFlag(String v) { this.reversalFlag = v; }
    public String getOrigTxnId() { return origTxnId; }
    public void setOrigTxnId(String v) { this.origTxnId = v; }
    public String getBatchId() { return batchId; }
    public void setBatchId(String v) { this.batchId = v; }
}
