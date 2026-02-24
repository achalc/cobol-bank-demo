package com.fnb.bank.entity;

import jakarta.persistence.*;
import java.math.BigDecimal;
import java.time.LocalDate;

@Entity
@Table(name = "custmstr")
public class Customer {

    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @Column(name = "cif_id", unique = true, nullable = false, length = 10)
    private String cifId;

    @Column(name = "tax_id_type", length = 1)
    private String taxIdType;

    @Column(name = "tax_id", length = 11)
    private String taxId;

    @Column(name = "last_name", nullable = false, length = 25)
    private String lastName;

    @Column(name = "first_name", length = 20)
    private String firstName;

    @Column(name = "middle_initial", length = 1)
    private String middleInitial;

    @Column(name = "suffix", length = 4)
    private String suffix;

    @Column(name = "addr_line1", length = 30)
    private String addrLine1;

    @Column(name = "addr_line2", length = 30)
    private String addrLine2;

    @Column(name = "city", length = 20)
    private String city;

    @Column(name = "state", length = 2)
    private String state;

    @Column(name = "zip", length = 5)
    private String zip;

    @Column(name = "zip4", length = 4)
    private String zip4;

    @Column(name = "country", length = 3)
    private String country;

    @Column(name = "phone_primary", length = 12)
    private String phonePrimary;

    @Column(name = "phone_alt", length = 12)
    private String phoneAlt;

    @Column(name = "email", length = 40)
    private String email;

    @Column(name = "kyc_status", length = 1)
    private String kycStatus;

    @Column(name = "kyc_date")
    private LocalDate kycDate;

    @Column(name = "aml_risk", length = 1)
    private String amlRisk;

    @Column(name = "cra_code", length = 2)
    private String craCode;

    @Column(name = "rel_officer", length = 6)
    private String relOfficer;

    @Column(name = "segment", length = 2)
    private String segment;

    @Column(name = "status", length = 2)
    private String status;

    @Column(name = "open_date")
    private LocalDate openDate;

    @Column(name = "maint_date")
    private LocalDate maintDate;

    @Column(name = "maint_user", length = 8)
    private String maintUser;

    public Long getId() { return id; }
    public void setId(Long id) { this.id = id; }
    public String getCifId() { return cifId; }
    public void setCifId(String cifId) { this.cifId = cifId; }
    public String getTaxIdType() { return taxIdType; }
    public void setTaxIdType(String taxIdType) { this.taxIdType = taxIdType; }
    public String getTaxId() { return taxId; }
    public void setTaxId(String taxId) { this.taxId = taxId; }
    public String getLastName() { return lastName; }
    public void setLastName(String lastName) { this.lastName = lastName; }
    public String getFirstName() { return firstName; }
    public void setFirstName(String firstName) { this.firstName = firstName; }
    public String getMiddleInitial() { return middleInitial; }
    public void setMiddleInitial(String mi) { this.middleInitial = mi; }
    public String getSuffix() { return suffix; }
    public void setSuffix(String suffix) { this.suffix = suffix; }
    public String getAddrLine1() { return addrLine1; }
    public void setAddrLine1(String v) { this.addrLine1 = v; }
    public String getAddrLine2() { return addrLine2; }
    public void setAddrLine2(String v) { this.addrLine2 = v; }
    public String getCity() { return city; }
    public void setCity(String v) { this.city = v; }
    public String getState() { return state; }
    public void setState(String v) { this.state = v; }
    public String getZip() { return zip; }
    public void setZip(String v) { this.zip = v; }
    public String getZip4() { return zip4; }
    public void setZip4(String v) { this.zip4 = v; }
    public String getCountry() { return country; }
    public void setCountry(String v) { this.country = v; }
    public String getPhonePrimary() { return phonePrimary; }
    public void setPhonePrimary(String v) { this.phonePrimary = v; }
    public String getPhoneAlt() { return phoneAlt; }
    public void setPhoneAlt(String v) { this.phoneAlt = v; }
    public String getEmail() { return email; }
    public void setEmail(String v) { this.email = v; }
    public String getKycStatus() { return kycStatus; }
    public void setKycStatus(String v) { this.kycStatus = v; }
    public LocalDate getKycDate() { return kycDate; }
    public void setKycDate(LocalDate v) { this.kycDate = v; }
    public String getAmlRisk() { return amlRisk; }
    public void setAmlRisk(String v) { this.amlRisk = v; }
    public String getCraCode() { return craCode; }
    public void setCraCode(String v) { this.craCode = v; }
    public String getRelOfficer() { return relOfficer; }
    public void setRelOfficer(String v) { this.relOfficer = v; }
    public String getSegment() { return segment; }
    public void setSegment(String v) { this.segment = v; }
    public String getStatus() { return status; }
    public void setStatus(String v) { this.status = v; }
    public LocalDate getOpenDate() { return openDate; }
    public void setOpenDate(LocalDate v) { this.openDate = v; }
    public LocalDate getMaintDate() { return maintDate; }
    public void setMaintDate(LocalDate v) { this.maintDate = v; }
    public String getMaintUser() { return maintUser; }
    public void setMaintUser(String v) { this.maintUser = v; }
}
