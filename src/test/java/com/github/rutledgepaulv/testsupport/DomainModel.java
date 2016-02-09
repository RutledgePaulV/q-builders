package com.github.rutledgepaulv.testsupport;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

public class DomainModel {

    private Byte myByte;
    private Float myFloat;
    private Double myDouble;
    private Long myLong;
    private Integer myInteger;
    private Short myShort;
    private String myString;
    private String myString2;
    private Character myCharacter;
    private List<DomainModel> mySubList = new ArrayList<>();
    private List<String> myListOfStrings = new ArrayList<>();

    public Byte getMyByte() {
        return myByte;
    }

    public void setMyByte(Byte myByte) {
        this.myByte = myByte;
    }

    public Float getMyFloat() {
        return myFloat;
    }

    public void setMyFloat(Float myFloat) {
        this.myFloat = myFloat;
    }

    public Double getMyDouble() {
        return myDouble;
    }

    public void setMyDouble(Double myDouble) {
        this.myDouble = myDouble;
    }

    public Long getMyLong() {
        return myLong;
    }

    public void setMyLong(Long myLong) {
        this.myLong = myLong;
    }

    public Integer getMyInteger() {
        return myInteger;
    }

    public void setMyInteger(Integer myInteger) {
        this.myInteger = myInteger;
    }

    public Short getMyShort() {
        return myShort;
    }

    public void setMyShort(Short myShort) {
        this.myShort = myShort;
    }

    public String getMyString() {
        return myString;
    }

    public void setMyString(String myString) {
        this.myString = myString;
    }

    public Character getMyCharacter() {
        return myCharacter;
    }

    public void setMyCharacter(Character myCharacter) {
        this.myCharacter = myCharacter;
    }

    public List<String> getMyListOfStrings() {
        return myListOfStrings;
    }

    public void setMyListOfStrings(List<String> myListOfStrings) {
        this.myListOfStrings = myListOfStrings;
    }


    public String getMyString2() {
        return myString2;
    }

    public void setMyString2(String myString2) {
        this.myString2 = myString2;
    }

    public List<DomainModel> getMySubList() {
        return mySubList;
    }

    public void setMySubList(List<DomainModel> mySubList) {
        this.mySubList = mySubList;
    }

    public DomainModel copy() {
        DomainModel domainModel = new DomainModel();
        domainModel.setMyByte(getMyByte());
        domainModel.setMyCharacter(getMyCharacter());
        domainModel.setMyDouble(getMyDouble());
        domainModel.setMyFloat(getMyFloat());
        domainModel.setMyInteger(getMyInteger());
        domainModel.setMyListOfStrings(getMyListOfStrings().stream().collect(Collectors.toList()));
        domainModel.setMyLong(getMyLong());
        domainModel.setMyShort(getMyShort());
        domainModel.setMyString(getMyString());
        domainModel.setMyString2(getMyString2());
        domainModel.setMySubList(getMySubList().stream().map(DomainModel::copy).collect(Collectors.toList()));
        return domainModel;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) {
            return true;
        }
        if (o == null || getClass() != o.getClass()) {
            return false;
        }
        DomainModel domainModel = (DomainModel) o;
        return Objects.equals(myByte, domainModel.myByte) &&
                Objects.equals(myFloat, domainModel.myFloat) &&
                Objects.equals(myDouble, domainModel.myDouble) &&
                Objects.equals(myLong, domainModel.myLong) &&
                Objects.equals(myInteger, domainModel.myInteger) &&
                Objects.equals(myShort, domainModel.myShort) &&
                Objects.equals(myString, domainModel.myString) &&
                Objects.equals(myString2, domainModel.myString2) &&
                Objects.equals(myCharacter, domainModel.myCharacter) &&
                Objects.equals(mySubList, domainModel.mySubList) &&
                Objects.equals(myListOfStrings, domainModel.myListOfStrings);
    }

    @Override
    public int hashCode() {
        return Objects.hash(myByte, myFloat, myDouble, myLong, myInteger, myShort, myString, myString2, myCharacter,
                mySubList, myListOfStrings);
    }
}
