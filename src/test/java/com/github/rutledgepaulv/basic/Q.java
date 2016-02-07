package com.github.rutledgepaulv.basic;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.stream.Collectors;

public class Q {

    private Byte myByte;
    private Float myFloat;
    private Double myDouble;
    private Long myLong;
    private Integer myInteger;
    private Short myShort;
    private String myString;
    private String myString2;
    private Character myCharacter;
    private List<Q> mySubList = new ArrayList<>();
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

    public List<Q> getMySubList() {
        return mySubList;
    }

    public void setMySubList(List<Q> mySubList) {
        this.mySubList = mySubList;
    }

    public Q copy() {
        Q q = new Q();
        q.setMyByte(getMyByte());
        q.setMyCharacter(getMyCharacter());
        q.setMyDouble(getMyDouble());
        q.setMyFloat(getMyFloat());
        q.setMyInteger(getMyInteger());
        q.setMyListOfStrings(getMyListOfStrings().stream().collect(Collectors.toList()));
        q.setMyLong(getMyLong());
        q.setMyShort(getMyShort());
        q.setMyString(getMyString());
        q.setMyString2(getMyString2());
        q.setMySubList(getMySubList().stream().map(Q::copy).collect(Collectors.toList()));
        return q;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) {
            return true;
        }
        if (o == null || getClass() != o.getClass()) {
            return false;
        }
        Q q = (Q) o;
        return Objects.equals(myByte, q.myByte) &&
                Objects.equals(myFloat, q.myFloat) &&
                Objects.equals(myDouble, q.myDouble) &&
                Objects.equals(myLong, q.myLong) &&
                Objects.equals(myInteger, q.myInteger) &&
                Objects.equals(myShort, q.myShort) &&
                Objects.equals(myString, q.myString) &&
                Objects.equals(myString2, q.myString2) &&
                Objects.equals(myCharacter, q.myCharacter) &&
                Objects.equals(mySubList, q.mySubList) &&
                Objects.equals(myListOfStrings, q.myListOfStrings);
    }

    @Override
    public int hashCode() {
        return Objects.hash(myByte, myFloat, myDouble, myLong, myInteger, myShort, myString, myString2, myCharacter,
                mySubList, myListOfStrings);
    }
}
