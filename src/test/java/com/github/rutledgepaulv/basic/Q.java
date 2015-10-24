package com.github.rutledgepaulv.basic;

import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

public class Q {

    private Byte myByte;
    private Float myFloat;
    private Double myDouble;
    private Long myLong;
    private Integer myInteger;
    private Short myShort;
    private String myString;
    private Character myCharacter;
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
                Objects.equals(myCharacter, q.myCharacter);
    }

    @Override
    public int hashCode() {
        return Objects.hash(myByte, myFloat, myDouble, myLong, myInteger, myShort, myString, myCharacter);
    }


}
