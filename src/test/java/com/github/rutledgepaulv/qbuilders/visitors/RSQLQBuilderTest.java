package com.github.rutledgepaulv.qbuilders.visitors;

import com.github.rutledgepaulv.base.QBuilderTestBase;

import static org.junit.Assert.assertEquals;

public class RSQLQBuilderTest extends QBuilderTestBase<RSQLVisitor, String> {

    public RSQLQBuilderTest() {

        String_EQ = "myString==abcdefg";
        String_NE = "myString!=abcdefg";
        String_LT = "myString=lt=abcdefg";
        String_GT = "myString=gt=abcdefg";
        String_EX = "myString=ex=true";
        String_DNE = "myString=ex=false";

        Boolean_TRUE = "myBoolean==true";
        Boolean_FALSE = "myBoolean==false";
        Boolean_EX = "myBoolean=ex=true";
        Boolean_DNE = "myBoolean=ex=false";

        Short_EQ = "myShort==100";
        Short_NE = "myShort!=100";
        Short_LT = "myShort=lt=100";
        Short_GT = "myShort=gt=100";
        Short_LTE = "myShort=le=100";
        Short_GTE = "myShort=ge=100";
        Short_EX = "myShort=ex=true";
        Short_DNE = "myShort=ex=false";

        Integer_EQ = "myInteger==100";
        Integer_NE = "myInteger!=100";
        Integer_LT = "myInteger=lt=100";
        Integer_GT = "myInteger=gt=100";
        Integer_LTE = "myInteger=le=100";
        Integer_GTE = "myInteger=ge=100";
        Integer_EX = "myInteger=ex=true";
        Integer_DNE = "myInteger=ex=false";

        Long_EQ = "myLong==100";
        Long_NE = "myLong!=100";
        Long_LT = "myLong=lt=100";
        Long_GT = "myLong=gt=100";
        Long_LTE = "myLong=le=100";
        Long_GTE = "myLong=ge=100";
        Long_EX = "myLong=ex=true";
        Long_DNE = "myLong=ex=false";

        Float_EQ = "myFloat==100.0";
        Float_NE = "myFloat!=100.0";
        Float_LT = "myFloat=lt=100.0";
        Float_GT = "myFloat=gt=100.0";
        Float_LTE = "myFloat=le=100.0";
        Float_GTE = "myFloat=ge=100.0";
        Float_EX = "myFloat=ex=true";
        Float_DNE = "myFloat=ex=false";

        Double_EQ = "myDouble==100.0";
        Double_NE = "myDouble!=100.0";
        Double_LT = "myDouble=lt=100.0";
        Double_GT = "myDouble=gt=100.0";
        Double_LTE = "myDouble=le=100.0";
        Double_GTE = "myDouble=ge=100.0";
        Double_EX = "myDouble=ex=true";
        Double_DNE = "myDouble=ex=false";


        INLINE_ANDING = "myString==Thing;myLong=ex=false";

        INLINE_ORING = "myString==Thing,myLong=ex=false";

        LIST_ANDING = "(myString==Thing;myLong=ex=false)";

        LIST_ORING = "(myString==Thing,myLong=ex=false)";

        LIST_ORING_OF_INLINE_ANDING = "((myString==Thing;myLong=ex=false),(myString!=Cats;myLong=gt=30))";

        LIST_ANDING_OF_INLINE_ORING = "((myString==Thing,myLong=ex=false);(myString!=Cats,myLong=gt=30))";

        LIST_ANDING_OR_LIST_ORING = "((myString==Thing,myLong=ex=false);(myString!=Cats,myLong=gt=30))," +
                "((myString==Thing;myLong=ex=false),(myString!=Cats;myLong=gt=30))";

        LIST_ORING_AND_LIST_ANDING = "((myString==Thing;myLong=ex=false),(myString!=Cats;myLong=gt=30));" +
                "((myString==Thing,myLong=ex=false);(myString!=Cats,myLong=gt=30))";
    }



    @Override
    protected RSQLVisitor getVisitor() {
        return new RSQLVisitor();
    }

    @Override
    protected void compare(String expected, String converted) {
        assertEquals(expected, converted);
    }

}
