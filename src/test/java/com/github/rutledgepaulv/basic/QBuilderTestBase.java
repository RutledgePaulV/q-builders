package com.github.rutledgepaulv.basic;

import com.github.rutledgepaulv.basic.qbuilders.conditions.Condition;
import com.github.rutledgepaulv.basic.qbuilders.visitors.NodeVisitor;
import org.junit.Test;

import static com.github.rutledgepaulv.basic.QModel.QueryModelPredef.*;


public abstract class QBuilderTestBase<T extends NodeVisitor<S>, S> {

    protected abstract T getVisitor();
    protected abstract void compare(String expected, S converted);

    protected interface Simple {
        interface String {
            Condition<QModel> EQ = myString().eq("abcdefg");
            Condition<QModel> NE = myString().ne("abcdefg");
            Condition<QModel> LT = myString().lexicallyBefore("abcdefg");
            Condition<QModel> GT = myString().lexicallyAfter("abcdefg");
            Condition<QModel> EX = myString().exists();
            Condition<QModel> DNE = myString().doesNotExist();
            Condition<QModel> IN = myString().in("a", "b", "c");
            Condition<QModel> NIN = myString().nin("d", "e", "f");
        }

        interface Boolean {
            Condition<QModel> TRUE = myBoolean().isTrue();
            Condition<QModel> FALSE = myBoolean().isFalse();
            Condition<QModel> EX = myBoolean().exists();
            Condition<QModel> DNE = myBoolean().doesNotExist();
        }

        interface Short {
            Condition<QModel> EQ = myShort().eq((short)100);
            Condition<QModel> NE = myShort().ne((short)100);
            Condition<QModel> GT = myShort().gt((short)100);
            Condition<QModel> LT = myShort().lt((short)100);
            Condition<QModel> GTE = myShort().gte((short)100);
            Condition<QModel> LTE = myShort().lte((short)100);
            Condition<QModel> EX = myShort().exists();
            Condition<QModel> DNE = myShort().doesNotExist();
            Condition<QModel> IN = myShort().in((short)98, (short)99, (short)100);
            Condition<QModel> NIN = myShort().nin((short)101, (short)102, (short)103);
        }

        interface Integer {
            Condition<QModel> EQ = myInteger().eq(100);
            Condition<QModel> NE = myInteger().ne(100);
            Condition<QModel> GT = myInteger().gt(100);
            Condition<QModel> LT = myInteger().lt(100);
            Condition<QModel> GTE = myInteger().gte(100);
            Condition<QModel> LTE = myInteger().lte(100);
            Condition<QModel> EX = myInteger().exists();
            Condition<QModel> DNE = myInteger().doesNotExist();
            Condition<QModel> IN = myInteger().in(98, 99, 100);
            Condition<QModel> NIN = myInteger().nin(101, 102, 103);
        }

        interface Long {
            Condition<QModel> EQ = myLong().eq(100L);
            Condition<QModel> NE = myLong().ne(100L);
            Condition<QModel> GT = myLong().gt(100L);
            Condition<QModel> LT = myLong().lt(100L);
            Condition<QModel> GTE = myLong().gte(100L);
            Condition<QModel> LTE = myLong().lte(100L);
            Condition<QModel> EX = myLong().exists();
            Condition<QModel> DNE = myLong().doesNotExist();
            Condition<QModel> IN = myLong().in(98L, 99L, 100L);
            Condition<QModel> NIN = myLong().nin(101L, 102L, 103L);
        }

        interface Float {
            Condition<QModel> EQ = myFloat().eq(100f);
            Condition<QModel> NE = myFloat().ne(100f);
            Condition<QModel> GT = myFloat().gt(100f);
            Condition<QModel> LT = myFloat().lt(100f);
            Condition<QModel> GTE = myFloat().gte(100f);
            Condition<QModel> LTE = myFloat().lte(100f);
            Condition<QModel> EX = myFloat().exists();
            Condition<QModel> DNE = myFloat().doesNotExist();
            Condition<QModel> IN = myFloat().in(98f, 99f, 100f);
            Condition<QModel> NIN = myFloat().nin(101f, 102f, 103f);
        }

        interface Double {
            Condition<QModel> EQ = myDouble().eq(100.0);
            Condition<QModel> NE = myDouble().ne(100.0);
            Condition<QModel> GT = myDouble().gt(100.0);
            Condition<QModel> LT = myDouble().lt(100.0);
            Condition<QModel> GTE = myDouble().gte(100.0);
            Condition<QModel> LTE = myDouble().lte(100.0);
            Condition<QModel> EX = myDouble().exists();
            Condition<QModel> DNE = myDouble().doesNotExist();
            Condition<QModel> IN = myDouble().in(98.0, 99.0, 100.0);
            Condition<QModel> NIN = myDouble().nin(101.0, 102.0, 103.0);
        }
    }

    protected interface Logical {
        Condition<QModel> INLINE_ANDING = myString().eq("Thing").and().myLong().doesNotExist();
        Condition<QModel> INLINE_ORING = myString().eq("Thing").or().myLong().doesNotExist();
        Condition<QModel> LIST_ANDING = and(myString().eq("Thing"), myLong().doesNotExist());
        Condition<QModel> LIST_ORING = or(myString().eq("Thing"), myLong().doesNotExist());
        Condition<QModel> LIST_ORING_OF_INLINE_ANDING = or(myString().eq("Thing").and().myLong().doesNotExist(),
                                                                        myString().ne("Cats").and().myLong().gt(30L));

        Condition<QModel> LIST_ANDING_OF_INLINE_ORING = and(myString().eq("Thing").or().myLong().doesNotExist(),
                                                                        myString().ne("Cats").or().myLong().gt(30L));

        Condition<QModel> LIST_ANDING_OR_LIST_ORING = and(myString().eq("Thing").or().myLong().doesNotExist(),
                                                                        myString().ne("Cats").or().myLong().gt(30L))
                                                                    .or().or(myString().eq("Thing").and().myLong().doesNotExist(),
                                                                            myString().ne("Cats").and().myLong().gt(30L));

        Condition<QModel> LIST_ORING_ANDLIST_ANDING = or(myString().eq("Thing").and().myLong().doesNotExist(),
                                                                        myString().ne("Cats").and().myLong().gt(30L))
                                                                    .and().and(myString().eq("Thing").or().myLong().doesNotExist(),
                                                                            myString().ne("Cats").or().myLong().gt(30L));
    }

    protected String String_EQ;
    protected String String_NE;
    protected String String_LT;
    protected String String_GT;
    protected String String_EX;
    protected String String_DNE;
    protected String String_IN;
    protected String String_NIN;

    @Test
    public void simple_String() {
        compare(String_EQ, Simple.String.EQ);
        compare(String_NE, Simple.String.NE);
        compare(String_LT, Simple.String.LT);
        compare(String_GT, Simple.String.GT);
        compare(String_EX, Simple.String.EX);
        compare(String_DNE, Simple.String.DNE);
        compare(String_IN, Simple.String.IN);
        compare(String_NIN, Simple.String.NIN);
    }

    protected String Boolean_TRUE;
    protected String Boolean_FALSE;
    protected String Boolean_EX;
    protected String Boolean_DNE;

    @Test
    public void simple_Boolean() {
        compare(Boolean_TRUE, Simple.Boolean.TRUE);
        compare(Boolean_FALSE, Simple.Boolean.FALSE);
        compare(Boolean_EX, Simple.Boolean.EX);
        compare(Boolean_DNE, Simple.Boolean.DNE);
    }

    protected String Short_EQ;
    protected String Short_NE;
    protected String Short_LT;
    protected String Short_GT;
    protected String Short_LTE;
    protected String Short_GTE;
    protected String Short_EX;
    protected String Short_DNE;
    protected String Short_IN;
    protected String Short_NIN;

    @Test
    public void simple_Short() {
        compare(Short_EQ, Simple.Short.EQ);
        compare(Short_NE, Simple.Short.NE);
        compare(Short_LT, Simple.Short.LT);
        compare(Short_LTE, Simple.Short.LTE);
        compare(Short_GT, Simple.Short.GT);
        compare(Short_GTE, Simple.Short.GTE);
        compare(Short_EX, Simple.Short.EX);
        compare(Short_DNE, Simple.Short.DNE);
        compare(Short_IN, Simple.Short.IN);
        compare(Short_NIN, Simple.Short.NIN);
    }

    protected String Integer_EQ;
    protected String Integer_NE;
    protected String Integer_LT;
    protected String Integer_GT;
    protected String Integer_LTE;
    protected String Integer_GTE;
    protected String Integer_EX;
    protected String Integer_DNE;
    protected String Integer_IN;
    protected String Integer_NIN;

    @Test
    public void simple_Integer() {
        compare(Integer_EQ, Simple.Integer.EQ);
        compare(Integer_NE, Simple.Integer.NE);
        compare(Integer_LT, Simple.Integer.LT);
        compare(Integer_LTE, Simple.Integer.LTE);
        compare(Integer_GT, Simple.Integer.GT);
        compare(Integer_GTE, Simple.Integer.GTE);
        compare(Integer_EX, Simple.Integer.EX);
        compare(Integer_DNE, Simple.Integer.DNE);
        compare(Integer_IN, Simple.Integer.IN);
        compare(Integer_NIN, Simple.Integer.NIN);
    }

    protected String Long_EQ;
    protected String Long_NE;
    protected String Long_LT;
    protected String Long_GT;
    protected String Long_LTE;
    protected String Long_GTE;
    protected String Long_EX;
    protected String Long_DNE;
    protected String Long_IN;
    protected String Long_NIN;

    @Test
    public void simple_Long() {
        compare(Long_EQ, Simple.Long.EQ);
        compare(Long_NE, Simple.Long.NE);
        compare(Long_LT, Simple.Long.LT);
        compare(Long_LTE, Simple.Long.LTE);
        compare(Long_GT, Simple.Long.GT);
        compare(Long_GTE, Simple.Long.GTE);
        compare(Long_EX, Simple.Long.EX);
        compare(Long_DNE, Simple.Long.DNE);
        compare(Long_IN, Simple.Long.IN);
        compare(Long_NIN, Simple.Long.NIN);
    }

    protected String Float_EQ;
    protected String Float_NE;
    protected String Float_LT;
    protected String Float_GT;
    protected String Float_LTE;
    protected String Float_GTE;
    protected String Float_EX;
    protected String Float_DNE;
    protected String Float_IN;
    protected String Float_NIN;

    @Test
    public void simple_Float() {
        compare(Float_EQ, Simple.Float.EQ);
        compare(Float_NE, Simple.Float.NE);
        compare(Float_LT, Simple.Float.LT);
        compare(Float_LTE, Simple.Float.LTE);
        compare(Float_GT, Simple.Float.GT);
        compare(Float_GTE, Simple.Float.GTE);
        compare(Float_EX, Simple.Float.EX);
        compare(Float_DNE, Simple.Float.DNE);
        compare(Float_IN, Simple.Float.IN);
        compare(Float_NIN, Simple.Float.NIN);
    }

    protected String Double_EQ;
    protected String Double_NE;
    protected String Double_LT;
    protected String Double_GT;
    protected String Double_LTE;
    protected String Double_GTE;
    protected String Double_EX;
    protected String Double_DNE;
    protected String Double_IN;
    protected String Double_NIN;

    @Test
    public void simple_Double() {
        compare(Double_EQ, Simple.Double.EQ);
        compare(Double_NE, Simple.Double.NE);
        compare(Double_LT, Simple.Double.LT);
        compare(Double_LTE, Simple.Double.LTE);
        compare(Double_GT, Simple.Double.GT);
        compare(Double_GTE, Simple.Double.GTE);
        compare(Double_EX, Simple.Double.EX);
        compare(Double_DNE, Simple.Double.DNE);
        compare(Double_IN, Simple.Double.IN);
        compare(Double_NIN, Simple.Double.NIN);
    }

    protected String INLINE_ANDING;

    @Test
    public void inline_Anding() {
        compare(INLINE_ANDING, Logical.INLINE_ANDING);
    }

    protected String INLINE_ORING;

    @Test
    public void inline_Oring() {
        compare(INLINE_ORING, Logical.INLINE_ORING);
    }

    protected String LIST_ANDING;

    @Test
    public void list_Anding() {
        compare(LIST_ANDING, Logical.LIST_ANDING);
    }

    protected String LIST_ORING;

    @Test
    public void list_Oring() {
        compare(LIST_ORING, Logical.LIST_ORING);
    }


    protected String LIST_ORING_OF_INLINE_ANDING;

    @Test
    public void listOringOfInlineAnding() {
        compare(LIST_ORING_OF_INLINE_ANDING, Logical.LIST_ORING_OF_INLINE_ANDING);
    }

    protected String LIST_ANDING_OF_INLINE_ORING;

    @Test
    public void listAndingOfInlineOring() {
        compare(LIST_ANDING_OF_INLINE_ORING, Logical.LIST_ANDING_OF_INLINE_ORING);
    }

    protected String LIST_ANDING_OR_LIST_ORING;

    @Test
    public void listAndingOrListOring() {
        compare(LIST_ANDING_OR_LIST_ORING, Logical.LIST_ANDING_OR_LIST_ORING);
    }

    protected String LIST_ORING_AND_LIST_ANDING;

    @Test
    public void listOringAndListAnding() {
        compare(LIST_ORING_AND_LIST_ANDING, Logical.LIST_ORING_ANDLIST_ANDING);
    }

    protected void compare(String expected, Condition<QModel> condition) {
        T visitor = getVisitor();
        compare(expected, condition.query(visitor));
    }


}
