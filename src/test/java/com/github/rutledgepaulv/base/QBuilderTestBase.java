package com.github.rutledgepaulv.base;

import com.github.rutledgepaulv.qbuilders.conditions.CompleteCondition;
import com.github.rutledgepaulv.qbuilders.visitors.NodeVisitor;
import org.junit.Test;

import static com.github.rutledgepaulv.base.QModel.QueryModelPredef.*;


public abstract class QBuilderTestBase<T extends NodeVisitor<S>, S> {

    protected abstract T getVisitor();
    protected abstract void compare(String expected, S converted);

    protected interface Simple {
        interface String {
            CompleteCondition<QModel> EQ = myString().eq("abcdefg");
            CompleteCondition<QModel> NE = myString().ne("abcdefg");
            CompleteCondition<QModel> LT = myString().lexicallyBefore("abcdefg");
            CompleteCondition<QModel> GT = myString().lexicallyAfter("abcdefg");
            CompleteCondition<QModel> EX = myString().exists();
            CompleteCondition<QModel> DNE = myString().doesNotExist();
        }

        interface Boolean {
            CompleteCondition<QModel> TRUE = myBoolean().isTrue();
            CompleteCondition<QModel> FALSE = myBoolean().isFalse();
            CompleteCondition<QModel> EX = myBoolean().exists();
            CompleteCondition<QModel> DNE = myBoolean().doesNotExist();
        }

        interface Short {
            CompleteCondition<QModel> EQ = myShort().eq((short)100);
            CompleteCondition<QModel> NE = myShort().ne((short)100);
            CompleteCondition<QModel> GT = myShort().gt((short)100);
            CompleteCondition<QModel> LT = myShort().lt((short)100);
            CompleteCondition<QModel> GTE = myShort().gte((short)100);
            CompleteCondition<QModel> LTE = myShort().lte((short)100);
            CompleteCondition<QModel> EX = myShort().exists();
            CompleteCondition<QModel> DNE = myShort().doesNotExist();
        }

        interface Integer {
            CompleteCondition<QModel> EQ = myInteger().eq(100);
            CompleteCondition<QModel> NE = myInteger().ne(100);
            CompleteCondition<QModel> GT = myInteger().gt(100);
            CompleteCondition<QModel> LT = myInteger().lt(100);
            CompleteCondition<QModel> GTE = myInteger().gte(100);
            CompleteCondition<QModel> LTE = myInteger().lte(100);
            CompleteCondition<QModel> EX = myInteger().exists();
            CompleteCondition<QModel> DNE = myInteger().doesNotExist();
        }

        interface Long {
            CompleteCondition<QModel> EQ = myLong().eq(100L);
            CompleteCondition<QModel> NE = myLong().ne(100L);
            CompleteCondition<QModel> GT = myLong().gt(100L);
            CompleteCondition<QModel> LT = myLong().lt(100L);
            CompleteCondition<QModel> GTE = myLong().gte(100L);
            CompleteCondition<QModel> LTE = myLong().lte(100L);
            CompleteCondition<QModel> EX = myLong().exists();
            CompleteCondition<QModel> DNE = myLong().doesNotExist();
        }

        interface Float {
            CompleteCondition<QModel> EQ = myFloat().eq(100f);
            CompleteCondition<QModel> NE = myFloat().ne(100f);
            CompleteCondition<QModel> GT = myFloat().gt(100f);
            CompleteCondition<QModel> LT = myFloat().lt(100f);
            CompleteCondition<QModel> GTE = myFloat().gte(100f);
            CompleteCondition<QModel> LTE = myFloat().lte(100f);
            CompleteCondition<QModel> EX = myFloat().exists();
            CompleteCondition<QModel> DNE = myFloat().doesNotExist();
        }

        interface Double {
            CompleteCondition<QModel> EQ = myDouble().eq(100.0);
            CompleteCondition<QModel> NE = myDouble().ne(100.0);
            CompleteCondition<QModel> GT = myDouble().gt(100.0);
            CompleteCondition<QModel> LT = myDouble().lt(100.0);
            CompleteCondition<QModel> GTE = myDouble().gte(100.0);
            CompleteCondition<QModel> LTE = myDouble().lte(100.0);
            CompleteCondition<QModel> EX = myDouble().exists();
            CompleteCondition<QModel> DNE = myDouble().doesNotExist();
        }
    }

    protected interface Logical {
        CompleteCondition<QModel> INLINE_ANDING = myString().eq("Thing").and().myLong().doesNotExist();
        CompleteCondition<QModel> INLINE_ORING = myString().eq("Thing").or().myLong().doesNotExist();
        CompleteCondition<QModel> LIST_ANDING = and(myString().eq("Thing"), myLong().doesNotExist());
        CompleteCondition<QModel> LIST_ORING = or(myString().eq("Thing"), myLong().doesNotExist());
        CompleteCondition<QModel> LIST_ORING_OF_INLINE_ANDING = or(myString().eq("Thing").and().myLong().doesNotExist(),
                                                                        myString().ne("Cats").and().myLong().gt(30L));

        CompleteCondition<QModel> LIST_ANDING_OF_INLINE_ORING = and(myString().eq("Thing").or().myLong().doesNotExist(),
                                                                        myString().ne("Cats").or().myLong().gt(30L));

        CompleteCondition<QModel> LIST_ANDING_OR_LIST_ORING = and(myString().eq("Thing").or().myLong().doesNotExist(),
                                                                        myString().ne("Cats").or().myLong().gt(30L))
                                                                    .or().or(myString().eq("Thing").and().myLong().doesNotExist(),
                                                                            myString().ne("Cats").and().myLong().gt(30L));

        CompleteCondition<QModel> LIST_ORING_ANDLIST_ANDING = or(myString().eq("Thing").and().myLong().doesNotExist(),
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

    @Test
    public void simple_String() {
        compare(String_EQ, Simple.String.EQ);
        compare(String_NE, Simple.String.NE);
        compare(String_LT, Simple.String.LT);
        compare(String_GT, Simple.String.GT);
        compare(String_EX, Simple.String.EX);
        compare(String_DNE, Simple.String.DNE);
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
    }

    protected String Integer_EQ;
    protected String Integer_NE;
    protected String Integer_LT;
    protected String Integer_GT;
    protected String Integer_LTE;
    protected String Integer_GTE;
    protected String Integer_EX;
    protected String Integer_DNE;

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
    }

    protected String Long_EQ;
    protected String Long_NE;
    protected String Long_LT;
    protected String Long_GT;
    protected String Long_LTE;
    protected String Long_GTE;
    protected String Long_EX;
    protected String Long_DNE;

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
    }

    protected String Float_EQ;
    protected String Float_NE;
    protected String Float_LT;
    protected String Float_GT;
    protected String Float_LTE;
    protected String Float_GTE;
    protected String Float_EX;
    protected String Float_DNE;

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
    }

    protected String Double_EQ;
    protected String Double_NE;
    protected String Double_LT;
    protected String Double_GT;
    protected String Double_LTE;
    protected String Double_GTE;
    protected String Double_EX;
    protected String Double_DNE;

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

    protected void compare(String expected, CompleteCondition<QModel> condition) {
        T visitor = getVisitor();
        compare(expected, condition.query(visitor));
    }


}
