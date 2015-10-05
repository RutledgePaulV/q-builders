package com.github.rutledgepaulv.base;

import com.github.rutledgepaulv.conditions.CompleteCondition;
import com.github.rutledgepaulv.visitors.NodeVisitor;
import org.junit.Test;

import static com.github.rutledgepaulv.base.QueryModel.QueryModelPredef.*;


public abstract class QueryBuilderTestBase<T extends NodeVisitor<S>, S> {

    protected abstract T getVisitor();
    protected abstract void compare(String expected, S converted);

    protected interface Simple {
        interface String {
            CompleteCondition<QueryModel> EQ = myString().eq("abcdefg");
            CompleteCondition<QueryModel> NE = myString().ne("abcdefg");
            CompleteCondition<QueryModel> LT = myString().lexicallyBefore("abcdefg");
            CompleteCondition<QueryModel> GT = myString().lexicallyAfter("abcdefg");
            CompleteCondition<QueryModel> EX = myString().exists();
            CompleteCondition<QueryModel> DNE = myString().doesNotExist();
        }

        interface Boolean {
            CompleteCondition<QueryModel> TRUE = myBoolean().isTrue();
            CompleteCondition<QueryModel> FALSE = myBoolean().isFalse();
            CompleteCondition<QueryModel> EX = myBoolean().exists();
            CompleteCondition<QueryModel> DNE = myBoolean().doesNotExist();
        }

        interface Short {
            CompleteCondition<QueryModel> EQ = myShort().eq((short)100);
            CompleteCondition<QueryModel> NE = myShort().ne((short)100);
            CompleteCondition<QueryModel> GT = myShort().gt((short)100);
            CompleteCondition<QueryModel> LT = myShort().lt((short)100);
            CompleteCondition<QueryModel> GTE = myShort().gte((short)100);
            CompleteCondition<QueryModel> LTE = myShort().lte((short)100);
            CompleteCondition<QueryModel> EX = myShort().exists();
            CompleteCondition<QueryModel> DNE = myShort().doesNotExist();
        }

        interface Integer {
            CompleteCondition<QueryModel> EQ = myInteger().eq(100);
            CompleteCondition<QueryModel> NE = myInteger().ne(100);
            CompleteCondition<QueryModel> GT = myInteger().gt(100);
            CompleteCondition<QueryModel> LT = myInteger().lt(100);
            CompleteCondition<QueryModel> GTE = myInteger().gte(100);
            CompleteCondition<QueryModel> LTE = myInteger().lte(100);
            CompleteCondition<QueryModel> EX = myInteger().exists();
            CompleteCondition<QueryModel> DNE = myInteger().doesNotExist();
        }

        interface Long {
            CompleteCondition<QueryModel> EQ = myLong().eq(100L);
            CompleteCondition<QueryModel> NE = myLong().ne(100L);
            CompleteCondition<QueryModel> GT = myLong().gt(100L);
            CompleteCondition<QueryModel> LT = myLong().lt(100L);
            CompleteCondition<QueryModel> GTE = myLong().gte(100L);
            CompleteCondition<QueryModel> LTE = myLong().lte(100L);
            CompleteCondition<QueryModel> EX = myLong().exists();
            CompleteCondition<QueryModel> DNE = myLong().doesNotExist();
        }

        interface Float {
            CompleteCondition<QueryModel> EQ = myFloat().eq(100f);
            CompleteCondition<QueryModel> NE = myFloat().ne(100f);
            CompleteCondition<QueryModel> GT = myFloat().gt(100f);
            CompleteCondition<QueryModel> LT = myFloat().lt(100f);
            CompleteCondition<QueryModel> GTE = myFloat().gte(100f);
            CompleteCondition<QueryModel> LTE = myFloat().lte(100f);
            CompleteCondition<QueryModel> EX = myFloat().exists();
            CompleteCondition<QueryModel> DNE = myFloat().doesNotExist();
        }

        interface Double {
            CompleteCondition<QueryModel> EQ = myDouble().eq(100.0);
            CompleteCondition<QueryModel> NE = myDouble().ne(100.0);
            CompleteCondition<QueryModel> GT = myDouble().gt(100.0);
            CompleteCondition<QueryModel> LT = myDouble().lt(100.0);
            CompleteCondition<QueryModel> GTE = myDouble().gte(100.0);
            CompleteCondition<QueryModel> LTE = myDouble().lte(100.0);
            CompleteCondition<QueryModel> EX = myDouble().exists();
            CompleteCondition<QueryModel> DNE = myDouble().doesNotExist();
        }
    }

    protected interface Logical {
        CompleteCondition<QueryModel> INLINE_ANDING = myString().eq("Thing").and().myLong().doesNotExist();
        CompleteCondition<QueryModel> INLINE_ORING = myString().eq("Thing").or().myLong().doesNotExist();
        CompleteCondition<QueryModel> LIST_ANDING = and(myString().eq("Thing"), myLong().doesNotExist());
        CompleteCondition<QueryModel> LIST_ORING = or(myString().eq("Thing"), myLong().doesNotExist());
        CompleteCondition<QueryModel> LIST_ORING_OF_INLINE_ANDING = or(myString().eq("Thing").and().myLong().doesNotExist(),
                                                                        myString().ne("Cats").and().myLong().gt(30L));

        CompleteCondition<QueryModel> LIST_ANDING_OF_INLINE_ORING = and(myString().eq("Thing").or().myLong().doesNotExist(),
                                                                        myString().ne("Cats").or().myLong().gt(30L));

        CompleteCondition<QueryModel> LIST_ANDING_OR_LIST_ORING = and(myString().eq("Thing").or().myLong().doesNotExist(),
                                                                        myString().ne("Cats").or().myLong().gt(30L))
                                                                    .or().or(myString().eq("Thing").and().myLong().doesNotExist(),
                                                                            myString().ne("Cats").and().myLong().gt(30L));

        CompleteCondition<QueryModel> LIST_ORING_ANDLIST_ANDING = or(myString().eq("Thing").and().myLong().doesNotExist(),
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

    protected void compare(String expected, CompleteCondition<QueryModel> condition) {
        T visitor = getVisitor();
        compare(expected, condition.query(visitor));
    }


}
