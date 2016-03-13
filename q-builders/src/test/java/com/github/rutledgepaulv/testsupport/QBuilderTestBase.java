package com.github.rutledgepaulv.testsupport;

import com.github.rutledgepaulv.qbuilders.conditions.Condition;
import com.github.rutledgepaulv.qbuilders.visitors.NodeVisitor;
import org.junit.Test;

import java.time.OffsetDateTime;
import java.time.ZoneOffset;

import static com.github.rutledgepaulv.testsupport.DomainModel.MyEnum.*;


public abstract class QBuilderTestBase<T extends NodeVisitor<S>, S> {

    protected abstract T getVisitor();

    protected abstract void compare(String expected, S converted);

    protected interface Simple {
        interface String {
            Condition<QueryModel> EQ = QueryModel.QueryModelPredef.myString().eq("abcdefg");
            Condition<QueryModel> NE = QueryModel.QueryModelPredef.myString().ne("abcdefg");
            Condition<QueryModel> LT = QueryModel.QueryModelPredef.myString().lexicallyBefore("abcdefg");
            Condition<QueryModel> GT = QueryModel.QueryModelPredef.myString().lexicallyAfter("abcdefg");
            Condition<QueryModel> GTE = QueryModel.QueryModelPredef.myString().lexicallyNotBefore("abcdefg");
            Condition<QueryModel> LTE = QueryModel.QueryModelPredef.myString().lexicallyNotAfter("abcdefg");
            Condition<QueryModel> EX = QueryModel.QueryModelPredef.myString().exists();
            Condition<QueryModel> DNE = QueryModel.QueryModelPredef.myString().doesNotExist();
            Condition<QueryModel> IN = QueryModel.QueryModelPredef.myString().in("a", "b", "c");
            Condition<QueryModel> NIN = QueryModel.QueryModelPredef.myString().nin("d", "e", "f");
        }

        interface Enum {
            Condition<QueryModel> EQ = QueryModel.QueryModelPredef.myEnum().eq(VALUE1);
            Condition<QueryModel> NE = QueryModel.QueryModelPredef.myEnum().ne(VALUE1);
            Condition<QueryModel> EX = QueryModel.QueryModelPredef.myEnum().exists();
            Condition<QueryModel> DNE = QueryModel.QueryModelPredef.myEnum().doesNotExist();
            Condition<QueryModel> IN = QueryModel.QueryModelPredef.myEnum().in(VALUE1, VALUE2, VALUE3);
            Condition<QueryModel> NIN = QueryModel.QueryModelPredef.myEnum().nin(VALUE1, VALUE2, VALUE3);
        }

        interface Boolean {
            Condition<QueryModel> TRUE = QueryModel.QueryModelPredef.myBoolean().isTrue();
            Condition<QueryModel> FALSE = QueryModel.QueryModelPredef.myBoolean().isFalse();
            Condition<QueryModel> EX = QueryModel.QueryModelPredef.myBoolean().exists();
            Condition<QueryModel> DNE = QueryModel.QueryModelPredef.myBoolean().doesNotExist();
        }

        interface Short {
            Condition<QueryModel> EQ = QueryModel.QueryModelPredef.myShort().eq((short) 100);
            Condition<QueryModel> NE = QueryModel.QueryModelPredef.myShort().ne((short) 100);
            Condition<QueryModel> GT = QueryModel.QueryModelPredef.myShort().gt((short) 100);
            Condition<QueryModel> LT = QueryModel.QueryModelPredef.myShort().lt((short) 100);
            Condition<QueryModel> GTE = QueryModel.QueryModelPredef.myShort().gte((short) 100);
            Condition<QueryModel> LTE = QueryModel.QueryModelPredef.myShort().lte((short) 100);
            Condition<QueryModel> EX = QueryModel.QueryModelPredef.myShort().exists();
            Condition<QueryModel> DNE = QueryModel.QueryModelPredef.myShort().doesNotExist();
            Condition<QueryModel> IN = QueryModel.QueryModelPredef.myShort().in((short) 98, (short) 99, (short) 100);
            Condition<QueryModel> NIN = QueryModel.QueryModelPredef.myShort().nin((short) 101, (short) 102, (short) 103);
        }

        interface Integer {
            Condition<QueryModel> EQ = QueryModel.QueryModelPredef.myInteger().eq(100);
            Condition<QueryModel> NE = QueryModel.QueryModelPredef.myInteger().ne(100);
            Condition<QueryModel> GT = QueryModel.QueryModelPredef.myInteger().gt(100);
            Condition<QueryModel> LT = QueryModel.QueryModelPredef.myInteger().lt(100);
            Condition<QueryModel> GTE = QueryModel.QueryModelPredef.myInteger().gte(100);
            Condition<QueryModel> LTE = QueryModel.QueryModelPredef.myInteger().lte(100);
            Condition<QueryModel> EX = QueryModel.QueryModelPredef.myInteger().exists();
            Condition<QueryModel> DNE = QueryModel.QueryModelPredef.myInteger().doesNotExist();
            Condition<QueryModel> IN = QueryModel.QueryModelPredef.myInteger().in(98, 99, 100);
            Condition<QueryModel> NIN = QueryModel.QueryModelPredef.myInteger().nin(101, 102, 103);
        }

        interface Long {
            Condition<QueryModel> EQ = QueryModel.QueryModelPredef.myLong().eq(100L);
            Condition<QueryModel> NE = QueryModel.QueryModelPredef.myLong().ne(100L);
            Condition<QueryModel> GT = QueryModel.QueryModelPredef.myLong().gt(100L);
            Condition<QueryModel> LT = QueryModel.QueryModelPredef.myLong().lt(100L);
            Condition<QueryModel> GTE = QueryModel.QueryModelPredef.myLong().gte(100L);
            Condition<QueryModel> LTE = QueryModel.QueryModelPredef.myLong().lte(100L);
            Condition<QueryModel> EX = QueryModel.QueryModelPredef.myLong().exists();
            Condition<QueryModel> DNE = QueryModel.QueryModelPredef.myLong().doesNotExist();
            Condition<QueryModel> IN = QueryModel.QueryModelPredef.myLong().in(98L, 99L, 100L);
            Condition<QueryModel> NIN = QueryModel.QueryModelPredef.myLong().nin(101L, 102L, 103L);
        }

        interface Float {
            Condition<QueryModel> EQ = QueryModel.QueryModelPredef.myFloat().eq(100f);
            Condition<QueryModel> NE = QueryModel.QueryModelPredef.myFloat().ne(100f);
            Condition<QueryModel> GT = QueryModel.QueryModelPredef.myFloat().gt(100f);
            Condition<QueryModel> LT = QueryModel.QueryModelPredef.myFloat().lt(100f);
            Condition<QueryModel> GTE = QueryModel.QueryModelPredef.myFloat().gte(100f);
            Condition<QueryModel> LTE = QueryModel.QueryModelPredef.myFloat().lte(100f);
            Condition<QueryModel> EX = QueryModel.QueryModelPredef.myFloat().exists();
            Condition<QueryModel> DNE = QueryModel.QueryModelPredef.myFloat().doesNotExist();
            Condition<QueryModel> IN = QueryModel.QueryModelPredef.myFloat().in(98f, 99f, 100f);
            Condition<QueryModel> NIN = QueryModel.QueryModelPredef.myFloat().nin(101f, 102f, 103f);
        }

        interface Double {
            Condition<QueryModel> EQ = QueryModel.QueryModelPredef.myDouble().eq(100.0);
            Condition<QueryModel> NE = QueryModel.QueryModelPredef.myDouble().ne(100.0);
            Condition<QueryModel> GT = QueryModel.QueryModelPredef.myDouble().gt(100.0);
            Condition<QueryModel> LT = QueryModel.QueryModelPredef.myDouble().lt(100.0);
            Condition<QueryModel> GTE = QueryModel.QueryModelPredef.myDouble().gte(100.0);
            Condition<QueryModel> LTE = QueryModel.QueryModelPredef.myDouble().lte(100.0);
            Condition<QueryModel> EX = QueryModel.QueryModelPredef.myDouble().exists();
            Condition<QueryModel> DNE = QueryModel.QueryModelPredef.myDouble().doesNotExist();
            Condition<QueryModel> IN = QueryModel.QueryModelPredef.myDouble().in(98.0, 99.0, 100.0);
            Condition<QueryModel> NIN = QueryModel.QueryModelPredef.myDouble().nin(101.0, 102.0, 103.0);
        }

        interface Instant {
            java.time.Instant epoch = OffsetDateTime.of(1970, 1, 1, 0, 0, 0, 0, ZoneOffset.ofHours(0))
                    .toInstant();

            java.time.Instant yearAfterEpoch = OffsetDateTime.of(1970, 1, 1, 0, 0, 0, 0, ZoneOffset.ofHours(0))
                    .plusYears(1).toInstant();

            Condition<QueryModel> EQ = QueryModel.QueryModelPredef.myDateTime().eq(epoch);
            Condition<QueryModel> NE = QueryModel.QueryModelPredef.myDateTime().ne(epoch);
            Condition<QueryModel> GT = QueryModel.QueryModelPredef.myDateTime().after(epoch, true);
            Condition<QueryModel> GTE = QueryModel.QueryModelPredef.myDateTime().after(epoch, false);
            Condition<QueryModel> LT = QueryModel.QueryModelPredef.myDateTime().before(yearAfterEpoch, true);
            Condition<QueryModel> LTE = QueryModel.QueryModelPredef.myDateTime().before(yearAfterEpoch, false);
            Condition<QueryModel> BETWEEN = QueryModel.QueryModelPredef.myDateTime().between(epoch, false, yearAfterEpoch, false);
            Condition<QueryModel> EX = QueryModel.QueryModelPredef.myDateTime().exists();
            Condition<QueryModel> DNE = QueryModel.QueryModelPredef.myDateTime().doesNotExist();
        }
    }

    protected interface Logical {
        Condition<QueryModel> INLINE_ANDING = QueryModel.QueryModelPredef.myString().eq("Thing").and().myLong().doesNotExist();
        Condition<QueryModel> INLINE_ORING = QueryModel.QueryModelPredef.myString().eq("Thing").or().myLong().doesNotExist();
        Condition<QueryModel> LIST_ANDING = QueryModel.QueryModelPredef
                .and(QueryModel.QueryModelPredef.myString().eq("Thing"), QueryModel.QueryModelPredef.myLong().doesNotExist());
        Condition<QueryModel> LIST_ORING = QueryModel.QueryModelPredef
                .or(QueryModel.QueryModelPredef.myString().eq("Thing"), QueryModel.QueryModelPredef.myLong().doesNotExist());
        Condition<QueryModel> LIST_ORING_OF_INLINE_ANDING = QueryModel.QueryModelPredef
                .or(QueryModel.QueryModelPredef.myString().eq("Thing").and().myLong().doesNotExist(),
                QueryModel.QueryModelPredef.myString().ne("Cats").and().myLong().gt(30L));

        Condition<QueryModel> LIST_ANDING_OF_INLINE_ORING = QueryModel.QueryModelPredef
                .and(QueryModel.QueryModelPredef.myString().eq("Thing").or().myLong().doesNotExist(),
                QueryModel.QueryModelPredef.myString().ne("Cats").or().myLong().gt(30L));

        Condition<QueryModel> LIST_ANDING_OR_LIST_ORING = QueryModel.QueryModelPredef
                .and(QueryModel.QueryModelPredef.myString().eq("Thing").or().myLong().doesNotExist(),
                QueryModel.QueryModelPredef.myString().ne("Cats").or().myLong().gt(30L)).or()
                .or(QueryModel.QueryModelPredef
                        .myString().eq("Thing").and().myLong().doesNotExist(), QueryModel.QueryModelPredef.myString().ne("Cats").and().myLong().gt(30L));

        Condition<QueryModel> LIST_ORING_ANDLIST_ANDING = QueryModel.QueryModelPredef
                .or(QueryModel.QueryModelPredef.myString().eq("Thing").and().myLong().doesNotExist(),
                QueryModel.QueryModelPredef.myString().ne("Cats").and().myLong().gt(30L)).and()
                .and(QueryModel.QueryModelPredef
                        .myString().eq("Thing").or().myLong().doesNotExist(), QueryModel.QueryModelPredef.myString().ne("Cats").or().myLong().gt(30L));
    }


    protected interface Chained {
            Condition<QueryModel> CHAINED_ANDS = QueryModel.QueryModelPredef.myString().eq("thing").and().myInteger().gt(0)
                    .and().myInteger().lt(5).and().myLong().in(0L, 1L, 2L).and().myDouble().lte(2.9)
                    .and().myBoolean().isFalse().and().myDateTime().doesNotExist();

            Condition<QueryModel> CHAINED_ORS = QueryModel.QueryModelPredef.myString().eq("thing").or().myInteger().gt(0).or()
                    .myInteger().lt(5).or().myLong().in(0L, 1L, 2L).or().myDouble().lte(2.9).or()
                    .myBoolean().isFalse().or().myDateTime().doesNotExist();

            Condition<QueryModel> CHAINED_ANDS_AND_ORS = QueryModel.QueryModelPredef.myString().eq("thing").and().myInteger()
                    .gt(0).or().myInteger().lt(5).or().myLong().in(0L, 1L, 2L).and().myDouble()
                    .lte(2.9).and().myBoolean().isFalse().or().myDateTime().doesNotExist();

            Condition<QueryModel> CHAINED_ORS_AND_ANDS = QueryModel.QueryModelPredef.myString().eq("thing").or().myInteger()
                    .gt(0).and().myInteger().lt(5).and().myLong().in(0L, 1L, 2L).or().myDouble()
                    .lte(2.9).or().myBoolean().isFalse().and().myDateTime().doesNotExist();
    }

    protected interface Composed {
        Condition<QueryModel> SUB_QUERY = QueryModel.QueryModelPredef.mySubList().any(Logical.INLINE_ANDING).and().myBoolean().isTrue();
    }

    protected interface VariedInputs {
        Condition<QueryModel> NULL_EQUALITY = QueryModel.QueryModelPredef.myString().eq(null);
        Condition<QueryModel> NULL_INEQUALITY = QueryModel.QueryModelPredef.myString().ne(null);
    }

    protected String Enum_EQ;
    protected String Enum_NE;
    protected String Enum_EX;
    protected String Enum_DNE;
    protected String Enum_IN;
    protected String Enum_NIN;

    @Test
    public void simple_Enum() {
        compare(Enum_EQ, Simple.Enum.EQ);
        compare(Enum_NE, Simple.Enum.NE);
        compare(Enum_EX, Simple.Enum.EX);
        compare(Enum_DNE, Simple.Enum.DNE);
        compare(Enum_IN, Simple.Enum.IN);
        compare(Enum_NIN, Simple.Enum.NIN);
    }

    protected String String_EQ;
    protected String String_NE;
    protected String String_LT;
    protected String String_GT;
    protected String String_LTE;
    protected String String_GTE;
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
        compare(String_LTE, Simple.String.LTE);
        compare(String_GTE, Simple.String.GTE);
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

    protected String DateTime_EQ;
    protected String DateTime_NE;
    protected String DateTime_LT;
    protected String DateTime_GT;
    protected String DateTime_LTE;
    protected String DateTime_GTE;
    protected String DateTime_EX;
    protected String DateTime_DNE;
    protected String DateTime_BETWEEN;

    @Test
    public void simple_DateTime() {
        compare(DateTime_EQ, Simple.Instant.EQ);
        compare(DateTime_NE, Simple.Instant.NE);
        compare(DateTime_LT, Simple.Instant.LT);
        compare(DateTime_LTE, Simple.Instant.LTE);
        compare(DateTime_GT, Simple.Instant.GT);
        compare(DateTime_GTE, Simple.Instant.GTE);
        compare(DateTime_EX, Simple.Instant.EX);
        compare(DateTime_DNE, Simple.Instant.DNE);
        compare(DateTime_BETWEEN, Simple.Instant.BETWEEN);
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

    protected String CHAINED_ANDS;

    @Test
    public void chainedAnds() {
        compare(CHAINED_ANDS, Chained.CHAINED_ANDS);
    }

    protected String CHAINED_ORS;

    @Test
    public void chainedOrs() {
        compare(CHAINED_ORS, Chained.CHAINED_ORS);
    }

    protected String CHAINED_ANDS_AND_ORS;

    @Test
    public void chainedAndsAndOrs() {
        compare(CHAINED_ANDS_AND_ORS, Chained.CHAINED_ANDS_AND_ORS);
    }

    protected String CHAINED_ORS_AND_ANDS;

    @Test
    public void chainedOrsAndAnds() {
        compare(CHAINED_ORS_AND_ANDS, Chained.CHAINED_ORS_AND_ANDS);
    }

    protected String SUB_QUERY;

    @Test
    public void subquery() {
        compare(SUB_QUERY, Composed.SUB_QUERY);
    }

    protected String NULL_EQUALITY;
    protected String NULL_INEQUALITY;

    @Test
    public void variedInputs() {
        compare(NULL_EQUALITY, VariedInputs.NULL_EQUALITY);
        compare(NULL_INEQUALITY, VariedInputs.NULL_INEQUALITY);
    }

    protected void compare(String expected, Condition<QueryModel> condition) {
        T visitor = getVisitor();
        compare(expected, condition.query(visitor));
    }


}
