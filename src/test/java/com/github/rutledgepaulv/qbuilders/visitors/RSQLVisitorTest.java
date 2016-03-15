package com.github.rutledgepaulv.qbuilders.visitors;

import com.github.rutledgepaulv.qbuilders.conditions.Condition;
import com.github.rutledgepaulv.testsupport.QBuilderTestBase;
import com.github.rutledgepaulv.testsupport.QueryModel;
import cz.jirutka.rsql.parser.RSQLParser;
import cz.jirutka.rsql.parser.ast.AndNode;
import cz.jirutka.rsql.parser.ast.ComparisonNode;
import cz.jirutka.rsql.parser.ast.Node;
import cz.jirutka.rsql.parser.ast.RSQLOperators;
import org.junit.Test;

import java.util.HashSet;
import java.util.Set;

import static com.github.rutledgepaulv.testsupport.QueryModel.QueryModelPredef.*;
import static org.junit.Assert.assertEquals;

public class RSQLVisitorTest extends QBuilderTestBase<RSQLVisitor, String, Void> {

    public RSQLVisitorTest() {

        Enum_EQ = "myEnum==\"VALUE1\"";
        Enum_NE = "myEnum!=\"VALUE1\"";
        Enum_EX = "myEnum=ex=\"true\"";
        Enum_DNE = "myEnum=ex=\"false\"";
        Enum_IN = "myEnum=in=(\"VALUE1\",\"VALUE2\",\"VALUE3\")";
        Enum_NIN = "myEnum=out=(\"VALUE1\",\"VALUE2\",\"VALUE3\")";

        String_EQ = "myString==\"abcdefg\"";
        String_NE = "myString!=\"abcdefg\"";
        String_LT = "myString=lt=\"abcdefg\"";
        String_GT = "myString=gt=\"abcdefg\"";
        String_EX = "myString=ex=\"true\"";
        String_DNE = "myString=ex=\"false\"";
        String_IN = "myString=in=(\"a\",\"b\",\"c\")";
        String_NIN = "myString=out=(\"d\",\"e\",\"f\")";
        String_LTE = "myString=le=\"abcdefg\"";
        String_GTE = "myString=ge=\"abcdefg\"";

        Boolean_TRUE = "myBoolean==\"true\"";
        Boolean_FALSE = "myBoolean==\"false\"";
        Boolean_EX = "myBoolean=ex=\"true\"";
        Boolean_DNE = "myBoolean=ex=\"false\"";

        Short_EQ = "myShort==\"100\"";
        Short_NE = "myShort!=\"100\"";
        Short_LT = "myShort=lt=\"100\"";
        Short_GT = "myShort=gt=\"100\"";
        Short_LTE = "myShort=le=\"100\"";
        Short_GTE = "myShort=ge=\"100\"";
        Short_EX = "myShort=ex=\"true\"";
        Short_DNE = "myShort=ex=\"false\"";
        Short_IN = "myShort=in=(\"98\",\"99\",\"100\")";
        Short_NIN = "myShort=out=(\"101\",\"102\",\"103\")";

        Integer_EQ = "myInteger==\"100\"";
        Integer_NE = "myInteger!=\"100\"";
        Integer_LT = "myInteger=lt=\"100\"";
        Integer_GT = "myInteger=gt=\"100\"";
        Integer_LTE = "myInteger=le=\"100\"";
        Integer_GTE = "myInteger=ge=\"100\"";
        Integer_EX = "myInteger=ex=\"true\"";
        Integer_DNE = "myInteger=ex=\"false\"";
        Integer_IN = "myInteger=in=(\"98\",\"99\",\"100\")";
        Integer_NIN = "myInteger=out=(\"101\",\"102\",\"103\")";

        Long_EQ = "myLong==\"100\"";
        Long_NE = "myLong!=\"100\"";
        Long_LT = "myLong=lt=\"100\"";
        Long_GT = "myLong=gt=\"100\"";
        Long_LTE = "myLong=le=\"100\"";
        Long_GTE = "myLong=ge=\"100\"";
        Long_EX = "myLong=ex=\"true\"";
        Long_DNE = "myLong=ex=\"false\"";
        Long_IN = "myLong=in=(\"98\",\"99\",\"100\")";
        Long_NIN = "myLong=out=(\"101\",\"102\",\"103\")";

        Float_EQ = "myFloat==\"100.0\"";
        Float_NE = "myFloat!=\"100.0\"";
        Float_LT = "myFloat=lt=\"100.0\"";
        Float_GT = "myFloat=gt=\"100.0\"";
        Float_LTE = "myFloat=le=\"100.0\"";
        Float_GTE = "myFloat=ge=\"100.0\"";
        Float_EX = "myFloat=ex=\"true\"";
        Float_DNE = "myFloat=ex=\"false\"";
        Float_IN = "myFloat=in=(\"98.0\",\"99.0\",\"100.0\")";
        Float_NIN = "myFloat=out=(\"101.0\",\"102.0\",\"103.0\")";

        Double_EQ = "myDouble==\"100.0\"";
        Double_NE = "myDouble!=\"100.0\"";
        Double_LT = "myDouble=lt=\"100.0\"";
        Double_GT = "myDouble=gt=\"100.0\"";
        Double_LTE = "myDouble=le=\"100.0\"";
        Double_GTE = "myDouble=ge=\"100.0\"";
        Double_EX = "myDouble=ex=\"true\"";
        Double_DNE = "myDouble=ex=\"false\"";
        Double_IN = "myDouble=in=(\"98.0\",\"99.0\",\"100.0\")";
        Double_NIN = "myDouble=out=(\"101.0\",\"102.0\",\"103.0\")";

        DateTime_EQ = "myDateTime==\"1970-01-01T00:00:00Z\"";
        DateTime_NE = "myDateTime!=\"1970-01-01T00:00:00Z\"";
        DateTime_LT = "myDateTime=lt=\"1971-01-01T00:00:00Z\"";
        DateTime_LTE = "myDateTime=le=\"1971-01-01T00:00:00Z\"";
        DateTime_GT = "myDateTime=gt=\"1970-01-01T00:00:00Z\"";
        DateTime_GTE = "myDateTime=ge=\"1970-01-01T00:00:00Z\"";
        DateTime_EX = "myDateTime=ex=\"true\"";
        DateTime_DNE = "myDateTime=ex=\"false\"";
        DateTime_BETWEEN = "(myDateTime=ge=\"1970-01-01T00:00:00Z\";myDateTime=le=\"1971-01-01T00:00:00Z\")";

        INLINE_ANDING = "myString==\"Thing\";myLong=ex=\"false\"";

        INLINE_ORING = "myString==\"Thing\",myLong=ex=\"false\"";

        LIST_ANDING = "(myString==\"Thing\";myLong=ex=\"false\")";

        LIST_ORING = "(myString==\"Thing\",myLong=ex=\"false\")";

        LIST_ORING_OF_INLINE_ANDING = "((myString==\"Thing\";myLong=ex=\"false\"),(myString!=\"Cats\";myLong=gt=\"30\"))";

        LIST_ANDING_OF_INLINE_ORING = "((myString==\"Thing\",myLong=ex=\"false\");(myString!=\"Cats\",myLong=gt=\"30\"))";

        LIST_ANDING_OR_LIST_ORING = "((myString==\"Thing\",myLong=ex=\"false\");(myString!=\"Cats\",myLong=gt=\"30\"))," +
                "((myString==\"Thing\";myLong=ex=\"false\"),(myString!=\"Cats\";myLong=gt=\"30\"))";

        LIST_ORING_AND_LIST_ANDING = "((myString==\"Thing\";myLong=ex=\"false\"),(myString!=\"Cats\";myLong=gt=\"30\"));" +
                "((myString==\"Thing\",myLong=ex=\"false\");(myString!=\"Cats\",myLong=gt=\"30\"))";


        CHAINED_ORS = "myString==\"thing\",myInteger=gt=\"0\",myInteger=lt=\"5\",myLong=in=(\"0\",\"1\",\"2\")," +
                "myDouble=le=\"2.9\",myBoolean==\"false\",myDateTime=ex=\"false\"";

        CHAINED_ANDS = "myString==\"thing\";myInteger=gt=\"0\";myInteger=lt=\"5\";" +
                "myLong=in=(\"0\",\"1\",\"2\");myDouble=le=\"2.9\";myBoolean==\"false\";myDateTime=ex=\"false\"";

        CHAINED_ANDS_AND_ORS = "(((myString==\"thing\";myInteger=gt=\"0\"),myInteger=lt=\"5\",myLong=in=(\"0\",\"1\",\"2\"));" +
                "myDouble=le=\"2.9\";myBoolean==\"false\"),myDateTime=ex=\"false\"";

        CHAINED_ORS_AND_ANDS = "(((myString==\"thing\",myInteger=gt=\"0\");myInteger=lt=\"5\";" +
                "myLong=in=(\"0\",\"1\",\"2\")),myDouble=le=\"2.9\",myBoolean==\"false\");myDateTime=ex=\"false\"";

        SUB_QUERY = "mySubList=q='myString==\"Thing\";myLong=ex=\"false\"';myBoolean==\"true\"";

        NULL_EQUALITY = "myString=ex=\"false\"";
        NULL_INEQUALITY = "myString=ex=\"true\"";

    }

    @Test
    public void testDoubleAndSingleQuoteWithEscapeCharacterEscaping() {

        Condition<QueryModel> q = myString().eq("people's \\of \\the world").and().myBoolean()
                .isTrue().and().myListOfStrings().in("\"cats", "'demo'\"", "\"test");

        compare("myString==\"people's \\\\of \\\\the world\";myBoolean==\"true\";" +
                "myListOfStrings=in=('\"cats',\"'demo'\\\"\",'\"test')", q);
    }


    @Test
    public void testWithBackslashesInValue() {
        String value = "'\\something \\with \\\"some\" \\backslashes'";
        Condition<QueryModel> query = myString().eq(value);
        compare("myString==\"'\\\\something \\\\with \\\\\\\"some\\\" \\\\backslashes'\"", query);

        ComparisonNode node = (ComparisonNode) parse(query.query(new RSQLVisitor()));
        assertEquals("myString", node.getSelector());
        assertEquals("==", node.getOperator().getSymbol());
        assertEquals(value, node.getArguments().get(0));
    }


    @Test
    public void testUsingSingleQuoteButNoDoubleQuote() {
        String value = "'cats'";
        Condition<QueryModel> query = myString().eq(value);
        compare("myString==\"'cats'\"", query);

        ComparisonNode node = (ComparisonNode) parse(query.query(new RSQLVisitor()));
        assertEquals("myString", node.getSelector());
        assertEquals("==", node.getOperator().getSymbol());
        assertEquals(value, node.getArguments().get(0));
    }

    @Test
    public void testUsingDoubleQuoteButNoSingleQuote() {

        String value = "\"cats\"";
        Condition<QueryModel> query = myString().eq(value);
        compare("myString=='\"cats\"'", query);

        ComparisonNode node = (ComparisonNode) parse(query.query(new RSQLVisitor()));
        assertEquals("myString", node.getSelector());
        assertEquals("==", node.getOperator().getSymbol());
        assertEquals(value, node.getArguments().get(0));

    }

    @Test
    public void testEscapingOfBothKindsOfQuotesInSingleQueryValue() {
        String stringWithApostropheAndQuotation = "Paul's friend said \"Let's run off and escape together!\"";

        Condition<QueryModel> query = myString().eq(stringWithApostropheAndQuotation);

        compare("myString==\"Paul's friend said \\\"Let's run off and escape together!\\\"\"",
                query);

        ComparisonNode node = (ComparisonNode) parse(query.query(new RSQLVisitor()));
        assertEquals("myString", node.getSelector());
        assertEquals("==", node.getOperator().getSymbol());
        assertEquals(stringWithApostropheAndQuotation, node.getArguments().get(0));

    }

    @Test
    public void testEscapingOfBothKindsOfQuotesInMultiQueryValue() {
        String val1 = "Paul said \"Okay, but only if we can take a giant peach!\"";
        String val2 = "'Fine.' grumbled the friend. We can take the \"giant\" peach.";

        Condition<QueryModel> query = myString().in(val1, val2);

        compare("myString=in=('Paul said \"Okay, but only if we can take a giant peach!\"'," +
                        "\"'Fine.' grumbled the friend. We can take the \\\"giant\\\" peach.\")",
                query);

        ComparisonNode node = (ComparisonNode) parse(query.query(new RSQLVisitor()));
        assertEquals("myString", node.getSelector());
        assertEquals("=in=", node.getOperator().getSymbol());
        assertEquals(val1, node.getArguments().get(0));
        assertEquals(val2, node.getArguments().get(1));
    }

    @Test
    public void testEscapingOfBothKindsOfQuotesInSubqueryQueryValue() {
        String val1 = "\"Off to the peach we go!\" Hi ho' Hi ho' Hi ho'";
        String val2 = "'Rawwwwrrrrrrrrr' I'm a dinosawr'";
        Condition<QueryModel> subquery = mySubList().any(and(myString().eq(val1), myString().eq(val2)));

        compare("mySubList=q=\"(myString==\\\"\\\\\\\"Off to the peach we go!\\\\\\\" " +
                        "Hi ho' Hi ho' Hi ho'\\\";myString==\\\"'Rawwwwrrrrrrrrr' I'm a dinosawr'\\\")\"",
                subquery);

        cz.jirutka.rsql.parser.ast.ComparisonNode node = ( cz.jirutka.rsql.parser.ast.ComparisonNode ) parse(subquery.query(new RSQLVisitor()));
        assertEquals("=q=", node.getOperator().getSymbol());
        assertEquals("mySubList", node.getSelector());
        AndNode and = (AndNode) parse(node.getArguments().get(0));
        cz.jirutka.rsql.parser.ast.ComparisonNode  left = (ComparisonNode) and.getChildren().get(0);
        cz.jirutka.rsql.parser.ast.ComparisonNode  right = (ComparisonNode) and.getChildren().get(1);

        assertEquals("myString", left.getSelector());
        assertEquals(val1, left.getArguments().get(0));
        assertEquals("myString", right.getSelector());
        assertEquals(val2, right.getArguments().get(0));
    }

    protected Node parse(String rsql) {
        Set<cz.jirutka.rsql.parser.ast.ComparisonOperator> ops = new HashSet<>();
        ops.addAll(RSQLOperators.defaultOperators());
        ops.add(new cz.jirutka.rsql.parser.ast.ComparisonOperator("=q=", false));
        ops.add(new cz.jirutka.rsql.parser.ast.ComparisonOperator("=ex=", false));
        return new RSQLParser(ops).parse(rsql);
    }

    @Override
    protected RSQLVisitor getVisitor() {
        return new RSQLVisitor();
    }

    @Override
    protected void compare(String expected, String converted) {
        // make sure it's parseable
        parse(converted);
        assertEquals(expected, converted);
    }

}
