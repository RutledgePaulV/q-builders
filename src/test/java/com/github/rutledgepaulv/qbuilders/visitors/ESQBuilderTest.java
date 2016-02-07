package com.github.rutledgepaulv.qbuilders.visitors;

import com.github.rutledgepaulv.testsupport.basic.QBuilderTestBase;
import com.github.rutledgepaulv.qbuilders.visitors.basic.BasicEsVisitor;

import static org.junit.Assert.assertEquals;

public class ESQBuilderTest extends QBuilderTestBase<BasicEsVisitor, org.elasticsearch.index.query.QueryBuilder> {

    public ESQBuilderTest() {

        String_EQ = "{\n" +
                "  \"term\" : {\n" +
                "    \"myString\" : \"abcdefg\"\n" +
                "  }\n" +
                "}";

        String_NE = "{\n" +
                "  \"bool\" : {\n" +
                "    \"must_not\" : {\n" +
                "      \"term\" : {\n" +
                "        \"myString\" : \"abcdefg\"\n" +
                "      }\n" +
                "    }\n" +
                "  }\n" +
                "}";

        String_LT = "{\n" +
                "  \"range\" : {\n" +
                "    \"myString\" : {\n" +
                "      \"from\" : null,\n" +
                "      \"to\" : \"abcdefg\",\n" +
                "      \"include_lower\" : true,\n" +
                "      \"include_upper\" : false\n" +
                "    }\n" +
                "  }\n" +
                "}";

        String_LTE = "{\n" +
                "  \"range\" : {\n" +
                "    \"myString\" : {\n" +
                "      \"from\" : null,\n" +
                "      \"to\" : \"abcdefg\",\n" +
                "      \"include_lower\" : true,\n" +
                "      \"include_upper\" : true\n" +
                "    }\n" +
                "  }\n" +
                "}";

        String_GT = "{\n" +
                "  \"range\" : {\n" +
                "    \"myString\" : {\n" +
                "      \"from\" : \"abcdefg\",\n" +
                "      \"to\" : null,\n" +
                "      \"include_lower\" : false,\n" +
                "      \"include_upper\" : true\n" +
                "    }\n" +
                "  }\n" +
                "}";

        String_GTE = "{\n" +
                "  \"range\" : {\n" +
                "    \"myString\" : {\n" +
                "      \"from\" : \"abcdefg\",\n" +
                "      \"to\" : null,\n" +
                "      \"include_lower\" : true,\n" +
                "      \"include_upper\" : true\n" +
                "    }\n" +
                "  }\n" +
                "}";

        String_EX = "{\n" +
                "  \"exists\" : {\n" +
                "    \"field\" : \"myString\"\n" +
                "  }\n" +
                "}";

        String_DNE = "{\n" +
                "  \"bool\" : {\n" +
                "    \"must_not\" : {\n" +
                "      \"exists\" : {\n" +
                "        \"field\" : \"myString\"\n" +
                "      }\n" +
                "    }\n" +
                "  }\n" +
                "}";

        String_IN = "{\n" +
                "  \"terms\" : {\n" +
                "    \"myString\" : [ \"a\", \"b\", \"c\" ]\n" +
                "  }\n" +
                "}";

        String_NIN = "{\n" +
                "  \"bool\" : {\n" +
                "    \"must_not\" : {\n" +
                "      \"terms\" : {\n" +
                "        \"myString\" : [ \"d\", \"e\", \"f\" ]\n" +
                "      }\n" +
                "    }\n" +
                "  }\n" +
                "}";

        Boolean_TRUE = "{\n" +
                "  \"term\" : {\n" +
                "    \"myBoolean\" : true\n" +
                "  }\n" +
                "}";

        Boolean_FALSE = "{\n" +
                "  \"term\" : {\n" +
                "    \"myBoolean\" : false\n" +
                "  }\n" +
                "}";

        Boolean_EX = "{\n" +
                "  \"exists\" : {\n" +
                "    \"field\" : \"myBoolean\"\n" +
                "  }\n" +
                "}";

        Boolean_DNE = "{\n" +
                "  \"bool\" : {\n" +
                "    \"must_not\" : {\n" +
                "      \"exists\" : {\n" +
                "        \"field\" : \"myBoolean\"\n" +
                "      }\n" +
                "    }\n" +
                "  }\n" +
                "}";

        Short_EQ = "{\n" +
                "  \"term\" : {\n" +
                "    \"myShort\" : 100\n" +
                "  }\n" +
                "}";

        Short_NE = "{\n" +
                "  \"bool\" : {\n" +
                "    \"must_not\" : {\n" +
                "      \"term\" : {\n" +
                "        \"myShort\" : 100\n" +
                "      }\n" +
                "    }\n" +
                "  }\n" +
                "}";

        Short_LT = "{\n" +
                "  \"range\" : {\n" +
                "    \"myShort\" : {\n" +
                "      \"from\" : null,\n" +
                "      \"to\" : 100,\n" +
                "      \"include_lower\" : true,\n" +
                "      \"include_upper\" : false\n" +
                "    }\n" +
                "  }\n" +
                "}";

        Short_GT = "{\n" +
                "  \"range\" : {\n" +
                "    \"myShort\" : {\n" +
                "      \"from\" : 100,\n" +
                "      \"to\" : null,\n" +
                "      \"include_lower\" : false,\n" +
                "      \"include_upper\" : true\n" +
                "    }\n" +
                "  }\n" +
                "}";

        Short_LTE = "{\n" +
                "  \"range\" : {\n" +
                "    \"myShort\" : {\n" +
                "      \"from\" : null,\n" +
                "      \"to\" : 100,\n" +
                "      \"include_lower\" : true,\n" +
                "      \"include_upper\" : true\n" +
                "    }\n" +
                "  }\n" +
                "}";

        Short_GTE = "{\n" +
                "  \"range\" : {\n" +
                "    \"myShort\" : {\n" +
                "      \"from\" : 100,\n" +
                "      \"to\" : null,\n" +
                "      \"include_lower\" : true,\n" +
                "      \"include_upper\" : true\n" +
                "    }\n" +
                "  }\n" +
                "}";

        Short_EX = "{\n" +
                "  \"exists\" : {\n" +
                "    \"field\" : \"myShort\"\n" +
                "  }\n" +
                "}";

        Short_DNE = "{\n" +
                "  \"bool\" : {\n" +
                "    \"must_not\" : {\n" +
                "      \"exists\" : {\n" +
                "        \"field\" : \"myShort\"\n" +
                "      }\n" +
                "    }\n" +
                "  }\n" +
                "}";

        Short_IN = "{\n" +
                "  \"terms\" : {\n" +
                "    \"myShort\" : [ 98, 99, 100 ]\n" +
                "  }\n" +
                "}";

        Short_NIN = "{\n" +
                "  \"bool\" : {\n" +
                "    \"must_not\" : {\n" +
                "      \"terms\" : {\n" +
                "        \"myShort\" : [ 101, 102, 103 ]\n" +
                "      }\n" +
                "    }\n" +
                "  }\n" +
                "}";

        Integer_EQ = "{\n" +
                "  \"term\" : {\n" +
                "    \"myInteger\" : 100\n" +
                "  }\n" +
                "}";

        Integer_NE = "{\n" +
                "  \"bool\" : {\n" +
                "    \"must_not\" : {\n" +
                "      \"term\" : {\n" +
                "        \"myInteger\" : 100\n" +
                "      }\n" +
                "    }\n" +
                "  }\n" +
                "}";

        Integer_LT = "{\n" +
                "  \"range\" : {\n" +
                "    \"myInteger\" : {\n" +
                "      \"from\" : null,\n" +
                "      \"to\" : 100,\n" +
                "      \"include_lower\" : true,\n" +
                "      \"include_upper\" : false\n" +
                "    }\n" +
                "  }\n" +
                "}";

        Integer_GT = "{\n" +
                "  \"range\" : {\n" +
                "    \"myInteger\" : {\n" +
                "      \"from\" : 100,\n" +
                "      \"to\" : null,\n" +
                "      \"include_lower\" : false,\n" +
                "      \"include_upper\" : true\n" +
                "    }\n" +
                "  }\n" +
                "}";

        Integer_LTE = "{\n" +
                "  \"range\" : {\n" +
                "    \"myInteger\" : {\n" +
                "      \"from\" : null,\n" +
                "      \"to\" : 100,\n" +
                "      \"include_lower\" : true,\n" +
                "      \"include_upper\" : true\n" +
                "    }\n" +
                "  }\n" +
                "}";

        Integer_GTE = "{\n" +
                "  \"range\" : {\n" +
                "    \"myInteger\" : {\n" +
                "      \"from\" : 100,\n" +
                "      \"to\" : null,\n" +
                "      \"include_lower\" : true,\n" +
                "      \"include_upper\" : true\n" +
                "    }\n" +
                "  }\n" +
                "}";

        Integer_EX = "{\n" +
                "  \"exists\" : {\n" +
                "    \"field\" : \"myInteger\"\n" +
                "  }\n" +
                "}";

        Integer_DNE = "{\n" +
                "  \"bool\" : {\n" +
                "    \"must_not\" : {\n" +
                "      \"exists\" : {\n" +
                "        \"field\" : \"myInteger\"\n" +
                "      }\n" +
                "    }\n" +
                "  }\n" +
                "}";

        Integer_IN = "{\n" +
                "  \"terms\" : {\n" +
                "    \"myInteger\" : [ 98, 99, 100 ]\n" +
                "  }\n" +
                "}";

        Integer_NIN = "{\n" +
                "  \"bool\" : {\n" +
                "    \"must_not\" : {\n" +
                "      \"terms\" : {\n" +
                "        \"myInteger\" : [ 101, 102, 103 ]\n" +
                "      }\n" +
                "    }\n" +
                "  }\n" +
                "}";

        Long_EQ = "{\n" +
                "  \"term\" : {\n" +
                "    \"myLong\" : 100\n" +
                "  }\n" +
                "}";

        Long_NE = "{\n" +
                "  \"bool\" : {\n" +
                "    \"must_not\" : {\n" +
                "      \"term\" : {\n" +
                "        \"myLong\" : 100\n" +
                "      }\n" +
                "    }\n" +
                "  }\n" +
                "}";

        Long_LT = "{\n" +
                "  \"range\" : {\n" +
                "    \"myLong\" : {\n" +
                "      \"from\" : null,\n" +
                "      \"to\" : 100,\n" +
                "      \"include_lower\" : true,\n" +
                "      \"include_upper\" : false\n" +
                "    }\n" +
                "  }\n" +
                "}";

        Long_GT = "{\n" +
                "  \"range\" : {\n" +
                "    \"myLong\" : {\n" +
                "      \"from\" : 100,\n" +
                "      \"to\" : null,\n" +
                "      \"include_lower\" : false,\n" +
                "      \"include_upper\" : true\n" +
                "    }\n" +
                "  }\n" +
                "}";

        Long_LTE = "{\n" +
                "  \"range\" : {\n" +
                "    \"myLong\" : {\n" +
                "      \"from\" : null,\n" +
                "      \"to\" : 100,\n" +
                "      \"include_lower\" : true,\n" +
                "      \"include_upper\" : true\n" +
                "    }\n" +
                "  }\n" +
                "}";

        Long_GTE = "{\n" +
                "  \"range\" : {\n" +
                "    \"myLong\" : {\n" +
                "      \"from\" : 100,\n" +
                "      \"to\" : null,\n" +
                "      \"include_lower\" : true,\n" +
                "      \"include_upper\" : true\n" +
                "    }\n" +
                "  }\n" +
                "}";

        Long_EX = "{\n" +
                "  \"exists\" : {\n" +
                "    \"field\" : \"myLong\"\n" +
                "  }\n" +
                "}";

        Long_DNE = "{\n" +
                "  \"bool\" : {\n" +
                "    \"must_not\" : {\n" +
                "      \"exists\" : {\n" +
                "        \"field\" : \"myLong\"\n" +
                "      }\n" +
                "    }\n" +
                "  }\n" +
                "}";

        Long_IN = "{\n" +
                "  \"terms\" : {\n" +
                "    \"myLong\" : [ 98, 99, 100 ]\n" +
                "  }\n" +
                "}";

        Long_NIN = "{\n" +
                "  \"bool\" : {\n" +
                "    \"must_not\" : {\n" +
                "      \"terms\" : {\n" +
                "        \"myLong\" : [ 101, 102, 103 ]\n" +
                "      }\n" +
                "    }\n" +
                "  }\n" +
                "}";

        Float_EQ = "{\n" +
                "  \"term\" : {\n" +
                "    \"myFloat\" : 100.0\n" +
                "  }\n" +
                "}";

        Float_NE = "{\n" +
                "  \"bool\" : {\n" +
                "    \"must_not\" : {\n" +
                "      \"term\" : {\n" +
                "        \"myFloat\" : 100.0\n" +
                "      }\n" +
                "    }\n" +
                "  }\n" +
                "}";

        Float_LT = "{\n" +
                "  \"range\" : {\n" +
                "    \"myFloat\" : {\n" +
                "      \"from\" : null,\n" +
                "      \"to\" : 100.0,\n" +
                "      \"include_lower\" : true,\n" +
                "      \"include_upper\" : false\n" +
                "    }\n" +
                "  }\n" +
                "}";

        Float_GT = "{\n" +
                "  \"range\" : {\n" +
                "    \"myFloat\" : {\n" +
                "      \"from\" : 100.0,\n" +
                "      \"to\" : null,\n" +
                "      \"include_lower\" : false,\n" +
                "      \"include_upper\" : true\n" +
                "    }\n" +
                "  }\n" +
                "}";

        Float_LTE = "{\n" +
                "  \"range\" : {\n" +
                "    \"myFloat\" : {\n" +
                "      \"from\" : null,\n" +
                "      \"to\" : 100.0,\n" +
                "      \"include_lower\" : true,\n" +
                "      \"include_upper\" : true\n" +
                "    }\n" +
                "  }\n" +
                "}";

        Float_GTE = "{\n" +
                "  \"range\" : {\n" +
                "    \"myFloat\" : {\n" +
                "      \"from\" : 100.0,\n" +
                "      \"to\" : null,\n" +
                "      \"include_lower\" : true,\n" +
                "      \"include_upper\" : true\n" +
                "    }\n" +
                "  }\n" +
                "}";

        Float_EX = "{\n" +
                "  \"exists\" : {\n" +
                "    \"field\" : \"myFloat\"\n" +
                "  }\n" +
                "}";

        Float_DNE = "{\n" +
                "  \"bool\" : {\n" +
                "    \"must_not\" : {\n" +
                "      \"exists\" : {\n" +
                "        \"field\" : \"myFloat\"\n" +
                "      }\n" +
                "    }\n" +
                "  }\n" +
                "}";

        Float_IN = "{\n" +
                "  \"terms\" : {\n" +
                "    \"myFloat\" : [ 98.0, 99.0, 100.0 ]\n" +
                "  }\n" +
                "}";

        Float_NIN = "{\n" +
                "  \"bool\" : {\n" +
                "    \"must_not\" : {\n" +
                "      \"terms\" : {\n" +
                "        \"myFloat\" : [ 101.0, 102.0, 103.0 ]\n" +
                "      }\n" +
                "    }\n" +
                "  }\n" +
                "}";

        Double_EQ = "{\n" +
                "  \"term\" : {\n" +
                "    \"myDouble\" : 100.0\n" +
                "  }\n" +
                "}";

        Double_NE = "{\n" +
                "  \"bool\" : {\n" +
                "    \"must_not\" : {\n" +
                "      \"term\" : {\n" +
                "        \"myDouble\" : 100.0\n" +
                "      }\n" +
                "    }\n" +
                "  }\n" +
                "}";

        Double_LT = "{\n" +
                "  \"range\" : {\n" +
                "    \"myDouble\" : {\n" +
                "      \"from\" : null,\n" +
                "      \"to\" : 100.0,\n" +
                "      \"include_lower\" : true,\n" +
                "      \"include_upper\" : false\n" +
                "    }\n" +
                "  }\n" +
                "}";

        Double_GT = "{\n" +
                "  \"range\" : {\n" +
                "    \"myDouble\" : {\n" +
                "      \"from\" : 100.0,\n" +
                "      \"to\" : null,\n" +
                "      \"include_lower\" : false,\n" +
                "      \"include_upper\" : true\n" +
                "    }\n" +
                "  }\n" +
                "}";

        Double_LTE = "{\n" +
                "  \"range\" : {\n" +
                "    \"myDouble\" : {\n" +
                "      \"from\" : null,\n" +
                "      \"to\" : 100.0,\n" +
                "      \"include_lower\" : true,\n" +
                "      \"include_upper\" : true\n" +
                "    }\n" +
                "  }\n" +
                "}";

        Double_GTE = "{\n" +
                "  \"range\" : {\n" +
                "    \"myDouble\" : {\n" +
                "      \"from\" : 100.0,\n" +
                "      \"to\" : null,\n" +
                "      \"include_lower\" : true,\n" +
                "      \"include_upper\" : true\n" +
                "    }\n" +
                "  }\n" +
                "}";

        Double_EX = "{\n" +
                "  \"exists\" : {\n" +
                "    \"field\" : \"myDouble\"\n" +
                "  }\n" +
                "}";

        Double_DNE = "{\n" +
                "  \"bool\" : {\n" +
                "    \"must_not\" : {\n" +
                "      \"exists\" : {\n" +
                "        \"field\" : \"myDouble\"\n" +
                "      }\n" +
                "    }\n" +
                "  }\n" +
                "}";

        Double_IN = "{\n" +
                "  \"terms\" : {\n" +
                "    \"myDouble\" : [ 98.0, 99.0, 100.0 ]\n" +
                "  }\n" +
                "}";

        Double_NIN = "{\n" +
                "  \"bool\" : {\n" +
                "    \"must_not\" : {\n" +
                "      \"terms\" : {\n" +
                "        \"myDouble\" : [ 101.0, 102.0, 103.0 ]\n" +
                "      }\n" +
                "    }\n" +
                "  }\n" +
                "}";

        DateTime_EQ = "{\n" +
                "  \"term\" : {\n" +
                "    \"myDateTime\" : \"1970-01-01T00:00:00Z\"\n" +
                "  }\n" +
                "}";


        DateTime_NE = "{\n" +
                "  \"bool\" : {\n" +
                "    \"must_not\" : {\n" +
                "      \"term\" : {\n" +
                "        \"myDateTime\" : \"1970-01-01T00:00:00Z\"\n" +
                "      }\n" +
                "    }\n" +
                "  }\n" +
                "}";

        DateTime_LT = "{\n" +
                "  \"range\" : {\n" +
                "    \"myDateTime\" : {\n" +
                "      \"from\" : null,\n" +
                "      \"to\" : \"1971-01-01T00:00:00Z\",\n" +
                "      \"include_lower\" : true,\n" +
                "      \"include_upper\" : false\n" +
                "    }\n" +
                "  }\n" +
                "}";

        DateTime_LTE = "{\n" +
                "  \"range\" : {\n" +
                "    \"myDateTime\" : {\n" +
                "      \"from\" : null,\n" +
                "      \"to\" : \"1971-01-01T00:00:00Z\",\n" +
                "      \"include_lower\" : true,\n" +
                "      \"include_upper\" : true\n" +
                "    }\n" +
                "  }\n" +
                "}";

        DateTime_GT = "{\n" +
                "  \"range\" : {\n" +
                "    \"myDateTime\" : {\n" +
                "      \"from\" : \"1970-01-01T00:00:00Z\",\n" +
                "      \"to\" : null,\n" +
                "      \"include_lower\" : false,\n" +
                "      \"include_upper\" : true\n" +
                "    }\n" +
                "  }\n" +
                "}";

        DateTime_GTE = "{\n" +
                "  \"range\" : {\n" +
                "    \"myDateTime\" : {\n" +
                "      \"from\" : \"1970-01-01T00:00:00Z\",\n" +
                "      \"to\" : null,\n" +
                "      \"include_lower\" : true,\n" +
                "      \"include_upper\" : true\n" +
                "    }\n" +
                "  }\n" +
                "}";

        DateTime_EX = "{\n" +
                "  \"exists\" : {\n" +
                "    \"field\" : \"myDateTime\"\n" +
                "  }\n" +
                "}";

        DateTime_DNE = "{\n" +
                "  \"bool\" : {\n" +
                "    \"must_not\" : {\n" +
                "      \"exists\" : {\n" +
                "        \"field\" : \"myDateTime\"\n" +
                "      }\n" +
                "    }\n" +
                "  }\n" +
                "}";

        DateTime_BETWEEN = "{\n" +
                "  \"bool\" : {\n" +
                "    \"must\" : [ {\n" +
                "      \"range\" : {\n" +
                "        \"myDateTime\" : {\n" +
                "          \"from\" : \"1970-01-01T00:00:00Z\",\n" +
                "          \"to\" : null,\n" +
                "          \"include_lower\" : true,\n" +
                "          \"include_upper\" : true\n" +
                "        }\n" +
                "      }\n" +
                "    }, {\n" +
                "      \"range\" : {\n" +
                "        \"myDateTime\" : {\n" +
                "          \"from\" : null,\n" +
                "          \"to\" : \"1971-01-01T00:00:00Z\",\n" +
                "          \"include_lower\" : true,\n" +
                "          \"include_upper\" : true\n" +
                "        }\n" +
                "      }\n" +
                "    } ]\n" +
                "  }\n" +
                "}";

        INLINE_ANDING = "{\n" +
                "  \"bool\" : {\n" +
                "    \"must\" : [ {\n" +
                "      \"term\" : {\n" +
                "        \"myString\" : \"Thing\"\n" +
                "      }\n" +
                "    }, {\n" +
                "      \"bool\" : {\n" +
                "        \"must_not\" : {\n" +
                "          \"exists\" : {\n" +
                "            \"field\" : \"myLong\"\n" +
                "          }\n" +
                "        }\n" +
                "      }\n" +
                "    } ]\n" +
                "  }\n" +
                "}";

        INLINE_ORING = "{\n" +
                "  \"bool\" : {\n" +
                "    \"should\" : [ {\n" +
                "      \"term\" : {\n" +
                "        \"myString\" : \"Thing\"\n" +
                "      }\n" +
                "    }, {\n" +
                "      \"bool\" : {\n" +
                "        \"must_not\" : {\n" +
                "          \"exists\" : {\n" +
                "            \"field\" : \"myLong\"\n" +
                "          }\n" +
                "        }\n" +
                "      }\n" +
                "    } ]\n" +
                "  }\n" +
                "}";

        LIST_ANDING = "{\n" +
                "  \"bool\" : {\n" +
                "    \"must\" : [ {\n" +
                "      \"term\" : {\n" +
                "        \"myString\" : \"Thing\"\n" +
                "      }\n" +
                "    }, {\n" +
                "      \"bool\" : {\n" +
                "        \"must_not\" : {\n" +
                "          \"exists\" : {\n" +
                "            \"field\" : \"myLong\"\n" +
                "          }\n" +
                "        }\n" +
                "      }\n" +
                "    } ]\n" +
                "  }\n" +
                "}";

        LIST_ORING = "{\n" +
                "  \"bool\" : {\n" +
                "    \"should\" : [ {\n" +
                "      \"term\" : {\n" +
                "        \"myString\" : \"Thing\"\n" +
                "      }\n" +
                "    }, {\n" +
                "      \"bool\" : {\n" +
                "        \"must_not\" : {\n" +
                "          \"exists\" : {\n" +
                "            \"field\" : \"myLong\"\n" +
                "          }\n" +
                "        }\n" +
                "      }\n" +
                "    } ]\n" +
                "  }\n" +
                "}";


        LIST_ORING_OF_INLINE_ANDING = "{\n" +
                "  \"bool\" : {\n" +
                "    \"should\" : [ {\n" +
                "      \"bool\" : {\n" +
                "        \"must\" : [ {\n" +
                "          \"term\" : {\n" +
                "            \"myString\" : \"Thing\"\n" +
                "          }\n" +
                "        }, {\n" +
                "          \"bool\" : {\n" +
                "            \"must_not\" : {\n" +
                "              \"exists\" : {\n" +
                "                \"field\" : \"myLong\"\n" +
                "              }\n" +
                "            }\n" +
                "          }\n" +
                "        } ]\n" +
                "      }\n" +
                "    }, {\n" +
                "      \"bool\" : {\n" +
                "        \"must\" : [ {\n" +
                "          \"bool\" : {\n" +
                "            \"must_not\" : {\n" +
                "              \"term\" : {\n" +
                "                \"myString\" : \"Cats\"\n" +
                "              }\n" +
                "            }\n" +
                "          }\n" +
                "        }, {\n" +
                "          \"range\" : {\n" +
                "            \"myLong\" : {\n" +
                "              \"from\" : 30,\n" +
                "              \"to\" : null,\n" +
                "              \"include_lower\" : false,\n" +
                "              \"include_upper\" : true\n" +
                "            }\n" +
                "          }\n" +
                "        } ]\n" +
                "      }\n" +
                "    } ]\n" +
                "  }\n" +
                "}";

        LIST_ANDING_OF_INLINE_ORING = "{\n" +
                "  \"bool\" : {\n" +
                "    \"must\" : [ {\n" +
                "      \"bool\" : {\n" +
                "        \"should\" : [ {\n" +
                "          \"term\" : {\n" +
                "            \"myString\" : \"Thing\"\n" +
                "          }\n" +
                "        }, {\n" +
                "          \"bool\" : {\n" +
                "            \"must_not\" : {\n" +
                "              \"exists\" : {\n" +
                "                \"field\" : \"myLong\"\n" +
                "              }\n" +
                "            }\n" +
                "          }\n" +
                "        } ]\n" +
                "      }\n" +
                "    }, {\n" +
                "      \"bool\" : {\n" +
                "        \"should\" : [ {\n" +
                "          \"bool\" : {\n" +
                "            \"must_not\" : {\n" +
                "              \"term\" : {\n" +
                "                \"myString\" : \"Cats\"\n" +
                "              }\n" +
                "            }\n" +
                "          }\n" +
                "        }, {\n" +
                "          \"range\" : {\n" +
                "            \"myLong\" : {\n" +
                "              \"from\" : 30,\n" +
                "              \"to\" : null,\n" +
                "              \"include_lower\" : false,\n" +
                "              \"include_upper\" : true\n" +
                "            }\n" +
                "          }\n" +
                "        } ]\n" +
                "      }\n" +
                "    } ]\n" +
                "  }\n" +
                "}";

        LIST_ANDING_OR_LIST_ORING = "{\n" +
                "  \"bool\" : {\n" +
                "    \"should\" : [ {\n" +
                "      \"bool\" : {\n" +
                "        \"must\" : [ {\n" +
                "          \"bool\" : {\n" +
                "            \"should\" : [ {\n" +
                "              \"term\" : {\n" +
                "                \"myString\" : \"Thing\"\n" +
                "              }\n" +
                "            }, {\n" +
                "              \"bool\" : {\n" +
                "                \"must_not\" : {\n" +
                "                  \"exists\" : {\n" +
                "                    \"field\" : \"myLong\"\n" +
                "                  }\n" +
                "                }\n" +
                "              }\n" +
                "            } ]\n" +
                "          }\n" +
                "        }, {\n" +
                "          \"bool\" : {\n" +
                "            \"should\" : [ {\n" +
                "              \"bool\" : {\n" +
                "                \"must_not\" : {\n" +
                "                  \"term\" : {\n" +
                "                    \"myString\" : \"Cats\"\n" +
                "                  }\n" +
                "                }\n" +
                "              }\n" +
                "            }, {\n" +
                "              \"range\" : {\n" +
                "                \"myLong\" : {\n" +
                "                  \"from\" : 30,\n" +
                "                  \"to\" : null,\n" +
                "                  \"include_lower\" : false,\n" +
                "                  \"include_upper\" : true\n" +
                "                }\n" +
                "              }\n" +
                "            } ]\n" +
                "          }\n" +
                "        } ]\n" +
                "      }\n" +
                "    }, {\n" +
                "      \"bool\" : {\n" +
                "        \"should\" : [ {\n" +
                "          \"bool\" : {\n" +
                "            \"must\" : [ {\n" +
                "              \"term\" : {\n" +
                "                \"myString\" : \"Thing\"\n" +
                "              }\n" +
                "            }, {\n" +
                "              \"bool\" : {\n" +
                "                \"must_not\" : {\n" +
                "                  \"exists\" : {\n" +
                "                    \"field\" : \"myLong\"\n" +
                "                  }\n" +
                "                }\n" +
                "              }\n" +
                "            } ]\n" +
                "          }\n" +
                "        }, {\n" +
                "          \"bool\" : {\n" +
                "            \"must\" : [ {\n" +
                "              \"bool\" : {\n" +
                "                \"must_not\" : {\n" +
                "                  \"term\" : {\n" +
                "                    \"myString\" : \"Cats\"\n" +
                "                  }\n" +
                "                }\n" +
                "              }\n" +
                "            }, {\n" +
                "              \"range\" : {\n" +
                "                \"myLong\" : {\n" +
                "                  \"from\" : 30,\n" +
                "                  \"to\" : null,\n" +
                "                  \"include_lower\" : false,\n" +
                "                  \"include_upper\" : true\n" +
                "                }\n" +
                "              }\n" +
                "            } ]\n" +
                "          }\n" +
                "        } ]\n" +
                "      }\n" +
                "    } ]\n" +
                "  }\n" +
                "}";

        LIST_ORING_AND_LIST_ANDING = "{\n" +
                "  \"bool\" : {\n" +
                "    \"must\" : [ {\n" +
                "      \"bool\" : {\n" +
                "        \"should\" : [ {\n" +
                "          \"bool\" : {\n" +
                "            \"must\" : [ {\n" +
                "              \"term\" : {\n" +
                "                \"myString\" : \"Thing\"\n" +
                "              }\n" +
                "            }, {\n" +
                "              \"bool\" : {\n" +
                "                \"must_not\" : {\n" +
                "                  \"exists\" : {\n" +
                "                    \"field\" : \"myLong\"\n" +
                "                  }\n" +
                "                }\n" +
                "              }\n" +
                "            } ]\n" +
                "          }\n" +
                "        }, {\n" +
                "          \"bool\" : {\n" +
                "            \"must\" : [ {\n" +
                "              \"bool\" : {\n" +
                "                \"must_not\" : {\n" +
                "                  \"term\" : {\n" +
                "                    \"myString\" : \"Cats\"\n" +
                "                  }\n" +
                "                }\n" +
                "              }\n" +
                "            }, {\n" +
                "              \"range\" : {\n" +
                "                \"myLong\" : {\n" +
                "                  \"from\" : 30,\n" +
                "                  \"to\" : null,\n" +
                "                  \"include_lower\" : false,\n" +
                "                  \"include_upper\" : true\n" +
                "                }\n" +
                "              }\n" +
                "            } ]\n" +
                "          }\n" +
                "        } ]\n" +
                "      }\n" +
                "    }, {\n" +
                "      \"bool\" : {\n" +
                "        \"must\" : [ {\n" +
                "          \"bool\" : {\n" +
                "            \"should\" : [ {\n" +
                "              \"term\" : {\n" +
                "                \"myString\" : \"Thing\"\n" +
                "              }\n" +
                "            }, {\n" +
                "              \"bool\" : {\n" +
                "                \"must_not\" : {\n" +
                "                  \"exists\" : {\n" +
                "                    \"field\" : \"myLong\"\n" +
                "                  }\n" +
                "                }\n" +
                "              }\n" +
                "            } ]\n" +
                "          }\n" +
                "        }, {\n" +
                "          \"bool\" : {\n" +
                "            \"should\" : [ {\n" +
                "              \"bool\" : {\n" +
                "                \"must_not\" : {\n" +
                "                  \"term\" : {\n" +
                "                    \"myString\" : \"Cats\"\n" +
                "                  }\n" +
                "                }\n" +
                "              }\n" +
                "            }, {\n" +
                "              \"range\" : {\n" +
                "                \"myLong\" : {\n" +
                "                  \"from\" : 30,\n" +
                "                  \"to\" : null,\n" +
                "                  \"include_lower\" : false,\n" +
                "                  \"include_upper\" : true\n" +
                "                }\n" +
                "              }\n" +
                "            } ]\n" +
                "          }\n" +
                "        } ]\n" +
                "      }\n" +
                "    } ]\n" +
                "  }\n" +
                "}";

        CHAINED_ORS = "{\n" +
                "  \"bool\" : {\n" +
                "    \"should\" : [ {\n" +
                "      \"term\" : {\n" +
                "        \"myString\" : \"thing\"\n" +
                "      }\n" +
                "    }, {\n" +
                "      \"range\" : {\n" +
                "        \"myInteger\" : {\n" +
                "          \"from\" : 0,\n" +
                "          \"to\" : null,\n" +
                "          \"include_lower\" : false,\n" +
                "          \"include_upper\" : true\n" +
                "        }\n" +
                "      }\n" +
                "    }, {\n" +
                "      \"range\" : {\n" +
                "        \"myInteger\" : {\n" +
                "          \"from\" : null,\n" +
                "          \"to\" : 5,\n" +
                "          \"include_lower\" : true,\n" +
                "          \"include_upper\" : false\n" +
                "        }\n" +
                "      }\n" +
                "    }, {\n" +
                "      \"terms\" : {\n" +
                "        \"myLong\" : [ 0, 1, 2 ]\n" +
                "      }\n" +
                "    }, {\n" +
                "      \"range\" : {\n" +
                "        \"myDouble\" : {\n" +
                "          \"from\" : null,\n" +
                "          \"to\" : 2.9,\n" +
                "          \"include_lower\" : true,\n" +
                "          \"include_upper\" : true\n" +
                "        }\n" +
                "      }\n" +
                "    }, {\n" +
                "      \"term\" : {\n" +
                "        \"myBoolean\" : false\n" +
                "      }\n" +
                "    }, {\n" +
                "      \"bool\" : {\n" +
                "        \"must_not\" : {\n" +
                "          \"exists\" : {\n" +
                "            \"field\" : \"myDateTime\"\n" +
                "          }\n" +
                "        }\n" +
                "      }\n" +
                "    } ]\n" +
                "  }\n" +
                "}";

        CHAINED_ANDS_AND_ORS = "{\n" +
                "  \"bool\" : {\n" +
                "    \"should\" : [ {\n" +
                "      \"bool\" : {\n" +
                "        \"must\" : [ {\n" +
                "          \"bool\" : {\n" +
                "            \"should\" : [ {\n" +
                "              \"bool\" : {\n" +
                "                \"must\" : [ {\n" +
                "                  \"term\" : {\n" +
                "                    \"myString\" : \"thing\"\n" +
                "                  }\n" +
                "                }, {\n" +
                "                  \"range\" : {\n" +
                "                    \"myInteger\" : {\n" +
                "                      \"from\" : 0,\n" +
                "                      \"to\" : null,\n" +
                "                      \"include_lower\" : false,\n" +
                "                      \"include_upper\" : true\n" +
                "                    }\n" +
                "                  }\n" +
                "                } ]\n" +
                "              }\n" +
                "            }, {\n" +
                "              \"range\" : {\n" +
                "                \"myInteger\" : {\n" +
                "                  \"from\" : null,\n" +
                "                  \"to\" : 5,\n" +
                "                  \"include_lower\" : true,\n" +
                "                  \"include_upper\" : false\n" +
                "                }\n" +
                "              }\n" +
                "            }, {\n" +
                "              \"terms\" : {\n" +
                "                \"myLong\" : [ 0, 1, 2 ]\n" +
                "              }\n" +
                "            } ]\n" +
                "          }\n" +
                "        }, {\n" +
                "          \"range\" : {\n" +
                "            \"myDouble\" : {\n" +
                "              \"from\" : null,\n" +
                "              \"to\" : 2.9,\n" +
                "              \"include_lower\" : true,\n" +
                "              \"include_upper\" : true\n" +
                "            }\n" +
                "          }\n" +
                "        }, {\n" +
                "          \"term\" : {\n" +
                "            \"myBoolean\" : false\n" +
                "          }\n" +
                "        } ]\n" +
                "      }\n" +
                "    }, {\n" +
                "      \"bool\" : {\n" +
                "        \"must_not\" : {\n" +
                "          \"exists\" : {\n" +
                "            \"field\" : \"myDateTime\"\n" +
                "          }\n" +
                "        }\n" +
                "      }\n" +
                "    } ]\n" +
                "  }\n" +
                "}";

        CHAINED_ANDS = "{\n" +
                "  \"bool\" : {\n" +
                "    \"must\" : [ {\n" +
                "      \"term\" : {\n" +
                "        \"myString\" : \"thing\"\n" +
                "      }\n" +
                "    }, {\n" +
                "      \"range\" : {\n" +
                "        \"myInteger\" : {\n" +
                "          \"from\" : 0,\n" +
                "          \"to\" : null,\n" +
                "          \"include_lower\" : false,\n" +
                "          \"include_upper\" : true\n" +
                "        }\n" +
                "      }\n" +
                "    }, {\n" +
                "      \"range\" : {\n" +
                "        \"myInteger\" : {\n" +
                "          \"from\" : null,\n" +
                "          \"to\" : 5,\n" +
                "          \"include_lower\" : true,\n" +
                "          \"include_upper\" : false\n" +
                "        }\n" +
                "      }\n" +
                "    }, {\n" +
                "      \"terms\" : {\n" +
                "        \"myLong\" : [ 0, 1, 2 ]\n" +
                "      }\n" +
                "    }, {\n" +
                "      \"range\" : {\n" +
                "        \"myDouble\" : {\n" +
                "          \"from\" : null,\n" +
                "          \"to\" : 2.9,\n" +
                "          \"include_lower\" : true,\n" +
                "          \"include_upper\" : true\n" +
                "        }\n" +
                "      }\n" +
                "    }, {\n" +
                "      \"term\" : {\n" +
                "        \"myBoolean\" : false\n" +
                "      }\n" +
                "    }, {\n" +
                "      \"bool\" : {\n" +
                "        \"must_not\" : {\n" +
                "          \"exists\" : {\n" +
                "            \"field\" : \"myDateTime\"\n" +
                "          }\n" +
                "        }\n" +
                "      }\n" +
                "    } ]\n" +
                "  }\n" +
                "}";

        CHAINED_ORS_AND_ANDS = "{\n" +
                "  \"bool\" : {\n" +
                "    \"must\" : [ {\n" +
                "      \"bool\" : {\n" +
                "        \"should\" : [ {\n" +
                "          \"bool\" : {\n" +
                "            \"must\" : [ {\n" +
                "              \"bool\" : {\n" +
                "                \"should\" : [ {\n" +
                "                  \"term\" : {\n" +
                "                    \"myString\" : \"thing\"\n" +
                "                  }\n" +
                "                }, {\n" +
                "                  \"range\" : {\n" +
                "                    \"myInteger\" : {\n" +
                "                      \"from\" : 0,\n" +
                "                      \"to\" : null,\n" +
                "                      \"include_lower\" : false,\n" +
                "                      \"include_upper\" : true\n" +
                "                    }\n" +
                "                  }\n" +
                "                } ]\n" +
                "              }\n" +
                "            }, {\n" +
                "              \"range\" : {\n" +
                "                \"myInteger\" : {\n" +
                "                  \"from\" : null,\n" +
                "                  \"to\" : 5,\n" +
                "                  \"include_lower\" : true,\n" +
                "                  \"include_upper\" : false\n" +
                "                }\n" +
                "              }\n" +
                "            }, {\n" +
                "              \"terms\" : {\n" +
                "                \"myLong\" : [ 0, 1, 2 ]\n" +
                "              }\n" +
                "            } ]\n" +
                "          }\n" +
                "        }, {\n" +
                "          \"range\" : {\n" +
                "            \"myDouble\" : {\n" +
                "              \"from\" : null,\n" +
                "              \"to\" : 2.9,\n" +
                "              \"include_lower\" : true,\n" +
                "              \"include_upper\" : true\n" +
                "            }\n" +
                "          }\n" +
                "        }, {\n" +
                "          \"term\" : {\n" +
                "            \"myBoolean\" : false\n" +
                "          }\n" +
                "        } ]\n" +
                "      }\n" +
                "    }, {\n" +
                "      \"bool\" : {\n" +
                "        \"must_not\" : {\n" +
                "          \"exists\" : {\n" +
                "            \"field\" : \"myDateTime\"\n" +
                "          }\n" +
                "        }\n" +
                "      }\n" +
                "    } ]\n" +
                "  }\n" +
                "}";


        SUB_QUERY = "{\n" +
                "  \"bool\" : {\n" +
                "    \"must\" : [ {\n" +
                "      \"nested\" : {\n" +
                "        \"query\" : {\n" +
                "          \"bool\" : {\n" +
                "            \"must\" : [ {\n" +
                "              \"term\" : {\n" +
                "                \"myString\" : \"Thing\"\n" +
                "              }\n" +
                "            }, {\n" +
                "              \"bool\" : {\n" +
                "                \"must_not\" : {\n" +
                "                  \"exists\" : {\n" +
                "                    \"field\" : \"myLong\"\n" +
                "                  }\n" +
                "                }\n" +
                "              }\n" +
                "            } ]\n" +
                "          }\n" +
                "        },\n" +
                "        \"path\" : \"mySubList\"\n" +
                "      }\n" +
                "    }, {\n" +
                "      \"term\" : {\n" +
                "        \"myBoolean\" : true\n" +
                "      }\n" +
                "    } ]\n" +
                "  }\n" +
                "}";


        NULL_EQUALITY = "{\n" +
                "  \"bool\" : {\n" +
                "    \"must_not\" : {\n" +
                "      \"exists\" : {\n" +
                "        \"field\" : \"myString\"\n" +
                "      }\n" +
                "    }\n" +
                "  }\n" +
                "}";

        NULL_INEQUALITY = "{\n" +
                "  \"exists\" : {\n" +
                "    \"field\" : \"myString\"\n" +
                "  }\n" +
                "}";
    }


    @Override
    protected BasicEsVisitor getVisitor() {
        return new BasicEsVisitor();
    }

    @Override
    protected void compare(String expected, org.elasticsearch.index.query.QueryBuilder converted) {
        assertEquals(expected, converted.toString());
    }

}
