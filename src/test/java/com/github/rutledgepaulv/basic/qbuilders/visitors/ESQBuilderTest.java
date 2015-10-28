package com.github.rutledgepaulv.basic.qbuilders.visitors;

import com.github.rutledgepaulv.basic.QBuilderTestBase;
import com.github.rutledgepaulv.basic.qbuilders.visitors.basic.BasicEsVisitor;

import static org.junit.Assert.assertEquals;

public class ESQBuilderTest
        extends QBuilderTestBase<BasicEsVisitor, org.elasticsearch.index.query.FilterBuilder> {

    public ESQBuilderTest() {

        String_EQ = "{\n" +
                "  \"term\" : {\n" +
                "    \"myString\" : \"abcdefg\"\n" +
                "  }\n" +
                "}";

        String_NE = "{\n" +
                "  \"not\" : {\n" +
                "    \"filter\" : {\n" +
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

        String_EX = "{\n" +
                "  \"exists\" : {\n" +
                "    \"field\" : \"myString\"\n" +
                "  }\n" +
                "}";

        String_DNE = "{\n" +
                "  \"missing\" : {\n" +
                "    \"field\" : \"myString\"\n" +
                "  }\n" +
                "}";

        String_IN = "{\n" +
                "  \"terms\" : {\n" +
                "    \"myString\" : [ \"a\", \"b\", \"c\" ]\n" +
                "  }\n" +
                "}";

        String_NIN = "{\n" +
                "  \"not\" : {\n" +
                "    \"filter\" : {\n" +
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
                "  \"missing\" : {\n" +
                "    \"field\" : \"myBoolean\"\n" +
                "  }\n" +
                "}";

        Short_EQ = "{\n" +
                "  \"term\" : {\n" +
                "    \"myShort\" : 100\n" +
                "  }\n" +
                "}";

        Short_NE = "{\n" +
                "  \"not\" : {\n" +
                "    \"filter\" : {\n" +
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
                "  \"missing\" : {\n" +
                "    \"field\" : \"myShort\"\n" +
                "  }\n" +
                "}";

        Short_IN = "{\n" +
                "  \"terms\" : {\n" +
                "    \"myShort\" : [ 98, 99, 100 ]\n" +
                "  }\n" +
                "}";

        Short_NIN = "{\n" +
                "  \"not\" : {\n" +
                "    \"filter\" : {\n" +
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
                "  \"not\" : {\n" +
                "    \"filter\" : {\n" +
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
                "  \"missing\" : {\n" +
                "    \"field\" : \"myInteger\"\n" +
                "  }\n" +
                "}";

        Integer_IN = "{\n" +
                "  \"terms\" : {\n" +
                "    \"myInteger\" : [ 98, 99, 100 ]\n" +
                "  }\n" +
                "}";

        Integer_NIN = "{\n" +
                "  \"not\" : {\n" +
                "    \"filter\" : {\n" +
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
                "  \"not\" : {\n" +
                "    \"filter\" : {\n" +
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
                "  \"missing\" : {\n" +
                "    \"field\" : \"myLong\"\n" +
                "  }\n" +
                "}";

        Long_IN = "{\n" +
                "  \"terms\" : {\n" +
                "    \"myLong\" : [ 98, 99, 100 ]\n" +
                "  }\n" +
                "}";

        Long_NIN = "{\n" +
                "  \"not\" : {\n" +
                "    \"filter\" : {\n" +
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
                "  \"not\" : {\n" +
                "    \"filter\" : {\n" +
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
                "  \"missing\" : {\n" +
                "    \"field\" : \"myFloat\"\n" +
                "  }\n" +
                "}";

        Float_IN = "{\n" +
                "  \"terms\" : {\n" +
                "    \"myFloat\" : [ 98.0, 99.0, 100.0 ]\n" +
                "  }\n" +
                "}";

        Float_NIN = "{\n" +
                "  \"not\" : {\n" +
                "    \"filter\" : {\n" +
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
                "  \"not\" : {\n" +
                "    \"filter\" : {\n" +
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
                "  \"missing\" : {\n" +
                "    \"field\" : \"myDouble\"\n" +
                "  }\n" +
                "}";

        Double_IN = "{\n" +
                "  \"terms\" : {\n" +
                "    \"myDouble\" : [ 98.0, 99.0, 100.0 ]\n" +
                "  }\n" +
                "}";

        Double_NIN = "{\n" +
                "  \"not\" : {\n" +
                "    \"filter\" : {\n" +
                "      \"terms\" : {\n" +
                "        \"myDouble\" : [ 101.0, 102.0, 103.0 ]\n" +
                "      }\n" +
                "    }\n" +
                "  }\n" +
                "}";

        DateTime_EQ = "{\n" +
                "  \"term\" : {\n" +
                "    \"myDateTime\" : \"1970-01-01T00:00:00.000Z\"\n" +
                "  }\n" +
                "}";


        DateTime_NE = "{\n" +
                "  \"not\" : {\n" +
                "    \"filter\" : {\n" +
                "      \"term\" : {\n" +
                "        \"myDateTime\" : \"1970-01-01T00:00:00.000Z\"\n" +
                "      }\n" +
                "    }\n" +
                "  }\n" +
                "}";

        DateTime_LT = "{\n" +
                "  \"range\" : {\n" +
                "    \"myDateTime\" : {\n" +
                "      \"from\" : null,\n" +
                "      \"to\" : \"1971-01-01T00:00:00.000Z\",\n" +
                "      \"include_lower\" : true,\n" +
                "      \"include_upper\" : false\n" +
                "    }\n" +
                "  }\n" +
                "}";

        DateTime_LTE = "{\n" +
                "  \"range\" : {\n" +
                "    \"myDateTime\" : {\n" +
                "      \"from\" : null,\n" +
                "      \"to\" : \"1971-01-01T00:00:00.000Z\",\n" +
                "      \"include_lower\" : true,\n" +
                "      \"include_upper\" : true\n" +
                "    }\n" +
                "  }\n" +
                "}";

        DateTime_GT = "{\n" +
                "  \"range\" : {\n" +
                "    \"myDateTime\" : {\n" +
                "      \"from\" : \"1970-01-01T00:00:00.000Z\",\n" +
                "      \"to\" : null,\n" +
                "      \"include_lower\" : false,\n" +
                "      \"include_upper\" : true\n" +
                "    }\n" +
                "  }\n" +
                "}";

        DateTime_GTE = "{\n" +
                "  \"range\" : {\n" +
                "    \"myDateTime\" : {\n" +
                "      \"from\" : \"1970-01-01T00:00:00.000Z\",\n" +
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
                "  \"missing\" : {\n" +
                "    \"field\" : \"myDateTime\"\n" +
                "  }\n" +
                "}";

        DateTime_BETWEEN = "{\n" +
                "  \"and\" : {\n" +
                "    \"filters\" : [ {\n" +
                "      \"range\" : {\n" +
                "        \"myDateTime\" : {\n" +
                "          \"from\" : \"1970-01-01T00:00:00.000Z\",\n" +
                "          \"to\" : null,\n" +
                "          \"include_lower\" : true,\n" +
                "          \"include_upper\" : true\n" +
                "        }\n" +
                "      }\n" +
                "    }, {\n" +
                "      \"range\" : {\n" +
                "        \"myDateTime\" : {\n" +
                "          \"from\" : null,\n" +
                "          \"to\" : \"1971-01-01T00:00:00.000Z\",\n" +
                "          \"include_lower\" : true,\n" +
                "          \"include_upper\" : true\n" +
                "        }\n" +
                "      }\n" +
                "    } ]\n" +
                "  }\n" +
                "}";

        INLINE_ANDING = "{\n" +
                "  \"and\" : {\n" +
                "    \"filters\" : [ {\n" +
                "      \"term\" : {\n" +
                "        \"myString\" : \"Thing\"\n" +
                "      }\n" +
                "    }, {\n" +
                "      \"missing\" : {\n" +
                "        \"field\" : \"myLong\"\n" +
                "      }\n" +
                "    } ]\n" +
                "  }\n" +
                "}";

        INLINE_ORING = "{\n" +
                "  \"or\" : {\n" +
                "    \"filters\" : [ {\n" +
                "      \"term\" : {\n" +
                "        \"myString\" : \"Thing\"\n" +
                "      }\n" +
                "    }, {\n" +
                "      \"missing\" : {\n" +
                "        \"field\" : \"myLong\"\n" +
                "      }\n" +
                "    } ]\n" +
                "  }\n" +
                "}";

        LIST_ANDING = "{\n" +
                "  \"and\" : {\n" +
                "    \"filters\" : [ {\n" +
                "      \"term\" : {\n" +
                "        \"myString\" : \"Thing\"\n" +
                "      }\n" +
                "    }, {\n" +
                "      \"missing\" : {\n" +
                "        \"field\" : \"myLong\"\n" +
                "      }\n" +
                "    } ]\n" +
                "  }\n" +
                "}";

        LIST_ORING = "{\n" +
                "  \"or\" : {\n" +
                "    \"filters\" : [ {\n" +
                "      \"term\" : {\n" +
                "        \"myString\" : \"Thing\"\n" +
                "      }\n" +
                "    }, {\n" +
                "      \"missing\" : {\n" +
                "        \"field\" : \"myLong\"\n" +
                "      }\n" +
                "    } ]\n" +
                "  }\n" +
                "}";


        LIST_ORING_OF_INLINE_ANDING = "{\n" +
                "  \"or\" : {\n" +
                "    \"filters\" : [ {\n" +
                "      \"and\" : {\n" +
                "        \"filters\" : [ {\n" +
                "          \"term\" : {\n" +
                "            \"myString\" : \"Thing\"\n" +
                "          }\n" +
                "        }, {\n" +
                "          \"missing\" : {\n" +
                "            \"field\" : \"myLong\"\n" +
                "          }\n" +
                "        } ]\n" +
                "      }\n" +
                "    }, {\n" +
                "      \"and\" : {\n" +
                "        \"filters\" : [ {\n" +
                "          \"not\" : {\n" +
                "            \"filter\" : {\n" +
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
                "  \"and\" : {\n" +
                "    \"filters\" : [ {\n" +
                "      \"or\" : {\n" +
                "        \"filters\" : [ {\n" +
                "          \"term\" : {\n" +
                "            \"myString\" : \"Thing\"\n" +
                "          }\n" +
                "        }, {\n" +
                "          \"missing\" : {\n" +
                "            \"field\" : \"myLong\"\n" +
                "          }\n" +
                "        } ]\n" +
                "      }\n" +
                "    }, {\n" +
                "      \"or\" : {\n" +
                "        \"filters\" : [ {\n" +
                "          \"not\" : {\n" +
                "            \"filter\" : {\n" +
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
                "  \"or\" : {\n" +
                "    \"filters\" : [ {\n" +
                "      \"and\" : {\n" +
                "        \"filters\" : [ {\n" +
                "          \"or\" : {\n" +
                "            \"filters\" : [ {\n" +
                "              \"term\" : {\n" +
                "                \"myString\" : \"Thing\"\n" +
                "              }\n" +
                "            }, {\n" +
                "              \"missing\" : {\n" +
                "                \"field\" : \"myLong\"\n" +
                "              }\n" +
                "            } ]\n" +
                "          }\n" +
                "        }, {\n" +
                "          \"or\" : {\n" +
                "            \"filters\" : [ {\n" +
                "              \"not\" : {\n" +
                "                \"filter\" : {\n" +
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
                "      \"or\" : {\n" +
                "        \"filters\" : [ {\n" +
                "          \"and\" : {\n" +
                "            \"filters\" : [ {\n" +
                "              \"term\" : {\n" +
                "                \"myString\" : \"Thing\"\n" +
                "              }\n" +
                "            }, {\n" +
                "              \"missing\" : {\n" +
                "                \"field\" : \"myLong\"\n" +
                "              }\n" +
                "            } ]\n" +
                "          }\n" +
                "        }, {\n" +
                "          \"and\" : {\n" +
                "            \"filters\" : [ {\n" +
                "              \"not\" : {\n" +
                "                \"filter\" : {\n" +
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
                "  \"and\" : {\n" +
                "    \"filters\" : [ {\n" +
                "      \"or\" : {\n" +
                "        \"filters\" : [ {\n" +
                "          \"and\" : {\n" +
                "            \"filters\" : [ {\n" +
                "              \"term\" : {\n" +
                "                \"myString\" : \"Thing\"\n" +
                "              }\n" +
                "            }, {\n" +
                "              \"missing\" : {\n" +
                "                \"field\" : \"myLong\"\n" +
                "              }\n" +
                "            } ]\n" +
                "          }\n" +
                "        }, {\n" +
                "          \"and\" : {\n" +
                "            \"filters\" : [ {\n" +
                "              \"not\" : {\n" +
                "                \"filter\" : {\n" +
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
                "      \"and\" : {\n" +
                "        \"filters\" : [ {\n" +
                "          \"or\" : {\n" +
                "            \"filters\" : [ {\n" +
                "              \"term\" : {\n" +
                "                \"myString\" : \"Thing\"\n" +
                "              }\n" +
                "            }, {\n" +
                "              \"missing\" : {\n" +
                "                \"field\" : \"myLong\"\n" +
                "              }\n" +
                "            } ]\n" +
                "          }\n" +
                "        }, {\n" +
                "          \"or\" : {\n" +
                "            \"filters\" : [ {\n" +
                "              \"not\" : {\n" +
                "                \"filter\" : {\n" +
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

    }


    @Override
    protected BasicEsVisitor getVisitor() {
        return new BasicEsVisitor();
    }

    @Override
    protected void compare(String expected, org.elasticsearch.index.query.FilterBuilder converted) {
        assertEquals(expected, converted.toString());
    }

}
