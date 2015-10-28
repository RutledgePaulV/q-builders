package com.github.rutledgepaulv.basic.qbuilders.visitors;

import com.github.rutledgepaulv.basic.QBuilderTestBase;
import com.github.rutledgepaulv.basic.qbuilders.visitors.basic.BasicMongoVisitor;
import org.springframework.data.mongodb.core.query.Criteria;

import static org.junit.Assert.assertEquals;

public class MongoQBuilderTest extends QBuilderTestBase<BasicMongoVisitor, Criteria> {

    public MongoQBuilderTest() {

        String_EQ = "{ \"myString\" : \"abcdefg\"}";
        String_NE = "{ \"myString\" : { \"$ne\" : \"abcdefg\"}}";
        String_LT = "{ \"myString\" : { \"$lt\" : \"abcdefg\"}}";
        String_GT = "{ \"myString\" : { \"$gt\" : \"abcdefg\"}}";
        String_EX = "{ \"myString\" : { \"$exists\" : true}}";
        String_DNE = "{ \"myString\" : { \"$exists\" : false}}";
        String_IN = "{ \"myString\" : { \"$in\" : [ \"a\" , \"b\" , \"c\"]}}";
        String_NIN = "{ \"myString\" : { \"$nin\" : [ \"d\" , \"e\" , \"f\"]}}";

        Boolean_TRUE = "{ \"myBoolean\" : true}";
        Boolean_FALSE = "{ \"myBoolean\" : false}";
        Boolean_EX = "{ \"myBoolean\" : { \"$exists\" : true}}";
        Boolean_DNE = "{ \"myBoolean\" : { \"$exists\" : false}}";

        Short_EQ = "{ \"myShort\" : 100}";
        Short_NE = "{ \"myShort\" : { \"$ne\" : 100}}";
        Short_LT = "{ \"myShort\" : { \"$lt\" : 100}}";
        Short_GT = "{ \"myShort\" : { \"$gt\" : 100}}";
        Short_LTE = "{ \"myShort\" : { \"$lte\" : 100}}";
        Short_GTE = "{ \"myShort\" : { \"$gte\" : 100}}";
        Short_EX = "{ \"myShort\" : { \"$exists\" : true}}";
        Short_DNE = "{ \"myShort\" : { \"$exists\" : false}}";
        Short_IN = "{ \"myShort\" : { \"$in\" : [ 98 , 99 , 100]}}";
        Short_NIN = "{ \"myShort\" : { \"$nin\" : [ 101 , 102 , 103]}}";

        Integer_EQ = "{ \"myInteger\" : 100}";
        Integer_NE = "{ \"myInteger\" : { \"$ne\" : 100}}";
        Integer_LT = "{ \"myInteger\" : { \"$lt\" : 100}}";
        Integer_GT = "{ \"myInteger\" : { \"$gt\" : 100}}";
        Integer_LTE = "{ \"myInteger\" : { \"$lte\" : 100}}";
        Integer_GTE = "{ \"myInteger\" : { \"$gte\" : 100}}";
        Integer_EX = "{ \"myInteger\" : { \"$exists\" : true}}";
        Integer_DNE = "{ \"myInteger\" : { \"$exists\" : false}}";
        Integer_IN = "{ \"myInteger\" : { \"$in\" : [ 98 , 99 , 100]}}";
        Integer_NIN = "{ \"myInteger\" : { \"$nin\" : [ 101 , 102 , 103]}}";

        Long_EQ = "{ \"myLong\" : 100}";
        Long_NE = "{ \"myLong\" : { \"$ne\" : 100}}";
        Long_LT = "{ \"myLong\" : { \"$lt\" : 100}}";
        Long_GT = "{ \"myLong\" : { \"$gt\" : 100}}";
        Long_LTE = "{ \"myLong\" : { \"$lte\" : 100}}";
        Long_GTE = "{ \"myLong\" : { \"$gte\" : 100}}";
        Long_EX = "{ \"myLong\" : { \"$exists\" : true}}";
        Long_DNE = "{ \"myLong\" : { \"$exists\" : false}}";
        Long_IN = "{ \"myLong\" : { \"$in\" : [ 98 , 99 , 100]}}";
        Long_NIN = "{ \"myLong\" : { \"$nin\" : [ 101 , 102 , 103]}}";

        Float_EQ = "{ \"myFloat\" : 100.0}";
        Float_NE = "{ \"myFloat\" : { \"$ne\" : 100.0}}";
        Float_LT = "{ \"myFloat\" : { \"$lt\" : 100.0}}";
        Float_GT = "{ \"myFloat\" : { \"$gt\" : 100.0}}";
        Float_LTE = "{ \"myFloat\" : { \"$lte\" : 100.0}}";
        Float_GTE = "{ \"myFloat\" : { \"$gte\" : 100.0}}";
        Float_EX = "{ \"myFloat\" : { \"$exists\" : true}}";
        Float_DNE = "{ \"myFloat\" : { \"$exists\" : false}}";
        Float_IN = "{ \"myFloat\" : { \"$in\" : [ 98.0 , 99.0 , 100.0]}}";
        Float_NIN = "{ \"myFloat\" : { \"$nin\" : [ 101.0 , 102.0 , 103.0]}}";

        Double_EQ = "{ \"myDouble\" : 100.0}";
        Double_NE = "{ \"myDouble\" : { \"$ne\" : 100.0}}";
        Double_LT = "{ \"myDouble\" : { \"$lt\" : 100.0}}";
        Double_GT = "{ \"myDouble\" : { \"$gt\" : 100.0}}";
        Double_LTE = "{ \"myDouble\" : { \"$lte\" : 100.0}}";
        Double_GTE = "{ \"myDouble\" : { \"$gte\" : 100.0}}";
        Double_EX = "{ \"myDouble\" : { \"$exists\" : true}}";
        Double_DNE = "{ \"myDouble\" : { \"$exists\" : false}}";
        Double_IN = "{ \"myDouble\" : { \"$in\" : [ 98.0 , 99.0 , 100.0]}}";
        Double_NIN = "{ \"myDouble\" : { \"$nin\" : [ 101.0 , 102.0 , 103.0]}}";

        DateTime_EQ = "{ \"myDateTime\" : { \"$date\" : \"1970-01-01T00:00:00.000Z\"}}";
        DateTime_NE = "{ \"myDateTime\" : { \"$ne\" : { \"$date\" : \"1970-01-01T00:00:00.000Z\"}}}";
        DateTime_LT = "{ \"myDateTime\" : { \"$lt\" : { \"$date\" : \"1971-01-01T00:00:00.000Z\"}}}";
        DateTime_LTE = "{ \"myDateTime\" : { \"$lte\" : { \"$date\" : \"1971-01-01T00:00:00.000Z\"}}}";
        DateTime_GT = "{ \"myDateTime\" : { \"$gt\" : { \"$date\" : \"1970-01-01T00:00:00.000Z\"}}}";
        DateTime_GTE = "{ \"myDateTime\" : { \"$gte\" : { \"$date\" : \"1970-01-01T00:00:00.000Z\"}}}";
        DateTime_EX = "{ \"myDateTime\" : { \"$exists\" : true}}";
        DateTime_DNE = "{ \"myDateTime\" : { \"$exists\" : false}}";
        DateTime_BETWEEN = "{ \"$and\" : [ { \"myDateTime\" : { \"$gte\" : { \"$date\" : \"1970-01-01T00:00:00.000Z\"}}} , " +
                "{ \"myDateTime\" : { \"$lte\" : { \"$date\" : \"1971-01-01T00:00:00.000Z\"}}}]}";


        INLINE_ANDING = "{ \"$and\" : [ { \"myString\" : \"Thing\"} , { \"myLong\" : { \"$exists\" : false}}]}";

        INLINE_ORING = "{ \"$or\" : [ { \"myString\" : \"Thing\"} , { \"myLong\" : { \"$exists\" : false}}]}";

        LIST_ANDING = "{ \"$and\" : [ { \"myString\" : \"Thing\"} , { \"myLong\" : { \"$exists\" : false}}]}";

        LIST_ORING = "{ \"$or\" : [ { \"myString\" : \"Thing\"} , { \"myLong\" : { \"$exists\" : false}}]}";


        LIST_ORING_OF_INLINE_ANDING = "{ \"$or\" : [ { \"$and\" : [ { \"myString\" : \"Thing\"} , " +
                "{ \"myLong\" : { \"$exists\" : false}}]} , { \"$and\" : [ { \"myString\" : { \"$ne\" : \"Cats\"}} ," +
                " { \"myLong\" : { \"$gt\" : 30}}]}]}";


        LIST_ANDING_OF_INLINE_ORING = "{ \"$and\" : [ { \"$or\" : [ { \"myString\" : \"Thing\"} , " +
                "{ \"myLong\" : { \"$exists\" : false}}]} , { \"$or\" : [ { \"myString\" : { \"$ne\" : \"Cats\"}} ," +
                " { \"myLong\" : { \"$gt\" : 30}}]}]}";


        LIST_ANDING_OR_LIST_ORING = "{ \"$or\" : [ { \"$and\" : [ { \"$or\" : [ { \"myString\" : \"Thing\"} ," +
                " { \"myLong\" : { \"$exists\" : false}}]} , { \"$or\" : [ { \"myString\" : { \"$ne\" : \"Cats\"}} ," +
                " { \"myLong\" : { \"$gt\" : 30}}]}]} , { \"$or\" : [ { \"$and\" : [ { \"myString\" : \"Thing\"} ," +
                " { \"myLong\" : { \"$exists\" : false}}]} , { \"$and\" : [ { \"myString\" : { \"$ne\" : \"Cats\"}} ," +
                " { \"myLong\" : { \"$gt\" : 30}}]}]}]}";


        LIST_ORING_AND_LIST_ANDING = "{ \"$and\" : [ { \"$or\" : [ { \"$and\" : [ { \"myString\" : \"Thing\"} , " +
                "{ \"myLong\" : { \"$exists\" : false}}]} , { \"$and\" : [ { \"myString\" : { \"$ne\" : \"Cats\"}} ," +
                " { \"myLong\" : { \"$gt\" : 30}}]}]} , { \"$and\" : [ { \"$or\" : [ { \"myString\" : \"Thing\"} , " +
                "{ \"myLong\" : { \"$exists\" : false}}]} , { \"$or\" : [ { \"myString\" : { \"$ne\" : \"Cats\"}} , " +
                "{ \"myLong\" : { \"$gt\" : 30}}]}]}]}";
    }



    @Override
    protected BasicMongoVisitor getVisitor() {
        return new BasicMongoVisitor();
    }

    @Override
    protected void compare(String expected, Criteria converted) {
        assertEquals(expected,  converted.getCriteriaObject().toString());
    }

}
