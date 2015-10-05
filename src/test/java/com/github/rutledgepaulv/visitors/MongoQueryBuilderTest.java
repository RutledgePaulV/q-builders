package com.github.rutledgepaulv.visitors;

import com.github.rutledgepaulv.base.QueryBuilderTestBase;
import org.springframework.data.mongodb.core.query.Criteria;

import static org.junit.Assert.assertEquals;

public class MongoQueryBuilderTest extends QueryBuilderTestBase<MongoCriteriaVisitor, Criteria> {

    public MongoQueryBuilderTest() {

        String_EQ = "{ \"myString\" : \"abcdefg\"}";
        String_NE = "{ \"myString\" : { \"$ne\" : \"abcdefg\"}}";
        String_LT = "{ \"myString\" : { \"$lt\" : \"abcdefg\"}}";
        String_GT = "{ \"myString\" : { \"$gt\" : \"abcdefg\"}}";
        String_EX = "{ \"myString\" : { \"$exists\" : true}}";
        String_DNE = "{ \"myString\" : { \"$exists\" : false}}";

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

        Integer_EQ = "{ \"myInteger\" : 100}";
        Integer_NE = "{ \"myInteger\" : { \"$ne\" : 100}}";
        Integer_LT = "{ \"myInteger\" : { \"$lt\" : 100}}";
        Integer_GT = "{ \"myInteger\" : { \"$gt\" : 100}}";
        Integer_LTE = "{ \"myInteger\" : { \"$lte\" : 100}}";
        Integer_GTE = "{ \"myInteger\" : { \"$gte\" : 100}}";
        Integer_EX = "{ \"myInteger\" : { \"$exists\" : true}}";
        Integer_DNE = "{ \"myInteger\" : { \"$exists\" : false}}";

        Long_EQ = "{ \"myLong\" : 100}";
        Long_NE = "{ \"myLong\" : { \"$ne\" : 100}}";
        Long_LT = "{ \"myLong\" : { \"$lt\" : 100}}";
        Long_GT = "{ \"myLong\" : { \"$gt\" : 100}}";
        Long_LTE = "{ \"myLong\" : { \"$lte\" : 100}}";
        Long_GTE = "{ \"myLong\" : { \"$gte\" : 100}}";
        Long_EX = "{ \"myLong\" : { \"$exists\" : true}}";
        Long_DNE = "{ \"myLong\" : { \"$exists\" : false}}";

        Float_EQ = "{ \"myFloat\" : 100.0}";
        Float_NE = "{ \"myFloat\" : { \"$ne\" : 100.0}}";
        Float_LT = "{ \"myFloat\" : { \"$lt\" : 100.0}}";
        Float_GT = "{ \"myFloat\" : { \"$gt\" : 100.0}}";
        Float_LTE = "{ \"myFloat\" : { \"$lte\" : 100.0}}";
        Float_GTE = "{ \"myFloat\" : { \"$gte\" : 100.0}}";
        Float_EX = "{ \"myFloat\" : { \"$exists\" : true}}";
        Float_DNE = "{ \"myFloat\" : { \"$exists\" : false}}";

        Double_EQ = "{ \"myDouble\" : 100.0}";
        Double_NE = "{ \"myDouble\" : { \"$ne\" : 100.0}}";
        Double_LT = "{ \"myDouble\" : { \"$lt\" : 100.0}}";
        Double_GT = "{ \"myDouble\" : { \"$gt\" : 100.0}}";
        Double_LTE = "{ \"myDouble\" : { \"$lte\" : 100.0}}";
        Double_GTE = "{ \"myDouble\" : { \"$gte\" : 100.0}}";
        Double_EX = "{ \"myDouble\" : { \"$exists\" : true}}";
        Double_DNE = "{ \"myDouble\" : { \"$exists\" : false}}";


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
    protected MongoCriteriaVisitor getVisitor() {
        return new MongoCriteriaVisitor();
    }

    @Override
    protected void compare(String expected, Criteria converted) {
        assertEquals(expected,  converted.getCriteriaObject().toString());
    }

}
