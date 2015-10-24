package com.github.rutledgepaulv.advanced;

import com.github.rutledgepaulv.basic.qbuilders.visitors.advanced.AdvancedMongoVisitor;
import org.junit.Test;
import org.springframework.data.mongodb.core.query.Criteria;

import static org.junit.Assert.assertEquals;

public class AdvancedQBuilderTest {

    @Test
    public void testRegexField() {
        Criteria mongo = new AdvancedQModel().regexable().regex("/^does-it-match$/g").query(new AdvancedMongoVisitor());
        assertEquals("{ \"regexable\" : { \"$regex\" : \"/^does-it-match$/g\"}}", mongo.getCriteriaObject().toString());
    }

}
