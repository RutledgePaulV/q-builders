package com.github.rutledgepaulv.qbuilders.visitors;

import com.github.rutledgepaulv.qbuilders.builders.QBuilder;
import com.github.rutledgepaulv.qbuilders.conditions.Condition;
import com.github.rutledgepaulv.qbuilders.nodes.ComparisonNode;
import com.github.rutledgepaulv.qbuilders.structures.FieldPath;
import org.elasticsearch.index.query.QueryBuilder;
import org.junit.Test;

import static org.junit.Assert.assertEquals;

public class DemonstrateCustomVisitor {

    public static class DemonstrateTypeThing extends QBuilder<DemonstrateTypeThing> {
    }


    public static class VariableFieldElasticsearchVisitor extends ElasticsearchVisitor {

        @Override
        protected QueryBuilder visit(ComparisonNode node, Context context) {

            if ("value".equals(node.getField().asKey())) {
                node.setField(new FieldPath(node.getField().asKey() + "_" + determineSuffix(single(node.getValues()))));
            }

            return super.visit(node, context);
        }

        private String determineSuffix(Object value) {
            if (value instanceof Number) {
                return "number";
            } else if (value instanceof String) {
                return "text";
            } else if (value instanceof Boolean) {
                return "bool";
            }

            return "";
        }

    }


    @Test
    public void test() {
        Condition<DemonstrateTypeThing> numQuery = new DemonstrateTypeThing().intNum("value").eq(2);
        Condition<DemonstrateTypeThing> textQuery = new DemonstrateTypeThing().string("value").eq("dadad");
        Condition<DemonstrateTypeThing> boolQuery = new DemonstrateTypeThing().bool("value").isTrue();

        assertEquals("{\n" +
                "  \"term\" : {\n" +
                "    \"value_number\" : 2\n" +
                "  }\n" +
                "}", numQuery.query(new VariableFieldElasticsearchVisitor(), new ElasticsearchVisitor.Context()).toString());


        assertEquals("{\n" +
                "  \"term\" : {\n" +
                "    \"value_text\" : \"dadad\"\n" +
                "  }\n" +
                "}", textQuery.query(new VariableFieldElasticsearchVisitor(), new ElasticsearchVisitor.Context()).toString());


        assertEquals("{\n" +
                "  \"term\" : {\n" +
                "    \"value_bool\" : true\n" +
                "  }\n" +
                "}", boolQuery.query(new VariableFieldElasticsearchVisitor(), new ElasticsearchVisitor.Context()).toString());

    }

}
