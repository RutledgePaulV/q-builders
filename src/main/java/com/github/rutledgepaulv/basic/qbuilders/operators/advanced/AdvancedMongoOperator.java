package com.github.rutledgepaulv.basic.qbuilders.operators.advanced;

import com.github.rutledgepaulv.basic.qbuilders.operators.basic.ComparisonOperator;

/**
 * A sample of how advanced, backend specific operators may work.
 */
public class AdvancedMongoOperator extends ComparisonOperator {

    protected AdvancedMongoOperator(String representation) {
        super(representation);
    }


    public static final AdvancedMongoOperator REGEX = new AdvancedMongoOperator("REGEX");

}
