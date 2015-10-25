package com.github.rutledgepaulv.advanced;

import com.github.rutledgepaulv.basic.qbuilders.builders.QBuilder;

import static com.github.rutledgepaulv.dirty.FieldUtil.getCurrentMethodName;

public class AdvancedQModel extends QBuilder<AdvancedQModel> {

    public AdvancedStringField<AdvancedQModel> regexable() {
        return prop(getCurrentMethodName(), AdvancedStringFieldDelegate.class, AdvancedStringField.class);
    }

}
