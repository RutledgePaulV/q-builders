package com.github.rutledgepaulv.qbuilders.utilities;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.util.Arrays;

public final class ObjectUtils {
    private ObjectUtils() {
    }


    public static <T> T init(Class<T> clazz, Object... args) {
        try {
            Constructor<T> constructor = clazz.getConstructor((Class<?>[])
                    Arrays.stream(args).map(Object::getClass).toArray());

            return constructor.newInstance(args);

        } catch (NoSuchMethodException | InstantiationException | InvocationTargetException | IllegalAccessException e) {
            throw new RuntimeException("Could not instantiate class for provided arguments.");
        }
    }

}
