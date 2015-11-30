package com.github.rutledgepaulv.basic.qbuilders.utilities;

import java.lang.reflect.Constructor;
import java.lang.reflect.InvocationTargetException;
import java.util.Arrays;
import java.util.Objects;

@SuppressWarnings({"unchecked", "Convert2MethodRef"})
public abstract class ObjectUtils {


    public static <T> T init(Class<T> clazz, Object... args) {
        try {
            Class<?>[] clazzes = Arrays.stream(args).map(Object::getClass).toArray(length -> new Class<?>[length]);

            Constructor<?> constructor = Arrays.stream(clazz.getConstructors())
                    .filter(construct -> Objects.equals(clazzes.length, construct.getParameterCount()))
                    .filter(construct -> {
                        Class<?>[] params = construct.getParameterTypes();
                        for (int i = 0; i < clazzes.length; i++) {
                            if(!params[i].isAssignableFrom(clazzes[i])) {
                                return false;
                            }
                        }
                        return true;
                    }).findFirst().orElseThrow(() -> new RuntimeException("Could not find compatible constructor."));

            return (T) constructor.newInstance(args);

        } catch (InstantiationException | InvocationTargetException | IllegalAccessException e) {
            throw new RuntimeException("Could not instantiate class for provided arguments.");
        }
    }

}
