package com.github.rutledgepaulv.basic.qbuilders.utilities;

import java.lang.reflect.InvocationTargetException;
import java.util.Arrays;
import java.util.Objects;
import java.util.stream.IntStream;

@SuppressWarnings({"unchecked", "Convert2MethodRef"})
public final class ObjectUtils {
    private ObjectUtils(){}


    /**
     * Instantiate a class for the provided constructor arguments.
     *
     * @param clazz The class to instantiate
     *
     * @param args The arguments for the constructor.
     *             The constructor used will be determined from the arguments provided.
     *
     * @return The new instance.
     */
    public static <T> T init(Class<T> clazz, Object... args) {
        try {
            final Object[] arguments = args != null ? args : new Object[]{};

            return (T) Arrays.stream(clazz.getConstructors())
                    .filter(construct -> Objects.equals(arguments.length, construct.getParameterCount()))
                    .filter(construct -> IntStream.range(0, arguments.length)
                            .allMatch(val -> construct.getParameterTypes()[val].isAssignableFrom(arguments[val].getClass())))
                    .findFirst().orElseThrow(() -> new InstantiationException("Could not find compatible constructor."))
                    .newInstance(arguments);
        } catch (InstantiationException | InvocationTargetException | IllegalAccessException e) {
            throw new RuntimeException("Could not instantiate class for provided arguments.", e);
        }
    }

}
