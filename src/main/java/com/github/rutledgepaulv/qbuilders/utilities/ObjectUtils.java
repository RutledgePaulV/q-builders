/*
 *
 *  *  com.github.rutledgepaulv.qbuilders.utilities.ObjectUtils
 *  *  *
 *  *  * Copyright (C) 2016 Paul Rutledge <paul.v.rutledge@gmail.com>
 *  *  *
 *  *  * This software may be modified and distributed under the terms
 *  *  * of the MIT license.  See the LICENSE file for details.
 *  *
 *
 */

package com.github.rutledgepaulv.qbuilders.utilities;

import java.lang.reflect.InvocationTargetException;
import java.util.Arrays;
import java.util.Objects;
import java.util.stream.IntStream;

@SuppressWarnings({"unchecked", "Convert2MethodRef"})
public final class ObjectUtils {

    /**
     * Instantiate a class for the provided constructor arguments.
     *
     * @param clazz The class to instantiate
     *
     * @param args The arguments for the constructor.
     *             The constructor used will be determined from the arguments provided.
     *
     * @param <T> The type of the provided class and resulting instance.
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
