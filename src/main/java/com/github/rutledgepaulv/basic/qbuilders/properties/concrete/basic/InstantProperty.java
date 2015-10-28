package com.github.rutledgepaulv.basic.qbuilders.properties.concrete.basic;

import com.github.rutledgepaulv.basic.qbuilders.conditions.Partial;
import com.github.rutledgepaulv.basic.qbuilders.properties.virtual.InstantLikeProperty;

import java.time.Instant;

public interface InstantProperty<T extends Partial<T>> extends InstantLikeProperty<T, Instant> {


}
