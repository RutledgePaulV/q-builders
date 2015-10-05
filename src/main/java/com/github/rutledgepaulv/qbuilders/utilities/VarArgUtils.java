package com.github.rutledgepaulv.qbuilders.utilities;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

public final class VarArgUtils {
    private VarArgUtils(){
    }

    @SafeVarargs
    public static <S> List<S> combine(S v1, S v2, S... vn){
        List<S> all = new ArrayList<>();
        all.add(v1);
        all.add(v2);
        all.addAll(Arrays.asList(vn));
        return all;
    }

    @SafeVarargs
    public static <S> List<S> list(S... vn) {
        return Arrays.asList(vn);
    }

}
