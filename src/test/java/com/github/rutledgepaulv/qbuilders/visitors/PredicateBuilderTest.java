package com.github.rutledgepaulv.qbuilders.visitors;

import com.github.rutledgepaulv.base.Q;
import com.github.rutledgepaulv.base.QModel;
import com.github.rutledgepaulv.qbuilders.conditions.CompleteCondition;
import org.junit.Before;
import org.junit.Test;

import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;
import java.util.function.Predicate;
import java.util.stream.Collectors;

import static com.github.rutledgepaulv.base.QModel.QueryModelPredef.myListOfStrings;
import static com.github.rutledgepaulv.base.QModel.QueryModelPredef.myLong;
import static com.github.rutledgepaulv.base.QModel.QueryModelPredef.myString;
import static org.junit.Assert.assertEquals;

public class PredicateBuilderTest {

    private Set<Q> actual = new HashSet<>();

    private Q AAA = get((byte) 7, (short) 5, 4, 0, 3, 2, 'a', "testing1", "a");
    private Q ABA = get((byte) 5, (short) 3, 6, 2, 1, 4, 'c', "testing3", "ab", "a");
    private Q AAB = get((byte) 6, (short) 4, 5, 1, 2, 3, 'b', "testing2", "abc", "ab", "a");
    private Q BAA = get((byte) 4, (short) 2, 7, 3, 0, 5, 'd', null, "abcd", "abc", "ab", "a");
    private Q BAB = get((byte) 3, (short) 1, 0, 4, 7, 6, 'e', "testing5", "abcde", "abcd", "abc", "ab", "a");
    private Q BBA = get((byte) 2, (short) 0, 1, 5, 6, 7, 'f', "testing6");
    private Q ABB = get((byte) 1, (short) 7, 2, 6, 5, 0, 'g', null);
    private Q BBB = get((byte) 0, (short) 6, 3, 7, 4, 1, 'h', "testing8");


    @Before
    public void setUp() {
        actual.clear();
        actual.addAll(Arrays.asList(AAA, AAB, ABA, BBA, BAA, BAB, ABB, BBB));
    }


    @Test
    public void testEquality() {
        compare(myLong().eq(4L), BAB);
    }

    @Test
    public void testEqualityListField() {
        compare(myListOfStrings().eq("abcd"), BAA, BAB);
    }

    @Test
    public void testInequality() {
        compare(myLong().ne(4L), AAA, AAB, ABA, BAA, BBA, ABB, BBB);
    }

    @Test
    public void testInequalityListField() {
        compare(myListOfStrings().ne("abcd"), AAA, AAB, ABA, BBA, ABB, BBB);
    }

    @Test
    public void testGreaterThan() {
        compare(myLong().gt(5L), ABB, BBB);
    }

    @Test
    public void testGreaterThanOrEqualTo() {
        compare(myLong().gte(5L), BBA, ABB, BBB);
    }

    @Test
    public void testLessThan() {
        compare(myLong().lt(5L), AAA, AAB, ABA, BAA, BAB);
    }

    @Test
    public void testLessThanOrEqualTo() {
        compare(myLong().lte(5L), AAA, AAB, ABA, BAA, BAB, BBA);
    }

    @Test
    public void exists() {
        compare(myString().exists(), AAA, AAB, ABA, BAB, BBA, BBB);
    }

    @Test
    public void existsListField() {
        compare(myListOfStrings().exists(), AAA, AAB, ABA, BBA, BAA, BAB, ABB, BBB);
    }

    @Test
    public void doesNotExist() {
        compare(myString().doesNotExist(), BAA, ABB);
    }

    @Test
    public void testDoesNotExistListField() {
        compare(myListOfStrings().doesNotExist());
    }

    @Test
    public void inNonListField() {
        compare(myString().in("testing6", "testing5"), BAB, BBA);
    }

    @Test
    public void inListField() {
        compare(myListOfStrings().in("abcd", "abc"), BAB, BAA, AAB);
    }

    @Test
    public void ninNonListField() {
        compare(myString().nin("testing6", "testing5"), AAA, AAB, ABA, BAA, ABB, BBB);
    }

    @Test
    public void ninListField() {
        compare(myListOfStrings().nin("abcd", "abc"), AAA, ABA, BBA, ABB, BBB);
    }

    private void compare(CompleteCondition<QModel> query, Q... expected) {
        Predicate<Q> predicate = query.query(new PredicateVisitor<>());
        Set<Q> ex = new HashSet<>(Arrays.asList(expected));
        Set<Q> ac = actual.stream().filter(predicate).collect(Collectors.toSet());
        assertEquals(ex, ac);
    }


    private Q get(byte val1, short val2, int val3, long val4, float val5, double val6, char val7, String val8,
            String... myStrings) {
        Q q = new Q();
        q.setMyByte(val1);
        q.setMyShort(val2);
        q.setMyInteger(val3);
        q.setMyLong(val4);
        q.setMyFloat(val5);
        q.setMyDouble(val6);
        q.setMyCharacter(val7);
        q.setMyString(val8);
        q.setMyListOfStrings(Arrays.asList(myStrings));
        return q;
    }

}
