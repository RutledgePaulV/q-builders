package com.github.rutledgepaulv.basic.qbuilders.visitors;

import com.github.rutledgepaulv.basic.Q;
import com.github.rutledgepaulv.basic.QModel;
import com.github.rutledgepaulv.basic.qbuilders.conditions.Condition;
import com.github.rutledgepaulv.basic.qbuilders.visitors.basic.BasicPredicateVisitor;
import org.junit.Before;
import org.junit.Test;

import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;
import java.util.function.Predicate;
import java.util.stream.Collectors;

import static com.github.rutledgepaulv.basic.QModel.QueryModelPredef.*;
import static org.junit.Assert.assertEquals;

public class PredicateBuilderTest {

    private Set<Q> actual = new HashSet<>();

    private Q AAA = get((byte) 7, (short) 5, 4, 0, 3, 2, 'a', "testing1", "test_a", "a");
    private Q ABA = get((byte) 5, (short) 3, 6, 2, 1, 4, 'c', "testing3", "test_b", "ab", "a");
    private Q AAB = get((byte) 6, (short) 4, 5, 1, 2, 3, 'b', "testing2", "test_c", "abc", "ab", "a");
    private Q BAA = get((byte) 4, (short) 2, 7, 3, 0, 5, 'd', null, "test_d", "abcd", "abc", "ab", "a");
    private Q BAB = get((byte) 3, (short) 1, 0, 4, 7, 6, 'e', "testing5", "test_e", "abcde", "abcd", "abc", "ab", "a");
    private Q BBA = get((byte) 2, (short) 0, 1, 5, 6, 7, 'f', "testing6", "test_f");
    private Q ABB = get((byte) 1, (short) 7, 2, 6, 5, 0, 'g', null, "test_g");
    private Q BBB = get((byte) 0, (short) 6, 3, 7, 4, 1, 'h', "testing8", "test_h");


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
        compare(myString2().lexicallyAfter("test_a"), AAB, ABA, BBA, BAA, BAB, ABB, BBB);
    }

    @Test
    public void testGreaterThanOrEqualTo() {
        compare(myLong().gte(5L), BBA, ABB, BBB);
        compare(myString2().lexicallyNotBefore("test_a"), AAA, AAB, ABA, BBA, BAA, BAB, ABB, BBB);
    }

    @Test
    public void testLessThan() {
        compare(myLong().lt(5L), AAA, AAB, ABA, BAA, BAB);
        compare(myString2().lexicallyBefore("test_h"), AAA, AAB, ABA, BBA, BAA, BAB, ABB);
    }

    @Test
    public void testLessThanOrEqualTo() {
        compare(myLong().lte(5L), AAA, AAB, ABA, BAA, BAB, BBA);
        compare(myString2().lexicallyNotAfter("test_h"), AAA, AAB, ABA, BBA, BAA, BAB, ABB, BBB);
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

    @Test
    public void inNonListFieldArray() {
        compare(myString().in(Arrays.asList("testing6", "testing5")), BAB, BBA);
    }

    @Test
    public void inListFieldArray() {
        compare(myListOfStrings().in(Arrays.asList("abcd", "abc")), BAB, BAA, AAB);
    }

    @Test
    public void ninNonListFieldArray() {
        compare(myString().nin(Arrays.asList("testing6", "testing5")), AAA, AAB, ABA, BAA, ABB, BBB);
    }

    @Test
    public void ninListFieldArray() {
        compare(myListOfStrings().nin(Arrays.asList("abcd", "abc")), AAA, ABA, BBA, ABB, BBB);
    }

    @Test
    public void testAnding() {
        compare(myString().doesNotExist().and().myLong().eq(6L), ABB);
    }

    @Test
    public void testOring() {
        compare(myString().doesNotExist().or().myString().eq("testing8"), BAA, ABB, BBB);
    }

    private void compare(Condition<QModel> query, Q... expected) {
        Predicate<Q> predicate = query.query(new BasicPredicateVisitor<>());
        Set<Q> ex = new HashSet<>(Arrays.asList(expected));
        Set<Q> ac = actual.stream().filter(predicate).collect(Collectors.toSet());
        assertEquals(ex, ac);
    }


    private Q get(byte val1, short val2, int val3, long val4, float val5, double val6, char val7, String val8, String val9,
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
        q.setMyString2(val9);
        q.setMyListOfStrings(Arrays.asList(myStrings));
        return q;
    }

}
