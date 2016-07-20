/*
 *
 *  *  com.github.rutledgepaulv.qbuilders.visitors.PredicateVisitorTest
 *  *  *
 *  *  * Copyright (C) 2016 Paul Rutledge <paul.v.rutledge@gmail.com>
 *  *  *
 *  *  * This software may be modified and distributed under the terms
 *  *  * of the MIT license.  See the LICENSE file for details.
 *  *
 *
 */

package com.github.rutledgepaulv.qbuilders.visitors;

import com.github.rutledgepaulv.qbuilders.builders.QBuilder;
import com.github.rutledgepaulv.qbuilders.conditions.Condition;
import com.github.rutledgepaulv.qbuilders.properties.concrete.ConditionProperty;
import com.github.rutledgepaulv.qbuilders.properties.concrete.EnumProperty;
import com.github.rutledgepaulv.qbuilders.properties.concrete.StringProperty;
import com.github.rutledgepaulv.testsupport.DomainModel;
import com.github.rutledgepaulv.testsupport.QueryModel;
import org.junit.Before;
import org.junit.Test;

import java.util.*;
import java.util.function.Predicate;
import java.util.stream.Collectors;

import static com.github.rutledgepaulv.testsupport.QueryModel.QueryModelPredef.*;
import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;

public class PredicateVisitorTest {

    private Set<DomainModel> actual = new HashSet<>();

    private DomainModel AAA = get((byte) 7, (short) 5, 4, 0, 3, 2, 'a', "testing1", "test_a", "a");
    private DomainModel ABA = get((byte) 5, (short) 3, 6, 2, 1, 4, 'c', "testing3", "test_b", "ab", "a");
    private DomainModel AAB = get((byte) 6, (short) 4, 5, 1, 2, 3, 'b', "testing2", "test_c", "abc", "ab", "a");
    private DomainModel BAA = get((byte) 4, (short) 2, 7, 3, 0, 5, 'd', null, "test_d", "abcd", "abc", "ab", "a");
    private DomainModel BAB = get((byte) 3, (short) 1, 0, 4, 7, 6, 'e', "testing5", "test_e", "abcde", "abcd", "abc", "ab", "a");
    private DomainModel BBA = get((byte) 2, (short) 0, 1, 5, 6, 7, 'f', "testing6", "test_f");
    private DomainModel ABB = get((byte) 1, (short) 7, 2, 6, 5, 0, 'g', null, "test_g");
    private DomainModel BBB = get((byte) 0, (short) 6, 3, 7, 4, 1, 'h', "testing8", "test_h");


    @Before
    public void setUp() {
        actual.clear();

        actual.addAll(Arrays.asList(AAA, AAB, ABA, BBA, BAA, BAB, ABB, BBB));
        Set<DomainModel> clones = actual.stream().map(DomainModel::copy).collect(Collectors.toSet());

        actual.forEach(entry -> entry.getMySubList().addAll(clones.stream()
                .filter(thing -> !thing.equals(entry))
                .collect(Collectors.toList())));
    }

    @Test
    public void testRegex() {
        compare(myListOfStrings().pattern(".*cd$"), BAA, BAB);
    }

    @Test
    public void subquery(){
        compare(mySubList().any(myLong().gt(4L).and().myString().doesNotExist()),
                AAA, ABA, AAB, BAA, BAB, BBA, BBB);
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

    public enum Icecream {
        GINGER_ROOT(), GOBLIN_BLOOD()
    }

    public class User {

        private String firstName;
        public String middleName;
        private String lastName;
        private Icecream favouriteFlavor;
        private List<User> friends = new LinkedList<>();
        private User[] neighbors = new User[2];
        private User bestFriend;

        public User withFriends(User... newFriends) {
            Collections.addAll(friends, newFriends);
            return this;
        }


        public User firstName(String paul) {
            firstName = paul;
            return this;
        }

        public User lastName(String paul) {
            lastName = paul;
            return this;
        }

        public User bestFriend(User paul) {
            bestFriend = paul;
            return this;
        }

        public String getFirstName() {
            return firstName;
        }

        public String getLastName() {
            return lastName;
        }

        public List<User> getFriends() {
            return friends;
        }

        public User[] getNeighbors() {
            return neighbors;
        }

        public User getBestFriend() {
            return bestFriend;
        }

        public Icecream getFavouriteFlavor() {
            return favouriteFlavor;
        }
    }

    @SuppressWarnings("WeakerAccess")
    public class UserQuery extends QBuilder<UserQuery> {

        public StringProperty<UserQuery> firstName() {
            return string("firstName");
        }

        public StringProperty<UserQuery> middleName() {
            return string("middleName");
        }

        public StringProperty<UserQuery> lastName() {
            return string("lastName");
        }

        public ConditionProperty<UserQuery, UserQuery> bestFriend() {
            return condition("bestFriend");
        }

        public ConditionProperty<UserQuery, UserQuery> friends() {
            return condition("friends");
        }

        public ConditionProperty<UserQuery, UserQuery> neighbors() {
            return condition("neighbors");
        }

        public EnumProperty<UserQuery, Icecream> favouriteFlavor() {
            return enumeration("favouriteFlavor");
        }
    }

    public class Bruiser extends User {
        public String cruiser;
        public Optional<String> getCruiser() { return Optional.ofNullable(cruiser); }
    }

    @Test
    public void regularQueryTest() {
        List<User> people = new LinkedList<>();
        people.add(new User().withFriends(new User().firstName("James"), new User().firstName("Paul")));
        people.add(new User().withFriends(new User().firstName("Bob")));

        Condition<UserQuery> condition = new UserQuery().friends().any(new UserQuery().firstName().eq("Paul"));
        Predicate<User> query = condition.query(new PredicateVisitor<>());

        assertEquals(people.get(0), people.stream().filter(query).findFirst().get());
        assertEquals(1, people.stream().filter(query).count());
    }

    @Test
    public void regularQueryNoGetterTest() {
        List<User> people = new LinkedList<>();
        User paul = new User();
        paul.middleName = "Paul";
        people.add(new User().withFriends(new User().firstName("James"), paul));
        people.add(new User().withFriends(new User().firstName("Bob")));
        Condition<UserQuery> condition = new UserQuery().friends().any(new UserQuery().middleName().eq("Paul"));
        Predicate<User> query = condition.query(new PredicateVisitor<>());
        assertEquals(people.get(0), people.stream().filter(query).findFirst().get());
        assertEquals(1, people.stream().filter(query).count());
    }


    @Test
    public void regularQueryNoGetterSuperclassTest() {
        List<User> people = new LinkedList<>();
        Bruiser paul = new Bruiser();
        paul.middleName = "Braul";
        people.add(new User().withFriends(new User().firstName("James"), paul));
        people.add(new User().withFriends(new User().firstName("Bob")));
        Condition<UserQuery> condition = new UserQuery().friends().any(new UserQuery().middleName().eq("Braul"));
        Predicate<User> query = condition.query(new PredicateVisitor<>());
        assertEquals(people.get(0), people.stream().filter(query).findFirst().get());
        assertEquals(1, people.stream().filter(query).count());
    }

    @Test
    public void stringQueryTest() {
        List<User> people = new LinkedList<>();
        people.add(new User().firstName("James").withFriends(new User().firstName("Paul")));
        people.add(new User().firstName("Paul"));

        Condition<UserQuery> condition = new UserQuery().string("firstName").eq("Paul");
        Predicate<User> query = condition.query(new PredicateVisitor<>());

        assertEquals(people.get(1), people.stream().filter(query).findFirst().get());
        assertEquals(1, people.stream().filter(query).count());
    }

    @Test
    public void stringQueryMultipleLevelsTest() {
        List<User> people = new LinkedList<>();
        people.add(new User().bestFriend(new User().firstName("Geraldo")).firstName("Kanye"));
        people.add(new User().bestFriend(new User().firstName("James")).firstName("Kim"));
        people.add(new User().bestFriend(new User().firstName("Geraldo")).firstName("James"));

        Condition<UserQuery> condition = new UserQuery().string("bestFriend.firstName").eq("Geraldo");
        Predicate<User> query = condition.query(new PredicateVisitor<>());

        assertEquals(people.get(0), people.stream().filter(query).findFirst().get());
        assertEquals(2, people.stream().filter(query).count());
    }

    @Test
    public void stringQueryListTest() {
        List<User> people = new LinkedList<>();
        people.add(new User().withFriends(new User().firstName("Paul")).firstName("Dan"));
        people.add(new User().withFriends(new User().firstName("Durande")).firstName("Don"));

        Condition<UserQuery> condition = new UserQuery().string("friends.firstName").eq("Durande");
        Predicate<User> query = condition.query(new PredicateVisitor<>());

        assertEquals(people.get(1), people.stream().filter(query).findFirst().get());
        assertEquals(1, people.stream().filter(query).count());
    }

    @Test
    public void stringQueryMultipleListTest() {
        List<User> people = new LinkedList<>();
        people.add(new User().withFriends(new User().withFriends(new User().firstName("Durande"))));
        people.add(new User().withFriends(new User().withFriends(new User().firstName("Paul"))));
        people.add(new User().withFriends(new User().firstName("Durande")));

        Condition<UserQuery> condition = new UserQuery().string("friends.friends.firstName").eq("Durande");
        Predicate<User> query = condition.query(new PredicateVisitor<>());

        assertEquals(people.get(0), people.stream().filter(query).findFirst().get());
        assertEquals(1, people.stream().filter(query).count());
    }


    @Test
    public void stringQueryArrayTest() {
        List<User> people = new LinkedList<>();
        User person1 = new User().firstName("Gumbo");
        person1.neighbors[0] = new User().firstName("Durande");
        person1.neighbors[1] = new User().firstName("KangarooCat");
        people.add(person1);

        User person2 = new User().firstName("Mendy");
        person2.neighbors[0] = new User().firstName("Not Durande");
        person2.neighbors[1] = new User().firstName("KangarooCat");
        people.add(person2);

        Condition<UserQuery> condition = new UserQuery().string("neighbors.firstName").eq("Durande");
        Predicate<User> query = condition.query(new PredicateVisitor<>());

        assertEquals(people.get(0), people.stream().filter(query).findFirst().get());
        assertEquals(1, people.stream().filter(query).count());
    }


    @Test
    public void stringQueryNullTest() {
        List<User> people = new LinkedList<>();
        people.add(new User().withFriends(null, null));

        Condition<UserQuery> condition = new UserQuery().string("friends.firstName").eq("Durande");
        Predicate<User> query = condition.query(new PredicateVisitor<>());

        Object nothing = people.stream().filter(query).findAny();
        assertEquals(Optional.empty(), nothing);
    }

    @Test
    public void stringQueryNullMiddleTest() {
        List<User> people = new LinkedList<>();
        people.add(new User().bestFriend(null));

        Condition<UserQuery> condition = new UserQuery().string("friends.bestFriend.firstName").eq("Durande");
        Predicate<User> query = condition.query(new PredicateVisitor<>());

        Object nothing = people.stream().filter(query).findAny();
        assertEquals(Optional.empty(), nothing);
    }

    @Test
    public void stringQueryLogicTest() {
        Condition<UserQuery> ev = new UserQuery().string("firstName").eq("Geraldo").and().string("lastName").eq("Neumann");
        Condition<UserQuery> condition = new UserQuery().friends().any(ev);
        Predicate<User> query = condition.query(new PredicateVisitor<>());

        List<User> people = new LinkedList<>();
        people.add(new User().withFriends(new User().firstName("Geraldo").lastName("Neumann")));
        people.add(new User().withFriends(new User().firstName("Geraldo").lastName("Not Neumann")).firstName("Geraldo")
                .lastName("Neumann"));

        assertEquals(people.get(0), people.stream().filter(query).findFirst().get());
        assertEquals(1, people.stream().filter(query).count());
    }

    @Test
    public void enumTest() {
        User user = new User();
        user.favouriteFlavor = Icecream.GOBLIN_BLOOD;
        user.firstName = "Geraldo";
        Condition<UserQuery> query = new UserQuery().favouriteFlavor().eq(Icecream.GOBLIN_BLOOD);
        assertTrue(query.query(new PredicateVisitor<>()).test(user));
    }

    @Test
    public void existsTest() {
        User user = new User();
        user.favouriteFlavor = Icecream.GOBLIN_BLOOD;
        user.firstName = "Geraldo";
        Condition<UserQuery> query = new UserQuery().favouriteFlavor().exists();
        assertTrue(query.query(new PredicateVisitor<>()).test(user));
    }



    private void compare(Condition<QueryModel> query, DomainModel... expected) {
        Predicate<DomainModel> pred = query.query(new PredicateVisitor<>());
        Set<DomainModel> ex = new HashSet<>(Arrays.asList(expected));
        Set<DomainModel> ac = actual.stream().filter(pred).collect(Collectors.toSet());
        assertEquals(ex, ac);
    }


    private DomainModel get(byte val1, short val2, int val3, long val4, float val5, double val6, char val7, String val8, String val9,
            String... myStrings) {
        DomainModel domainModel = new DomainModel();
        domainModel.setMyByte(val1);
        domainModel.setMyShort(val2);
        domainModel.setMyInteger(val3);
        domainModel.setMyLong(val4);
        domainModel.setMyFloat(val5);
        domainModel.setMyDouble(val6);
        domainModel.setMyCharacter(val7);
        domainModel.setMyString(val8);
        domainModel.setMyString2(val9);
        domainModel.setMyListOfStrings(Arrays.asList(myStrings));
        return domainModel;
    }

}
