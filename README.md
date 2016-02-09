[![Build Status](https://travis-ci.org/RutledgePaulV/q-builders.svg)](https://travis-ci.org/RutledgePaulV/q-builders)
[![Coverage Status](https://coveralls.io/repos/RutledgePaulV/q-builders/badge.svg?branch=master&service=github)](https://coveralls.io/github/RutledgePaulV/q-builders?branch=master)
[![Maven Central](https://maven-badges.herokuapp.com/maven-central/com.github.rutledgepaulv/q-builders/badge.svg)](https://maven-badges.herokuapp.com/maven-central/com.github.rutledgepaulv/q-builders)

### Overview
A generic abstraction for building queries for arbitrary domain models that minimizes
magic strings, provides type safety, produces queries that read like a sentence,
and provides an extensible way to define new target query formats.

Have a REST API? Chances are you want to provide an ability for people to query your API, but you
also make queries against the database yourself. Using these builders you define *one* query model
in a shared jar that you use in both your SDK and API, and target different formats. Like RSQL for
over-the-wire and mongo or hibernate queries on the API side.


### Why does this exist?
A lot of existing query builders are bad. It's *hard* to write a query builder that always restricts you to the
only logical options available, which has resulted in most query builders being overly generic and allowing you to 
call methods that you shouldn't be able to call at that time.


# Meet q-builders
As soon as you start typing, you'll notice that you're never even given an inapplicable option at any point
as you build your queries. Also, since you define the accepted type when you define your query model, there's
no need to worry about someone passing an integer to a string field, etc.


### Supported Target Query Expressions:
- Java Predicates
- A string in RSQL format
- Elasticsearch's QueryBuilder
- Spring Data's MongoDB Criteria


### Simple Usage:
```java
//--------------------------------------------------------------------------
// define a single query model for each of the models you want to be able to 
// query against. Return the appropriate property interface for each field
// based on the type of the value on your model
//--------------------------------------------------------------------------

public class PersonQuery extends QBuilder<PersonQuery> {

    public StringProperty<PersonQuery> firstName() {
        return stringField("firstName");
    }
    
    public IntegerProperty<PersonQuery> age() {
        return integerField("age");
    }
    
}


//--------------------------------------------------------------------------
// write your queries using a straightforward and intuitive syntax that 
// enforces type safety
//--------------------------------------------------------------------------

Condition<PersonQuery> q = new PersonQuery().firstName().eq("Paul").and().age().eq(23);


//--------------------------------------------------------------------------
// build the abstract representation of the query into the format of your 
// choice. currently supports rsql, mongo, and elasticsearch out of box
//--------------------------------------------------------------------------

String rsql = q.query(new BasicRsqlVisitor()); 
// firstName==Paul;age==23

Criteria mongoCriteria = q.query(new BasicMongoVisitor()); 
// {firstName: "Paul", age: 23}

FilterBuilder filter = q.query(new BasicEsVisitor());
// { "and" : { "filters" : [ { "term" : { "firstName" : "Paul" } }, { "term" : { "age" : 23 } } ] } }
```

### Predicate Usage:
```java

//--------------------------------------------------------------------------
// feeling bold? integration tests too much setup? you can build into java 
// predicates too, so you can test your queries using an in-memory list or the
// like (not recommended for production code use)
//--------------------------------------------------------------------------

List<Person> persons = getPersons();
Predicate<Person> predicate = q.query(new BasicPredicateVisitor<>());
List<Person> personsNamedPaulAndAge23 = persons.stream().filter(predicate).collect(toList());

```


### Static Import Usage:
```java

//--------------------------------------------------------------------------
// prefer the look and feel of static imports? me too.
//--------------------------------------------------------------------------

public class PersonQuery extends QBuilder<PersonQuery> {

    public static class PersonQueryPredef {
        public static StringProperty<PersonQuery> firstName() {
            return new PersonQuery().firstName();
        }
        public static IntegerProperty<PersonQuery> age() {
            return new PersonQuery().age();
        }
    }
    
    public StringProperty<PersonQuery> firstName() {
        return stringField("firstName");
    }
    
    public IntegerProperty<PersonQuery> age() {
        return integerField("age");
    }
    
}

import static com.company.queries.PersonQuery.PersonQueryPredef.*;

Condition<PersonQuery> query = firstName().eq("Paul").or()
                               .and(firstName().lexicallyBefore("Richard"), age().gt(22));
```

### Precedence:
Chaining both with ```and()``` and ```or()``` is complicated when you begin to talk about
precedence. The implementation is such that whenever you change from a chain of ```and()``` to
a chain of ```or()```, then the previous statements are wrapped together and the existing set
becomes one of the elements in the new ```or()``` chain, and vice versa for a chain of ```or()``` followed
by a chain of ```and()```. In general, to avoid unintended precedence concerns, it's best to limit your chains
to only ```and()``` or ```or()``` operators and use the parameterized 
```and(Condition<T> c1, Condition<T> c2, Condition<T>... cn)``` and 
```or(Condition<T> c1, Condition<T> c2, Condition<T>... cn)``` for more complicated queries.


### Customizations:
It's great to have a single syntax to define queries that can be used against multiple
backends, but what about features that are specific to certain things? How would I add
(insert thing here) query support for mongo? I'll provide an example of adding regex 
searches for mongo (something I chose to leave out of the core library due to the complexity of
supporting same-interface regex searches on each backend).


1) Define a custom property interface

```java

public interface AdvancedStringField<T extends Partial<T>> extends StringProperty<T> {

    Condition<T> regex(String pattern);

}
```

2) Define an implementation for that interface using a new operator
```java

public class AdvancedStringFieldDelegate<T extends QBuilder<T>> extends StringPropertyDelegate<T>
        implements AdvancedStringField<T> {

    public static final ComparisonOperator REGEX = new ComparisonOperator("REGEX");

    public AdvancedStringFieldDelegate(String field, T canonical) {
        super(field, canonical);
    }

    @Override
    public final Condition<T> regex(String pattern) {
        return condition(getField(), REGEX, Collections.singletonList(pattern));
    }

}
```

3) Define a custom 'visitor' (extending the provided one) with your functionality
```java

public class AdvancedMongoVisitor extends MongoVisitor {

    @Override
    protected Criteria visit(ComparisonNode node) {

        ComparisonOperator operator = node.getOperator();

        if(operator.equals(AdvancedStringFieldDelegate.REGEX)) {
            return Criteria.where(node.getField()).regex((String) node.getValues().iterator().next());
        } else {
            return super.visit(node);
        }

    }

}
```

4) Use the custom property in your query builders
```java

public class AdvancedPersonQuery extends QBuilder<AdvancedPersonQuery> {
    
    public AdvancedStringField<AdvancedQModel> firstName() {
        return prop(getCurrentMethodName(), AdvancedStringFieldDelegate.class, AdvancedStringField.class);
    }
    
}
```

5) Use your custom visitor when building queries that might contain the new property type.
```java

Condition<AdvancedQModel> q = firstName().regex("^pau.*");
Criteria criteria = q.query(new AdvancedMongoVisitor());

```

### Installation 
(coming soon to a maven repository near you)


#### Release
```xml
<dependencies>
    <dependency>
        <groupId>com.github.rutledgepaulv</groupId>
        <artifactId>q-builders</artifactId>
        <version>1.0</version>
    </dependency>
</dependencies>
```

#### Snapshot
```xml
<dependencies>
    <dependency>
        <groupId>com.github.rutledgepaulv</groupId>
        <artifactId>q-builders</artifactId>
        <version>2.0-SNAPSHOT</version>
    </dependency>
</dependencies>


<repositories>
    <repository>
        <id>ossrh</id>
        <name>Repository for snapshots</name>
        <url>https://oss.sonatype.org/content/repositories/snapshots</url>
        <snapshots>
            <enabled>true</enabled>
        </snapshots>
    </repository>
</repositories>
```



#### Optional dependencies
```xml
<dependencies>
    
    <!-- only necessary if you're using the spring data mongodb criteria target type -->
    <dependency>
        <groupId>org.springframework.data</groupId>
        <artifactId>spring-data-mongodb</artifactId>
        <version>1.8.2.RELEASE</version>
    </dependency>
    
    <!-- only necessary if you're using the elasticsearch filter builder target type -->
    <dependency>
        <groupId>org.elasticsearch</groupId>
        <artifactId>elasticsearch</artifactId>
        <version>2.2.0</version>
    </dependency>

    <!-- only necessary if you're using the java.util.function.Predicate target type -->
     <dependency>
        <groupId>com.fasterxml.jackson.core</groupId>
        <artifactId>jackson-databind</artifactId>
        <version>2.7.1</version>
     </dependency>
            
</dependencies>
```


### License

This project is licensed under [MIT license](http://opensource.org/licenses/MIT).
