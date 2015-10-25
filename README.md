[![Build Status](https://travis-ci.org/RutledgePaulV/q-builders.svg)](https://travis-ci.org/RutledgePaulV/q-builders)
[![Coverage Status](https://coveralls.io/repos/RutledgePaulV/q-builders/badge.svg?branch=master&service=github)](https://coveralls.io/github/RutledgePaulV/q-builders?branch=master)

### Overview
A generic abstraction for building queries for arbitrary domain models that minimizes
magic strings, provides type safety, produces queries that read like a sentence,
and provides an extensible way to define new target query formats. All of that together means that 
when you choose to use these as your query builders you only need to map fields once and 
use these builders anywhere. 

Have a REST API? Chances are you want to provide an ability for people to query your API, but you
also make queries against the database yourself. Using these builders you define *one* query model
that you use in both your SDK and API, and target different formats. Like RSQL for over-the-wire
and straight mongo queries on the API side.


### Why does this exist?
A lot of existing query builders are bad. It's *hard* to write a query builder that always restricts you to the
only logical options available, which has resulted in most query builders being overly generic and allowing you to 
call methods that you shouldn't be able to call at that time. Additionally, sometimes it's not clear when
you've got the final result versus when you can continue to build on it. What happens if you grab the final
result and then make more changes to the builder? Some examples of the things I take issue with in other builders:


*spring data mongodb*:

```java

Criteria crit = Criteria.where("myStringField")
                .in(Collections.singletonList(1), Collections.singletonList(2));
                
crit = new Critera().is("cats");
```

*apache cxf*:
```java

FiqlSearchConditionBuilder builder = new FiqlSearchConditionBuilder();
builder.is("cats").notAfter(new Date()).or().is("cats").equalTo(3, 3, 3, 3, 4).query();
builder.and(new ArrayList<>()).wrap().wrap().wrap().and().is("cats");
String finalQuery = builder.query();
```


# Meet q-builders

If you use intellisense, you'll notice that you're never even given an inapplicable option at any point
as you build your queries. Also, since you define the type when you define your query model, everything
is type safe. No need to worry about someone passing an integer to a string field, etc.

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

Condition<PersonQuery> q = new PersonQuery().firstName().eq("Paul")
                                                    .and().age().eq(23);



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



//--------------------------------------------------------------------------
// feeling bold? integration tests too much setup? you can build into java 
// predicates too, so you can test your queries using an in-memory list or the
// like (not recommended for production code use)
//--------------------------------------------------------------------------

List<Person> persons = getPersons();
Predicate<Person> predicate = q.query(new BasicPredicateVisitor());
List<Person> personsNamedPaulAndAge23 = persons.stream().filter(predicate).collect(Collectors.toList());


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
                               .and(firstName().lexicallyBefore("Richard").and().age().gt(22));




```




### Supported Target Types:
- [RSQL - (a text query syntax)](https://github.com/jirutka/rsql-parser)
- Elasticsearch FilterBuilder
- Spring Data MongoDB Criteria
- Java function predicate (for in-memory filtering of collections, etc)

_submit a PR to add more!_


### Installation 
(coming soon to a maven repository near you)

```xml
<dependencies>

    <dependency>
        <groupId>com.github.rutledgepaulv</groupId>
        <artifactId>q-builders</artifactId>
        <version>1.0</version>
    </dependency>
    
    <!-- only necessary if you're using the spring data mongodb criteria target type -->
    <dependency>
        <groupId>org.springframework.data</groupId>
        <artifactId>spring-data-mongodb</artifactId>
        <version>1.8.0.RELEASE</version>
    </dependency>
    
    <!-- only necessary if you're using the elasticsearch filter builder target type -->
    <dependency>
        <groupId>org.elasticsearch</groupId>
        <artifactId>elasticsearch</artifactId>
        <version>1.7.2</version>
    </dependency>

    <!-- only necessary if you're using the java.util.function.Predicate target type -->
     <dependency>
        <groupId>com.fasterxml.jackson.core</groupId>
        <artifactId>jackson-databind</artifactId>
        <version>2.6.1</version>
     </dependency>
            
</dependencies>
```


### License

This project is licensed under [MIT license](http://opensource.org/licenses/MIT).