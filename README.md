[![Build Status](https://travis-ci.org/RutledgePaulV/q-builders.svg)](https://travis-ci.org/RutledgePaulV/q-builders)
[![Coverage Status](https://coveralls.io/repos/RutledgePaulV/q-builders/badge.svg?branch=develop&service=github)](https://coveralls.io/github/RutledgePaulV/q-builders?branch=develop)
[![Maven Central](https://maven-badges.herokuapp.com/maven-central/com.github.rutledgepaulv/q-builders/badge.svg)](https://maven-badges.herokuapp.com/maven-central/com.github.rutledgepaulv/q-builders)

### Overview
An abstraction for building queries for arbitrary domain models that minimizes
magic strings, provides type safety, produces queries that read like a sentence,
and provides an extensible way to define new target query formats.

Have a REST API? Chances are you want to provide an ability for people to query your API, but you
also make queries against the database yourself. One option could be to use these builders to define *one* query model
in a shared jar that you use in both your SDK and API and simply target different query formats. Like RSQL for
over-the-wire and mongo on the API server.


### Why does this exist?
A lot of existing query builders are bad (in my opinion). It's difficult to write a convenient query builder in a statically typed language, which has resulted in most query builders giving up on type safety and allowing you to 
call methods that you shouldn't be able to call at that time.


### Why is this better?
It uses the type system to enforce that you can't call an unapplicable method at any point
as you build your queries. Also, since you define the accepted type when you define your query model, there's
no need to worry about someone passing an integer to a string field, etc. It supports both chaining and composition
to build a query and when used with static imports it begins to look like a succinct query DSL.


### Out Of Box Target Query Formats:
- Java Predicates
- A string in RSQL format
- Elasticsearch's QueryBuilder
- Spring Data's MongoDB Criteria
- Yours could be next...


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


Condition<PersonQuery> q = new PersonQuery().firstName().eq("Paul").and().age().eq(23);


String rsql = q.query(new RSQLVisitor()); 
// firstName==Paul;age==23

Criteria mongoCriteria = q.query(new MongoVisitor()); 
// {firstName: "Paul", age: 23}

FilterBuilder filter = q.query(new ElasticsearchVisitor());
// { "and" : { "filters" : [ { "term" : { "firstName" : "Paul" } }, { "term" : { "age" : 23 } } ] } }

Predicate<Person> predicate = q.query(new PredicateVisitor<>());
// List<Person> personsNamedPaulAndAge23 = persons.stream().filter(predicate).collect(toList());
```


### Static Import Usage:
```java

public class PersonQuery extends QBuilder<PersonQuery> {

    public static StringProperty<PersonQuery> firstName() {
        return new PersonQuery().stringField("firstName");
    }
    
    public static IntegerProperty<PersonQuery> age() {
        return new PersonQuery().integerField("age");
    }
    
}

import static com.company.queries.PersonQuery.PersonQueryPredef.*;

Condition<PersonQuery> query = firstName().eq("Paul").or(and(firstName().ne("Richard"), age().gt(22)));
```

### Precedence:
Chaining with the infix forms of ```and()``` and ```or()``` is complicated when you use both and then want to talk about
precedence. The implementation is such that whenever you change from a chain of ```and()``` to
a chain of ```or()``` (or vice versa), then the previous statements are wrapped together and the existing set
becomes one of the elements in the new ```or()``` (or ```and()```) chain. 

In general, to avoid unintended precedence concerns, it's best to limit your chains
to only ```and()``` or ```or()``` operators and use the compositional forms 
```and(Condition<T> c1, Condition<T> c2, Condition<T>... cn)``` and 
```or(Condition<T> c1, Condition<T> c2, Condition<T>... cn)``` for queries that must mix the two.


### RSQL Flavor
The RSQL builder introduces some new operators to the standard RSQL set. Since this library only
builds queries it doesn't dictate what you use to parse them. However, it's written specifically
to be compatible with [rsql-parser](https://github.com/jirutka/rsql-parser). So, you should
make sure that you add the following operators before parsing:

- "=ex=" The exists clause. It has values of either ```true``` or ```false```.
- "=q=" The subquery clause. It has a string value that itself might be an entire RSQL query.
- "=re=" A regular expression as a string. The string will be passed as-is to the backend visitor you use, so the regex string must be in the same flavor as the visitor you intend to use (mongo regex vs java regex, etc)



### Installation 


#### Release Versions
```xml
<dependencies>
    <dependency>
        <groupId>com.github.rutledgepaulv</groupId>
        <artifactId>q-builders</artifactId>
        <version>1.5</version>
    </dependency>
</dependencies>
```

#### Snapshot Versions
```xml
<dependencies>
    <dependency>
        <groupId>com.github.rutledgepaulv</groupId>
        <artifactId>q-builders</artifactId>
        <version>1.6-SNAPSHOT</version>
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
        <version>1.9.2.RELEASE</version>
    </dependency>
    
    <!-- only necessary if you're using the elasticsearch filter builder target type -->
    <dependency>
        <groupId>org.elasticsearch</groupId>
        <artifactId>elasticsearch</artifactId>
        <version>2.3.4</version>
    </dependency>
            
</dependencies>
```


### License

This project is licensed under [MIT license](http://opensource.org/licenses/MIT).
