[![Build Status](https://travis-ci.org/RutledgePaulV/q-builders.svg)](https://travis-ci.org/RutledgePaulV/q-builders)

## Overview

A generic abstraction for building queries for arbitrary domain models that minimizes
magic strings, provides type safety, produces queries that read like a sentence,
and provides an extensible way to define new target query formats. All of that together means that 
when you choose to use these as your query builders you only need to map fields once and 
use these builders anywhere. 

Have a REST API? Chances are you want to provide an ability for people to query your API, but you
also make queries against the database yourself. Using these builders you define *one* query model
that you use in both your SDK and API, and target different formats. Like RSQL for over-the-wire
and straight mongo queries on the API side.


## Supported Target Types (OOB):
- RSQL
- Spring Data's MongoDB Criteria

_submit a PR to add more!_


## Why does this exist?
A lot of existing query builders are bad. It's *hard* to write a query builder that always restricts you to the
only logical options available, which has resulted in most query builders being overly generic and allowing you to 
call methods that you shouldn't be able to call at that time. Additionally, sometimes it's not clear when
you've got the final result versus when you can continue to build on it. What happens if you grab the final
result and then make more changes to the builder? Some examples of poor query builders:


*spring data mongodb*:

```java
// notice that there's no type restrictions on the values, and you can pass 
// multiple lists of integers to a string field you'll get an exception at 
// runtime but everything will compile just fine.
Criteria crit = Criteria.where("myStringField")
                .in(Collections.singletonList(1), Collections.singletonList(2));


// you can define the value for a field without even saying what the field is. huh?
// again, you'll get an exception at runtime
crit = new Critera().is("cats");
```


*apache cxf*:
```java
// I think this one speaks for itself
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

// define a query object for each of your domain models
public class PersonQuery extends QBuilder<PersonQuery> {

    public StringProperty<PersonQuery> firstName() {
        return stringField("firstName");
    }
    
    public IntegerProperty<PersonQuery> age() {
        return integerField("age");
    }
    
}


CompleteCondition<PersonQuery> q = new PersonQuery().firstName()
                                       .equalTo("Paul").and().age().equalTo(23);

String rsql = q.query(new RSQLVisitor()); 
// firstName==Paul;age==23

Criteria mongoCriteria = q.query(new MongoCriteriaVisitor()); 
// {firstName: "Paul", age: 23}



// everybody loves static imports, right?
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


String rsql = firstName().lexicallyAfter("Pam").or()
              .and(age().greaterThan(20), age().lessThan(25))
              .query(new RSQLVisitor());
              
// firstName=gt=Pam,(age=gt=20;age=lt=25)
```


Installation (coming soon to a maven repository near you):
```xml
<dependencies>

    <dependency>
        <groupId>com.github.rutledgepaulv</groupId>
        <artifactId>q-builders</artifactId>
        <version>1.0</version>
    </dependency>
    
    <!-- only necessary if you're using the spring data mongodb target type -->
    <dependency>
        <groupId>org.springframework.data</groupId>
        <artifactId>spring-data-mongodb</artifactId>
        <version>1.8.0.RELEASE</version>
    </dependency>

</dependencies>
```