---
id: index
title: "Introduction to Caliban Deriving"
sidebar_label: "Caliban Deriving"
---

Full-featured, robust deriving for Caliban.

The Caliban Deriving library provides an advanced derive macro for the [Caliban library](https://github.com/ghostdogpr/caliban) that seamlessly integrates with the built-in schema derivation system and allows the library users to not only derive the schema for the user defined case classes and enums but also for calculated fields and functions with parameters.

## Getting started

Start by adding `caliban-deriving` as a dependency to your project:
  
```scala
libraryDependencies += "dev.zio" %% "caliban-deriving" % "@VERSION@"
```

Once the library is added, Caliban's default auto-derived schemas can be replaced one by one for each type by explicitly deriving its
schema using the Caliban Deriving method:

```scala
case class Example(value: Int)
object Example {
    implicit lazy val exampleSchema: Schema[Any, Example] =
      deriveSchemaInstance[Any, Example]
}
```

Please read [Caliban's official documentation about schemas](https://ghostdogpr.github.io/caliban/docs/schema.html#schemas) to learn more about what a schema is and what features the core library provides.

It is important that it is possible to use a _mix_ of Caliban's built-in schemas, auto-derived schemas and the ones provided by `deriveSchemaInstance`. Both derive methods are looking for implicit instances of `Schema`.

## Features

Caliban Deriving's `deriveSchemaInstance` function can be applied on the following Scala data types:

- _case classes_
- _sealed traits_  or Scala 3 _enums_
- any _trait_

### Common features
The following rules apply equally for case classes, sealed traits and traits.

- public `val` fields and parameterless `def` methods are generated as GraphQL _fields_. Their `Schema` is found by an implicit search for the given type. This way it supports exactly the same cases as Caliban's built-in derive method, including fields of type `ZIO`, `ZQuery` and `Future`.
- public methods with one or more parameters are generated as GraphQL _functions_. The `Schema` of the function arguments is found by an implicit search for their type.
- protected and private fields/methods are excluded
- public members annotated by `@GQLExclude` are also excluded
- the following core [Caliban annotations](https://ghostdogpr.github.io/caliban/docs/schema.html#annotations) are supported: `@GQLName`, `@GQLDescription`, `@GQLDeprecated` and `@GQLDirective`

### Case classes
From _case classes_ a GraphQL `type` is generated. If it is used as an input (as a parameter for a function in another type), then an GraphQL `input` is generated which only contains the constructor parameters of the case class, not the other members.

### Sealed traits
For _sum types_ (sealed trait with a set of case class / case objects implementations) if the base trait contains any methods, it will become a GraphQL `interface` and each constuctor a `type`.

If the base trait has no members, but the constructors have parameters, it becomes a GraphQL `union`. Members of constructors are still handled the same way as mentioned above.

If all the constructors are case objects then it becomes a GraphQL `enum`.

### Traits
 If the derivation is invoked on a simple trait (not `sealed`) then it works as if it were a concrete parameterless _case class_. This means a GraphQL `type` is generated and a schema derived for an arbitrary trait can be used to serve any implementation of it.

## Using with effects
When there are members of type `ZIO` or `ZQuery`, they can have an environment parameter (`-R`) that is not `Any`. Effects and queries without environment just work.

When working with effects requiring an environment the following rules apply:

- You have to have the `GenericSchema[R]` trait in scope as described [in the official documentation](https://ghostdogpr.github.io/caliban/docs/schema.html#effects). This should be done by inheriting this trait in an object and putting all the additional derived implicits there.
- The implicit resolution in this case will only be correct if you put all the implicit instances derived with Caliban Deriving's `deriveSchemaInstance` **directly into the object** extending `GenericSchema`.
- Although the macro can calculate the union of all the required environments used in a type, it is not automatically using it for the result `Schema`. The reason is that you may want to derive schema for multiple data types, each using only a subset of the total environment passed to the `GenericSchema`. In this case the full environment cannot be calculated inside the derivation. For this reason, you **must** provide the full required environment, the same that is passed to the `GenericSchema` in scope, to the `deriveSchemaInstance` method in its first type parameter.

### Mutations and subscriptions
Not supported yet.