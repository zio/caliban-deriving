package caliban.deriving

import caliban.GraphQL.graphQL
import caliban.deriving.annotations.GQLExclude
import caliban.schema.Annotations.{GQLDeprecated, GQLDescription, GQLName}
import caliban.schema.{GenericSchema, Schema}
import caliban.{GraphQL, RootResolver}
import zio._
import zio.query.ZQuery
import zio.random.Random
import zio.test._
import zio.test.environment._

object ProductDerivationSpec extends DefaultRunnableSpec {
  @GQLName("EP")
  case class ExampleProduct(
    name: String,
    @GQLDescription("A list of the character's nicknames") nicknames: List[String]
  ) {
    @GQLDescription("Foo")
    def foo(): Int = 5

    @GQLDeprecated("Don't use Bar.")
    def bar: Int = 42

    def query: ZQuery[Any, Nothing, String] =
      ZQuery.succeed(nicknames.mkString(", "))

    def op(pattern: String, limit: Int): List[String] =
      nicknames.filter(_.matches(pattern)).take(limit)

    @GQLDescription("Randomly picks one of the nicknames")
    def randomNickname: ZIO[Random, Nothing, Option[String]] =
      if (nicknames.nonEmpty) {
        random.nextIntBounded(nicknames.size).map { idx =>
          Some(nicknames(idx))
        }
      } else ZIO.none

    // This field will not be exposed via GraphQL
    @GQLExclude
    def internal(): Unit = xyz

    // Private fields are also not exposed via GraphQL
    private val xyz: Unit = ()
  }

  case class ExampleProduct2() {
    def something(p: ExampleProduct): Long = p.bar.toLong
  }

  object ExampleProduct extends GenericSchema[Random] {
    implicit lazy val exampleProductSchema: Schema[Random, ExampleProduct] =
      deriveSchemaInstance[Random, ExampleProduct]

    val exampleValue: ExampleProduct = ExampleProduct(
      "hello",
      List("a", "b")
    )
    lazy val api: GraphQL[Random]    = graphQL(RootResolver(exampleValue))

    val expectedSchema: String =
      """schema {
        |  query: EP
        |}
        |
        |type EP {
        |  name: String!
        |  "A list of the character's nicknames"
        |  nicknames: [String!]!
        |  "Foo"
        |  foo: Int!
        |  bar: Int! @deprecated(reason: "Don't use Bar.")
        |  query: String!
        |  op(pattern: String!, limit: Int!): [String!]!
        |  "Randomly picks one of the nicknames"
        |  randomNickname: String
        |}""".stripMargin

    implicit lazy val exampleProduct2Schema: Schema[Random, ExampleProduct2] =
      deriveSchemaInstance[Random, ExampleProduct2]

    val exampleValue2: ExampleProduct2 = ExampleProduct2()
    lazy val api2: GraphQL[Random]     = graphQL(RootResolver(exampleValue2))

    val expectedSchema2: String =
      """schema {
        |  query: ExampleProduct2
        |}
        |
        |scalar Long
        |
        |input ExampleProductInput {
        |  name: String!
        |  "A list of the character's nicknames"
        |  nicknames: [String!]!
        |}
        |
        |type ExampleProduct2 {
        |  something(p: ExampleProductInput!): Long!
        |}""".stripMargin
  }

  override def spec: ZSpec[TestEnvironment, Any] =
    suite("Caliban Derivation")(
      suite("Product type")(
        test("schema rendered as expected") {
          val rendered = ExampleProduct.api.render

          assertTrue(rendered == ExampleProduct.expectedSchema)
        },
        test("schema rendered as expected when used as input") {
          val rendered = ExampleProduct.api2.render

          assertTrue(rendered == ExampleProduct.expectedSchema2)
        }
      )
    )
}
