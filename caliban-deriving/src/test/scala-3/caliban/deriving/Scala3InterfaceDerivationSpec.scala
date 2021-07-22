package caliban.deriving

import caliban.GraphQL.graphQL
import caliban.schema.Annotations.GQLDescription
import caliban.schema.Schema
import caliban.{GraphQL, RootResolver}
import zio.test._
import zio.test.environment._
import caliban.deriving.EnumDerivationSpec.ExampleSum

object Scala3InterfaceDerivationSpec extends DefaultRunnableSpec {
  enum ExampleSum(
    val x: Int,
    @GQLDescription("the y field") val y: String
  ) {

    case A(z: Option[Int]) extends ExampleSum(z.getOrElse(1), "A")

    @GQLDescription("the B") case B(override val x: Int, override val y: String) extends ExampleSum(x, y)
  }

  implicit lazy val exampleSumSchema: Schema[Any, ExampleSum] = deriveSchemaInstance[Any, ExampleSum]

  lazy val exampleValue: ExampleSum = ExampleSum.A(Some(10))
  lazy val api: GraphQL[Any]        = graphQL(RootResolver(exampleValue))

  val expectedSchema: String =
    """schema {
      |  query: ExampleSum
      |}
      |
      |interface ExampleSum {
      |  x: Int!
      |  "the y field"
      |  y: String!
      |}
      |
      |type A implements ExampleSum {
      |  z: Int
      |  x: Int!
      |  y: String!
      |}
      |
      |"the B"
      |type B implements ExampleSum {
      |  x: Int!
      |  y: String!
      |}""".stripMargin

  override def spec: ZSpec[TestEnvironment, Any] =
    suite("Caliban Derivation")(
      suite("Sum type with common fields")(
        test("schema rendered as expected") {
          val rendered = api.render

          assertTrue(rendered == expectedSchema)
        }
      )
    )
}
