package caliban.deriving

import caliban.GraphQL.graphQL
import caliban.schema.Annotations.GQLDescription
import caliban.schema.Schema
import caliban.{GraphQL, RootResolver}
import zio.test._
import zio.test.environment._

object Scala3EnumDerivationSpec extends DefaultRunnableSpec {
  enum ExampleSum {
    case A
    @GQLDescription("the B") case B
  }

  implicit lazy val exampleSumSchema: Schema[Any, ExampleSum] = deriveSchemaInstance[Any, ExampleSum]

  lazy val exampleValue: ExampleSum = ExampleSum.A
  lazy val api: GraphQL[Any]        = graphQL(RootResolver(exampleValue))

  val expectedSchema: String =
    """schema {
      |  query: ExampleSum
      |}
      |
      |enum ExampleSum {
      |  A
      |  "the B"
      |B
      |}""".stripMargin

  override def spec: ZSpec[TestEnvironment, Any] =
    suite("Caliban Derivation")(
      suite("Scala 3 enum without common fields")(
        test("schema rendered as expected") {
          val rendered = api.render

          assertTrue(rendered == expectedSchema)
        }
      )
    )
}
