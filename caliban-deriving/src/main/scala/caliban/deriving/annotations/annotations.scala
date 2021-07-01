package caliban.deriving.annotations

import scala.annotation.StaticAnnotation

/**
 * Annotation used to indicate that a class member should not be included in the derived Schema
 */
case class GQLExclude() extends StaticAnnotation
