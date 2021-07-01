package caliban

import caliban.schema.Schema

import scala.language.experimental.macros

package object deriving {
  def deriveSchemaInstance[R, T]: Schema[R, T] = macro DerivationMacros.deriveSchema[R, T]
}
