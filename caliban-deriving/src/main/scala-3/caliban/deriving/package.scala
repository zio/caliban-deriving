package caliban.deriving

import caliban.parsing.adt.Directive
import caliban.schema.Schema
import caliban.schema.Annotations.GQLName
import caliban.schema.Annotations.GQLDescription
import caliban.schema.Annotations.GQLInputName
import caliban.schema.Annotations.GQLDeprecated
import caliban.schema.Annotations.GQLDirective
import scala.quoted.*
import caliban.schema.Types
import caliban.deriving.annotations.GQLExclude
import caliban.schema.Step.ObjectStep
import caliban.schema.Step
import caliban.schema.Step.FunctionStep
import caliban.InputValue
import caliban.schema.ArgBuilder
import caliban.CalibanError.ExecutionError
import caliban.schema.Step.QueryStep
import zio.query.ZQuery
import caliban.wrappers.ApolloTracing.Execution
import java.text.DateFormat.Field

inline def deriveSchemaInstance[R, T]: Schema[R, T]                                      =
  ${ deriveSchemaInstanceImpl[R, T] }

private def deriveSchemaInstanceImpl[R: Type, T: Type](using Quotes): Expr[Schema[R, T]] = {
  import quotes.reflect.*

  case class GraphQLInfo(
    name: Expr[String],
    inputName: Expr[String],
    description: Expr[Option[String]],
    deprecationReason: Expr[Option[String]],
    directives: Expr[List[caliban.parsing.adt.Directive]]
  ) {
    def isDeprecated: Expr[Boolean]                                      = '{ ${ deprecationReason }.isDefined }
    def directivesOpt: Expr[Option[List[caliban.parsing.adt.Directive]]] = '{ Some($directives).filterNot(_.isEmpty) }
  }

  case class Field(field: Symbol, constructorParameter: Option[Symbol], typ: TypeRepr)

  def extractAnnotation[Annotation: Type](symbol: Symbol, altSymbol: Option[Symbol]): Option[Term] =
    symbol
      .getAnnotation(TypeRepr.of[Annotation].typeSymbol)
      .orElse(altSymbol.flatMap(_.getAnnotation(TypeRepr.of[Annotation].typeSymbol)))

  def extractStringAnnotation[Annotation: Type](symbol: Symbol, altSymbol: Option[Symbol]): Option[Expr[String]] =
    extractAnnotation[Annotation](symbol, altSymbol).flatMap {
      case Apply(_, List(Literal(StringConstant(name)))) => Some(Expr(name))
      case _                                             => None
    }

  def extractInfo(symbol: Symbol, altSymbol: Option[Symbol]): GraphQLInfo = {
    val nameAnnotation                             = extractStringAnnotation[GQLName](symbol, altSymbol)
    val inputNameAnnotation                        = extractStringAnnotation[GQLInputName](symbol, altSymbol)
    val descriptionAnnotation                      = extractStringAnnotation[GQLDescription](symbol, altSymbol)
    val deprecatedAnnotation                       = extractStringAnnotation[GQLDeprecated](symbol, altSymbol)
    val directiveAnnotations: Seq[Expr[Directive]] = symbol.annotations.filter { annotation =>
      annotation.tpe =:= TypeRepr.of[GQLDirective]
    }.flatMap {
      case Apply(_, List(directive)) => Some(directive)
      case _                         => None
    }
      .map(_.asExprOf[Directive])

    val name                                    = nameAnnotation.getOrElse(Expr(symbol.name)) // TODO: append type parameter names
    val inputName                               = inputNameAnnotation.getOrElse('{ caliban.schema.Schema.customizeInputTypeName($name) })
    val description: Expr[Option[String]]       = descriptionAnnotation match {
      case Some(d) => '{ Some($d) }
      case None    => '{ None }
    }
    val deprecationReason: Expr[Option[String]] = deprecatedAnnotation match {
      case Some(d) => '{ Some($d) }
      case None    => '{ None }
    }
    val directives                              = '{ List(${ Varargs(directiveAnnotations) }: _*) }

    GraphQLInfo(
      name,
      inputName,
      description,
      deprecationReason,
      directives
    )
  }

  def isExcluded(symbol: Symbol): Boolean =
    symbol
      .getAnnotation(TypeRepr.of[GQLExclude].typeSymbol)
      .isDefined

  def makeCalibanType(schema: Expr[Schema[_, _]], isInput: Boolean): Expr[() => caliban.introspection.adt.__Type] =
    '{ () =>
      if (${ schema }.optional) {
        ${ schema }.toType_(${ Expr(isInput) })
      } else {
        Types.makeNonNull($schema.toType_(${ Expr(isInput) }))
      }
    }

  def summonSchema(envType: TypeRepr, fieldType: TypeRepr) =
    (envType.asType match {
      case '[e] =>
        fieldType.asType match {
          case '[f] => Expr.summon[Schema[e, f]]
        }
    }).getOrElse {
      report.throwError(s"Cannot find an instance of Schema for $fieldType")
    }

  def summonArgBuilder(fieldType: TypeRepr) =
    (fieldType.asType match {
      case '[f] => Expr.summon[ArgBuilder[f]]
    }).getOrElse {
      report.throwError(s"Cannot find an instance of Schema for $fieldType")
    }

  def deriveParam(envType: TypeRepr, field: Field): Expr[caliban.introspection.adt.__InputValue] = {
    val info   = extractInfo(field.field, field.constructorParameter)
    val schema = summonSchema(envType, field.typ)

    '{
      caliban.introspection.adt.__InputValue(
        ${ Expr(field.field.name) },
        ${ info.description },
        ${ makeCalibanType(schema, isInput = true) },
        None, // TODO
        ${ info.directivesOpt }
      )
    }
  }

  def getReturnType(fieldType: TypeRepr) =
    (fieldType match {
      case MethodType(_, _, returnType) =>
        returnType
      case _                            =>
        fieldType
    }).widen

  def deriveField(envType: TypeRepr, field: Field): Expr[caliban.introspection.adt.__Field] = {
    val info       = extractInfo(field.field, field.constructorParameter)
    val returnType = getReturnType(field.typ)
    val schema     = summonSchema(envType, returnType)

    val firstParamList = field.field.paramSymss.headOption // NOTE: multiple parameter lists are not supported
    val args           =
      firstParamList.filterNot(_.isEmpty) match {
        case Some(params) =>
          // Non-empty list of parameters
          params.map { param =>
            val paramType = Ref(param).tpe.widen
            deriveParam(envType, Field(param, None, paramType))
          }
        case None         =>
          // Parameterless or empty parameter list
          Nil
      }

    '{
      caliban.introspection.adt.__Field(
        ${ Expr(field.field.name) },
        ${ info.description },
        List(${ Varargs(args) }: _*),
        ${ makeCalibanType(schema, isInput = false) },
        ${ info.isDeprecated },
        ${ info.deprecationReason },
        ${ info.directivesOpt }
      )
    }
  }

  def deriveStep(resolveValue: Expr[T], envType: TypeRepr, field: Field): Expr[Step[R]] = {

    val info           = extractInfo(field.field, field.constructorParameter)
    val returnType     = getReturnType(field.typ)
    val schema         = summonSchema(envType, returnType)
    val firstParamList = field.field.paramSymss.headOption // NOTE: multiple parameter lists are not supported

    returnType.asType match {
      case '[t] =>
        firstParamList.filterNot(_.isEmpty) match {
          case Some(params) =>
            // Non-empty list of parameters

            def buildArgs(args: Expr[Map[String, InputValue]])(using Quotes): Expr[Either[ExecutionError, t]] = {
              val terms = params.map { param =>
                val paramType  = param.tree.asInstanceOf[ValDef].tpt.tpe
                val argBuilder = summonArgBuilder(paramType)
                '{ ${ argBuilder }.build(${ args }(${ Expr(param.name) })) }.asTerm
              }

              def unwrap(paramRefs: List[Term], remaining: List[(Symbol, Term)])(using
                Quotes
              ): Expr[Either[ExecutionError, t]] =
                remaining match {
                  case Nil                         =>
                    val call = Apply(Select(resolveValue.asTerm, field.field), paramRefs.reverse).asExprOf[t]
                    '{ Right[ExecutionError, t]($call) }.asExprOf[Either[ExecutionError, t]]
                  case (headSym, headTerm) :: tail =>
                    val headType = headSym.tree.asInstanceOf[ValDef].tpt.tpe
                    headType.asType match {
                      case '[ht] =>
                        val anonFun = Symbol.newMethod(
                          Symbol.spliceOwner,
                          "anonFun",
                          MethodType(List(headSym.name))(
                            (_: MethodType) => List(headType),
                            (_: MethodType) => TypeRepr.of[Either[ExecutionError, t]]
                          )
                        )

                        val term = Select
                          .unique(headTerm, "flatMap")
                          .appliedToTypes(List(TypeRepr.of[ExecutionError], returnType))
                          .appliedTo(
                            Block(
                              List(
                                DefDef(
                                  anonFun,
                                  { case List(List(paramTerm: Term)) =>
                                    Some(
                                      unwrap(paramTerm.asExprOf[ht].asTerm :: paramRefs, tail).asTerm
                                        .changeOwner(anonFun)
                                    )
                                  }
                                )
                              ),
                              Closure(Ref(anonFun), None)
                            )
                          )
                          .asExprOf[Either[ExecutionError, t]]
                        term
                    }
                }

              unwrap(Nil, params zip terms)
            }

            '{
              FunctionStep { args =>
                ${ buildArgs('args) } match {
                  case Left(error)  => QueryStep(ZQuery.fail(error))
                  case Right(value) => ${ schema.asExprOf[Schema[R, t]] }.resolve(value)
                }
              }
            }
          case None         =>
            val invoke =
              if (firstParamList == Some(Nil)) Apply(Select(resolveValue.asTerm, field.field), List()).asExprOf[t]
              else {
                Select(resolveValue.asTerm, field.field).asExprOf[t]
              }

            '{ ${ schema.asExprOf[Schema[R, t]] }.resolve(${ invoke }) }
        }
    }
  }

  def deriveStepWithName(resolveValue: Expr[T], envType: TypeRepr, field: Field): Expr[(String, Step[R])] = {
    val fieldName = field.field.name
    val step      = deriveStep(resolveValue, envType, field)

    '{
      (
        ${ Expr(fieldName) },
        $step
      )
    }
  }

  def deriveInput(envType: TypeRepr, info: GraphQLInfo, fields: List[Field]): Expr[caliban.introspection.adt.__Type] = {
    val fieldExprs: List[Expr[caliban.introspection.adt.__InputValue]] =
      fields.map { case field =>
        deriveParam(envType, field)
      }

    '{
      caliban.schema.Types.makeInputObject(
        Some(${ info.inputName }),
        ${ info.description },
        List(${ Varargs(fieldExprs) }: _*)
      )
    }
  }

  def deriveObject(
    envType: TypeRepr,
    info: GraphQLInfo,
    fields: List[Field]
  ): Expr[caliban.introspection.adt.__Type] = {
    val fieldExprs: List[Expr[caliban.introspection.adt.__Field]] =
      fields.map { field =>
        deriveField(envType, field)
      }

    '{
      caliban.schema.Types.makeObject(
        Some(${ info.name }),
        ${ info.description },
        List(${ Varargs(fieldExprs) }: _*),
        ${ info.directives }
      )
    }
  }

  def enrichWithConstructorField(product: Symbol, field: Field): Field =
    field.copy(constructorParameter = product.primaryConstructor.paramSymss.flatten.find { param =>
      param.name == field.field.name
    })

  def getInputFields(targetSym: Symbol, targetType: TypeRepr): List[Field] =
    targetSym.caseFields
      .filterNot(isExcluded)
      .map(field => Field(field, None, targetType.memberType(field)))
      .map(field => enrichWithConstructorField(targetSym, field))

  def getAllFields(targetSym: Symbol, targetType: TypeRepr): List[Field] = {
    val ordering          = targetSym.declarations.zipWithIndex.toMap
    val supertypeOrdering =
      (targetSym.memberFields ++ targetSym.memberMethods)
        .filterNot(ordering.contains)
        .groupBy(_.owner)
        .flatMap { case (owner, _) =>
          owner.declarations.zipWithIndex.map { case (k, idx) => k -> (idx + 1000) }
        }
        .toMap

    (targetSym.memberFields ++ targetSym.memberMethods).filter { field =>
      !field.flags.is(Flags.Artifact) &&
      !field.flags.is(Flags.Synthetic) &&
      !field.flags.is(Flags.Protected) &&
      !field.flags.is(Flags.Private)
    }
      .filterNot(isExcluded)
      .filterNot { member =>
        member.owner.fullName == "java.lang.Object" ||
        member.owner.fullName == "scala.Any" ||
        member.owner.fullName == "scala.Product" ||
        member.owner.fullName == "scala.reflect.Enum" ||
        member.owner.fullName == "scala.Equals" ||
        member.owner.fullName == "scala.deriving.Mirror$.Singleton"
      }
      .sortBy { member =>
        ordering
          .get(member)
          .orElse(supertypeOrdering.get(member))
          .getOrElse(Int.MaxValue)
      }
      .map(field => Field(field, None, targetType.memberType(field)))
      .map(field => enrichWithConstructorField(targetSym, field))
  }

  def deriveProduct(envType: TypeRepr, targetSym: Symbol, targetType: TypeRepr): Expr[Schema[R, T]] = {
    val inputFields = getInputFields(targetSym, targetType)
    val allFields   = getAllFields(targetSym, targetType)
    val info        = extractInfo(targetSym, None)

    '{
      new Schema[R, T] {
        def resolve(value: T): caliban.schema.Step[R]                                                           =
          ObjectStep.apply[R](
            ${ info.name },
            List(${ Varargs(allFields.map(field => deriveStepWithName('value, envType, field))) }: _*).toMap
          )

        protected[this] def toType(isInput: Boolean, isSubscription: Boolean): caliban.introspection.adt.__Type =
          if (isInput) {
            ${ deriveInput(envType, info, inputFields) }
          } else {
            ${ deriveObject(envType, info, allFields) }
          }
      }
    }
  }

  def findLeafConstructors(of: Symbol): List[Symbol] =
    of.children.flatMap { child =>
      if (child.flags.is(Flags.Trait)) {
        findLeafConstructors(child)
      } else {
        List(child)
      }
    }

  def getSubclassType(subclassTree: Tree): TypeRepr =
    subclassTree match {
      case cls: ClassDef          => cls.constructor.returnTpt.tpe
      case ValDef(_, tpt, _)      => tpt.tpe
      case Bind(_, pattern: Term) => pattern.tpe
    }

  def deriveInterface(
    info: GraphQLInfo,
    envType: TypeRepr,
    fields: List[Field],
    subtypeInOuts: Map[Symbol, (List[Field], List[Field])]
  ): Expr[caliban.introspection.adt.__Type] = {
    val fieldExprs: List[Expr[caliban.introspection.adt.__Field]] =
      fields.map { field =>
        deriveField(envType, field)
      }
    val subtypeExprs                                              =
      subtypeInOuts.map { case (subtype, (_, outs)) =>
        val subtypeInfo = extractInfo(subtype, None)
        deriveObject(envType, subtypeInfo, outs)
      }.toList

    '{
      lazy val iface: caliban.introspection.adt.__Type = caliban.schema.Types.makeInterface(
        Some(${ info.name }),
        ${ info.description },
        () => List(${ Varargs(fieldExprs) }: _*),
        List(${ Varargs(subtypeExprs) }: _*).map(_.copy(interfaces = () => Some(List(iface))))
      )
      iface
    }
  }

  def deriveUnion(
    info: GraphQLInfo,
    envType: TypeRepr,
    subtypeInOuts: Map[Symbol, (List[Field], List[Field])]
  ): Expr[caliban.introspection.adt.__Type] = {
    val subtypeExprs =
      subtypeInOuts.map { case (subtype, (_, outs)) =>
        val subtypeInfo = extractInfo(subtype, None)
        deriveObject(envType, subtypeInfo, outs)
      }.toList

    '{
      caliban.schema.Types.makeUnion(
        Some(${ info.name }),
        ${ info.description },
        List(${ Varargs(subtypeExprs) }: _*)
      )
    }
  }

  def deriveEnumValue(subtype: Symbol): Expr[caliban.introspection.adt.__EnumValue] = {
    val subtypeInfo = extractInfo(subtype, None)
    '{
      caliban.introspection.adt.__EnumValue(
        name = ${ subtypeInfo.name },
        description = ${ subtypeInfo.description },
        isDeprecated = ${ subtypeInfo.isDeprecated },
        deprecationReason = ${ subtypeInfo.deprecationReason }
      )
    }
  }

  def deriveEnum(
    info: GraphQLInfo,
    subtypeInOuts: Map[Symbol, (List[Field], List[Field])]
  ): Expr[caliban.introspection.adt.__Type] = {
    val enumValues =
      subtypeInOuts.map { case (subtype, _) =>
        deriveEnumValue(subtype)
      }.toList

    '{
      caliban.schema.Types.makeEnum(
        Some(${ info.name }),
        ${ info.description },
        List(${ Varargs(enumValues) }: _*),
        None
      )
    }
  }

  def deriveSum(envType: TypeRepr, targetSym: Symbol, targetType: TypeRepr): Expr[Schema[R, T]] = {
    val subclasses = findLeafConstructors(targetSym)
    val outputs    = getAllFields(targetSym, targetType)
    val info       = extractInfo(targetSym, None)

    val subclassInOut =
      subclasses.map { subclass =>
        val subclassType = getSubclassType(subclass.tree)
        val inputs       = getInputFields(subclass, subclassType)
        val outputs      = getAllFields(subclass, subclassType)

        (subclass, (inputs, outputs))
      }.toMap

    val isEnum      = outputs.isEmpty && subclassInOut.forall { case (_, (in, out)) => in.isEmpty && out.isEmpty }
    val isUnion     = !isEnum && outputs.isEmpty
    val isInterface = !isEnum && !isUnion

    def generateResolveMatch(value: Expr[T])(using Quotes): Expr[caliban.schema.Step[R]] =
      Match(
        value.asTerm,
        subclassInOut.map { case (subclass, (_, outs)) =>
          val subclassType = getSubclassType(subclass.tree)
          val subclassInfo = extractInfo(subclass, None)
          val sym          = Symbol.newBind(Symbol.spliceOwner, "x", Flags.EmptyFlags, subclassType)
          val pattern      = Bind(sym, Typed(Ref(sym), TypeIdent(subclassType.typeSymbol)))
          val rhs          = '{
            ObjectStep.apply[R](
              ${ subclassInfo.name },
              List(${ Varargs(outs.map(field => deriveStepWithName(Ref(sym).asExprOf[T], envType, field))) }: _*).toMap
            )
          }

          CaseDef(pattern, None, rhs.asTerm)
        }.toList
      ).asExprOf[caliban.schema.Step[R]]

    '{
      new Schema[R, T] {
        def resolve(value: T): caliban.schema.Step[R]                                                           =
          ${ generateResolveMatch('{ value }) }
        protected[this] def toType(isInput: Boolean, isSubscription: Boolean): caliban.introspection.adt.__Type =
          ${
            if (isInterface) {
              deriveInterface(info, envType, outputs, subclassInOut)
            } else if (isUnion) {
              deriveUnion(info, envType, subclassInOut)
            } else {
              deriveEnum(info, subclassInOut)
            }
          }
      }
    }
  }

  val envType    = TypeRepr.of[R]
  val targetType = TypeRepr.of[T]
  val targetTree = TypeTree.of[T]
  val targetSym  = targetTree.symbol

  val isCaseClass   = targetSym.flags.is(Flags.Case)
  val isSealedTrait =
    (targetSym.flags.is(Flags.Trait) && targetSym.flags.is(Flags.Sealed)) || (targetSym.flags.is(Flags.Enum))
  val isOpenTrait   = !isSealedTrait && targetSym.flags.is(Flags.Trait)

  val result =
    if (isCaseClass || isOpenTrait) {
      deriveProduct(envType, targetSym, targetType)
    } else {
      deriveSum(envType, targetSym, targetType)
    }

  result
}
