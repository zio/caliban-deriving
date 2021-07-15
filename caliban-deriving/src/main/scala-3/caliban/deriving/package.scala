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

inline def deriveSchemaInstance[R, T]: Schema[R, T] = 
  ${deriveSchemaInstanceImpl[R, T]}


private def deriveSchemaInstanceImpl[R: Type, T: Type](using Quotes): Expr[Schema[R, T]] = {
  import quotes.reflect.*

  case class GraphQLInfo(
    name: Expr[String],
    inputName: Expr[String],
    description: Expr[Option[String]],
    deprecationReason: Expr[Option[String]],
    directives: Expr[List[caliban.parsing.adt.Directive]]
  ) {
    def isDeprecated: Expr[Boolean] = '{${deprecationReason}.isDefined}
    def directivesOpt: Expr[Option[List[caliban.parsing.adt.Directive]]] = '{Some($directives).filterNot(_.isEmpty)}
  }


  def extractInfo(symbol: Symbol): GraphQLInfo = {
    println(s"Symbol ${symbol.name} annotations: ${symbol.annotations}")

    val nameAnnotation = symbol
        .getAnnotation(TypeRepr.of[GQLName].typeSymbol)
        .flatMap { 
            case Apply(_, List(Literal(StringConstant(name)))) => Some(Expr(name))
            case _ => None
        }
    val inputNameAnnotation = symbol
        .getAnnotation(TypeRepr.of[GQLInputName].typeSymbol)
        .flatMap { 
            case Apply(_, List(Literal(StringConstant(name)))) => Some(Expr(name))
            case _ => None
        }
    val descriptionAnnotation = symbol
        .getAnnotation(TypeRepr.of[GQLDescription].typeSymbol)
        .flatMap { 
            case Apply(_, List(Literal(StringConstant(name)))) => Some(Expr(name))
            case _ => None
        }
    val deprecatedAnnotation = symbol
        .getAnnotation(TypeRepr.of[GQLDeprecated].typeSymbol)
        .flatMap { 
            case Apply(_, List(Literal(StringConstant(name)))) => Some(Expr(name))
            case _ => None
        }
    val directiveAnnotations: Seq[Expr[Directive]] = symbol
        .annotations
        .filter { annotation =>
            annotation.tpe =:= TypeRepr.of[GQLDirective]
        }
        .flatMap { 
            case Apply(_, List(directive)) => Some(directive)
            case _ => None
        }
        .map(_.asExprOf[Directive])

    val name = nameAnnotation.getOrElse(Expr(symbol.name)) // TODO: append type parameter names
    val inputName = inputNameAnnotation.getOrElse('{caliban.schema.Schema.customizeInputTypeName($name)})
    val description: Expr[Option[String]] = descriptionAnnotation match {
        case Some(d) => '{Some($d)}
        case None => '{None}
    }
    val deprecationReason: Expr[Option[String]] = deprecatedAnnotation match {
        case Some(d) => '{Some($d)}
        case None => '{None}
    }
    val directives = '{List(${Varargs(directiveAnnotations)} : _*)}

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
    '{
        () => {
            if (${schema}.optional) {
                ${schema}.toType_(${Expr(isInput)})
            } else {
                Types.makeNonNull($schema.toType_(${Expr(isInput)}))
            }
        }
    }

  def summonSchema(envType: TypeRepr, fieldType: TypeRepr) = 
    (envType.asType match {
            case '[e] => 
                fieldType.asType match {
                    case '[f] => Expr.summon[Schema[e, f]]
                }
        }).getOrElse {
            report.throwError(s"Cannot find an instance of Schema for ${fieldType}")
        }

  def summonArgBuilder(fieldType: TypeRepr) = 
    (fieldType.asType match {
        case '[f] => Expr.summon[ArgBuilder[f]]
    }).getOrElse {
        report.throwError(s"Cannot find an instance of Schema for ${fieldType}")
    }        

  def deriveParam(envType: TypeRepr, field: Symbol, fieldType: TypeRepr): Expr[caliban.introspection.adt.__InputValue] = {
      val info = extractInfo(field)
      val schema =  summonSchema(envType, fieldType)

      '{
          caliban.introspection.adt.__InputValue(
              ${Expr(field.name)},
              ${info.description},
              ${makeCalibanType(schema, isInput = true)},
              None, // TODO
              ${info.directivesOpt}
          )
      }
  }

  def getReturnType(fieldType: TypeRepr) = 
    (fieldType match {
          case MethodType(_, _, returnType) => 
            returnType
          case _ => 
            fieldType
      }).widen

  def deriveField(envType: TypeRepr, field: Symbol, fieldType: TypeRepr): Expr[caliban.introspection.adt.__Field] = {
      val info = extractInfo(field)
      val returnType = getReturnType(fieldType)    
      val schema =  summonSchema(envType, returnType)

      val firstParamList = field.paramSymss.headOption // NOTE: multiple parameter lists are not supported
      val args = 
        firstParamList.filterNot(_.isEmpty) match {
            case Some(params) =>
                // Non-empty list of parameters
                params.map { param => 
                    val paramType = Ref(param).tpe.widen
                    deriveParam(envType, param, paramType)
                }
            case None =>
                // Parameterless or empty parameter list
                Nil
        }

      '{
          caliban.introspection.adt.__Field(
              ${Expr(field.name)},
              ${info.description},
              List(${Varargs(args)} : _*),
              ${makeCalibanType(schema, isInput = false)},
              ${info.isDeprecated},
              ${info.deprecationReason},
              ${info.directivesOpt}
          )
      }
  }

  def deriveStepWithName(resolveValue: Expr[T], envType: TypeRepr, field: Symbol, fieldType: TypeRepr): Expr[(String, Step[R])] = {
      val info = extractInfo(field)
      val returnType = getReturnType(fieldType)    
      val schema =  summonSchema(envType, returnType)
      val fieldName = field.name

      val firstParamList = field.paramSymss.headOption // NOTE: multiple parameter lists are not supported

      val step = 
        returnType.asType match {
            case '[t] =>

            firstParamList.filterNot(_.isEmpty) match {
                case Some(params) =>                
                    // Non-empty list of parameters

                    def buildArgs(args: Expr[Map[String, InputValue]])(using Quotes): Expr[Either[ExecutionError, t]] = {
                        val terms = params.map { param =>
                            // println(s"${param.name}: ${param.tree}")
                            val paramType = param.tree.asInstanceOf[ValDef].tpt.tpe
                            val argBuilder = summonArgBuilder(paramType)
                            '{ ${argBuilder}.build(${args}(${Expr(param.name)})) }.asTerm
                        }

                        // // println(terms.map(_.show))
                                           
                        def unwrap(paramRefs: List[Term], remaining: List[(Symbol, Term)])(using Quotes): Expr[Either[ExecutionError, t]] = 
                            remaining match {
                                case Nil => 
                                    val call = Apply(Select(resolveValue.asTerm, field), paramRefs.reverse).asExprOf[t]
                                    '{Right[ExecutionError, t]($call)}.asExprOf[Either[ExecutionError, t]]
                                case (headSym, headTerm) :: tail => 
                                    val headType = headSym.tree.asInstanceOf[ValDef].tpt.tpe
                                    headType.asType match {
                                        case '[ht] =>
                                            // println(s"headType: ${headType.show}")
                                            // println(s"headTerm: ${headTerm.show}")
                                            val anonFun = Symbol.newMethod(
                                                Symbol.spliceOwner, 
                                                "anonFun", 
                                                MethodType(List(headSym.name))((_: MethodType) => List(headType), (_: MethodType) => TypeRepr.of[Either[ExecutionError, t]]))

                                            val term = Select.unique(headTerm, "flatMap")
                                                .appliedToTypes(List(TypeRepr.of[ExecutionError], returnType))
                                                .appliedTo(
                                                    Block(
                                                        List(
                                                            DefDef(
                                                                anonFun, {
                                                                    case List(List(paramTerm: Term)) =>
                                                                        Some(unwrap(paramTerm.asExprOf[ht].asTerm :: paramRefs, tail).asTerm.changeOwner(anonFun))
                                                                }
                                                            )
                                                        ),
                                                        Closure(Ref(anonFun), None)
                                                    )
                                            ).asExprOf[Either[ExecutionError, t]]

                                            // println(s"=> ${term.asTerm.show(using Printer.TreeStructure)}")
                                            term
                                    }
                            }

                        unwrap(Nil, params zip terms)
                    }

                    '{
                        FunctionStep {
                            args => 
                                ${buildArgs('args)} match {
                                    case Left(error) => QueryStep(ZQuery.fail(error))
                                    case Right(value) => ${schema.asExprOf[Schema[R, t]]}.resolve(value)
                                }                            
                        }
                    }
                case None =>
                    println(s"resolveValue = ${resolveValue.show}; field = ${field.name}")

                    val invoke =
                        if (firstParamList == Some(Nil)) 
                            Apply(Select(resolveValue.asTerm, field), List()).asExprOf[t]
                        else {
                            val selector = Select(resolveValue.asTerm, field).asExprOf[t]
                            println(s"selector = ${selector.show}")
                            selector
                        }
                    
                    '{${schema.asExprOf[Schema[R, t]]}.resolve(${invoke})}
                }
        }

    '{
        (
            ${Expr(fieldName)},
            $step
        )
    }
  }

  def deriveInput(envType: TypeRepr, info: GraphQLInfo, fields: List[(Symbol, TypeRepr)]): Expr[caliban.introspection.adt.__Type] = {
    val fieldExprs: List[Expr[caliban.introspection.adt.__InputValue]] = 
        fields.map { case (field, fieldType) => 
            deriveParam(envType, field, fieldType)
        }

    '{
        caliban.schema.Types.makeInputObject(
            Some(${info.inputName}),
            ${info.description},
            List(${Varargs(fieldExprs)} : _*)
        )
    }
  }    

  def deriveObject(envType: TypeRepr, info: GraphQLInfo, fields: List[(Symbol, TypeRepr)]): Expr[caliban.introspection.adt.__Type] = {
    val fieldExprs: List[Expr[caliban.introspection.adt.__Field]] = 
        fields.map { case (field, fieldType) => 
            deriveField(envType, field, fieldType)
        }

    '{
        caliban.schema.Types.makeObject(
            Some(${info.name}),
            ${info.description},
            List(${Varargs(fieldExprs)} : _*),
            ${info.directives}
        )
    }
  }

  def deriveProduct(envType: TypeRepr, targetSym: Symbol, targetType: TypeRepr): Expr[Schema[R, T]] = {
      // TODO: get annotations from case class parameters too, not only the field symbols
    val inputFields = targetSym.caseFields
        .filterNot(isExcluded)
        .map { field => (field, targetType.memberType(field)) }
    val allFields = (targetSym.declaredFields ++ targetSym.declaredMethods)
        .filter { field => 
            !field.flags.is(Flags.Artifact) &&
            !field.flags.is(Flags.Synthetic) &&
            !field.flags.is(Flags.Protected) &&
            !field.flags.is(Flags.Private)
        }
        .filterNot(isExcluded)
        .map { field => (field, targetType.memberType(field)) }
    val info = extractInfo(targetSym)

    println(s"For ${targetSym.name} input fields are ${inputFields.map(_._1.name)}")
    println(s"For ${targetSym.name} all fields are ${allFields.map(_._1.name)}")

    '{
      new Schema[R, T] {
        def resolve(value: T): caliban.schema.Step[R] = 
            ObjectStep.apply[R](
                ${info.name},
                List(${Varargs(allFields.map { case (fieldSymbol, fieldType) => deriveStepWithName('value, envType, fieldSymbol, fieldType) })} : _*).toMap
            )

        protected[this] def toType(isInput: Boolean, isSubscription: Boolean): caliban.introspection.adt.__Type = {
          if (isInput) {
            ${deriveInput(envType, info, inputFields)}
          } else {
            ${deriveObject(envType, info, allFields)}
          }
        }
      }
    }
  }

  val envType = TypeRepr.of[R]
  val targetType = TypeRepr.of[T]
  val targetTree = TypeTree.of[T]
  val targetSym = targetTree.symbol

  println(s"env type: ${envType.show}")
  println(s"target type: $targetSym")
  println(s"target flags: ${targetSym.flags.show}")

  val isCaseClass = targetSym.flags.is(Flags.Case)
  val isSealedTrait = (targetSym.flags.is(Flags.Trait) && targetSym.flags.is(Flags.Sealed)) || (targetSym.flags.is(Flags.Enum))
  val isOpenTrait = !isSealedTrait && targetSym.flags.is(Flags.Trait)

    val result =
        if (isCaseClass || isOpenTrait) {
            deriveProduct(envType, targetSym, targetType)
        } else {
            '{
            new Schema[R, T] {
                def resolve(value: T): caliban.schema.Step[R] = ???
                protected[this] def  toType(isInput: Boolean, isSubscription: Boolean): caliban.introspection.adt.__Type = ???
            }
            }
        }
    // println("---------")
    // println(result.show)
    // println("---------")
    
    result
}


