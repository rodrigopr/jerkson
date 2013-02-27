package com.codahale.jerkson.deser

import com.codahale.jerkson.{ParsingException, JsonSnakeCase}
import com.codahale.jerkson.Util._
import com.fasterxml.jackson.databind._
import com.fasterxml.jackson.databind.node.{NullNode, TreeTraversingParser}
import com.fasterxml.jackson.databind.JavaType
import com.fasterxml.jackson.core.{JsonToken, JsonParser}
import scala.reflect.runtime.universe._
import scala.Some

class CaseClassDeserializer(config: DeserializationConfig, javaType: JavaType, classLoader: ClassLoader) extends JsonDeserializer[Object] {
  import CaseClassDeserializer._
  case class FieldInfo(name: String, tpe: Type, term: TermSymbol, javaType: JavaType, isNullable: Boolean)
  case class ConstructorInfo(params: List[String], methodSymbol: Symbol, methodMirror: MethodMirror)

  private val mirror = reflect.runtime.universe.runtimeMirror(classLoader)
  private val classSymbol = mirror.classSymbol(javaType.getRawClass)

  private val isSnakeCase = javaType.getRawClass.isAnnotationPresent(classOf[JsonSnakeCase])

  private val constructors = reflectConstructors()
  private val fields = reflectFields()

  def deserialize(jsonParser: JsonParser, context: DeserializationContext): Object = {
    if (jsonParser.getCurrentToken == JsonToken.START_OBJECT) {
      jsonParser.nextToken()
    }

    if (jsonParser.getCurrentToken != JsonToken.FIELD_NAME &&
      jsonParser.getCurrentToken != JsonToken.END_OBJECT) {
      throw context.mappingException(javaType.getRawClass)
    }

    val node = jsonParser.readValueAsTree[JsonNode]

    val values: Map[String, AnyRef] = fields.flatMap { case (paramName, FieldInfo(name, paramType, paramTerm, fieldJavaType, _)) =>
      val field = node.get(if(isSnakeCase) snakeCase(paramName) else paramName)
      val tp = new TreeTraversingParser(if (field == null) NullNode.getInstance else field, jsonParser.getCodec)

      val value = if (paramType <:< typeOf[Option[_]]) {
        val containerType = paramType.asInstanceOf[TypeRefApi].args.head

        Option(tp.getCodec.readValue(tp, mirror.runtimeClass(containerType)))
      } else if (paramType <:< typeOf[Enumeration#Value]) {
        val fullClassName = paramType.asInstanceOf[TypeRefApi].pre.typeSymbol.fullName
        val deserializer = EnumerationDeserializer.deserializerFor(fullClassName, new EnumerationDeserializer(mirror, paramType))

        deserializer.deserialize(tp)
      } else {
        tp.getCodec.readValue(tp, fieldJavaType)
      }

      if (field != null || value != null) {
        Some(name -> value)
      } else {
        None
      }
    }.toMap

    val betterConstructor = constructors.sortBy { case ConstructorInfo(cParams, _, _) => cParams.count(values.keySet.contains) * -1 }
    val ConstructorInfo(constructorParams, _, constructorMethod) = betterConstructor.head

    val params = constructorParams.map { p =>
      val v = values.getOrElse(p, null)

      if(v == null && !fields(p).isNullable) {
        throw new ParsingException("Invalid JSON. field '%s' is required cause it's not nullable.".format(p), null)
      }

      v
    }.toArray

    val instance = constructorMethod.apply(params: _*)

    if(fields.size != values.size) {
      val remainFields = values.filterKeys(c => !constructorParams.contains(c))
      if(!remainFields.isEmpty) {
        val instanceProxy = mirror.reflect(instance)
        remainFields.map { case (k,v) => (fields(k), v) }.foreach { case (field, value) =>
          val term = field.term
          if (!term.isFinal && !term.asTerm.isVal) {
            instanceProxy.reflectField(term).set(value)
          }
        }
      }
    }

    instance.asInstanceOf[Object]
  }

  private def getJavaTypeFor(tpe: Type): JavaType = {
    val runtimeClass = if(tpe =:= typeOf[Any]) {
      classOf[Any]
    } else if(tpe =:= typeOf[AnyRef]) {
      classOf[AnyRef]
    } else {
      mirror.runtimeClass(tpe)
    }

    val factory = config.getTypeFactory

    if(runtimeClass.isArray) {
      val typeParams = tpe.asInstanceOf[TypeRefApi].args
      factory.constructArrayType(mirror.runtimeClass(typeParams.head))
    } else {
      val typeParams = tpe.asInstanceOf[TypeRefApi].args.map(getJavaTypeFor).toArray
      if(!typeParams.isEmpty) {
        factory.constructSimpleType(runtimeClass, typeParams)
      } else {
        val properRuntimeClass = primitiveScalaMapping.getOrElse(runtimeClass, runtimeClass)

        factory.constructType(properRuntimeClass)
      }
    }
  }

  private def reflectFields() = {
    classSymbol.toType
      .declarations
      .filter(c => !c.isMethod && c.isTerm && (c.asTerm.isVal || c.asTerm.isVar))
      .map { field =>
        val name = field.name.toString.trim
        val fieldType = field.typeSignature
        val isNullable = !(fieldType <:< typeOf[AnyVal])

        name -> FieldInfo(name, fieldType, field.asTerm, getJavaTypeFor(fieldType), isNullable)
    }.toMap
  }

  private def reflectConstructors() = {
    val tpe = classSymbol.toType
    val constructors = tpe.declarations.filter(d => d.isMethod && d.asMethod.isConstructor)

    constructors.map { c =>
      val params = c.asMethod.paramss.flatten.map { p => p.name.toString.trim }
      val methodMirror = mirror.reflectClass(classSymbol).reflectConstructor(c.asMethod)

      ConstructorInfo(params, c, methodMirror)
    }.toList
  }

  override def isCachable = true
}

object CaseClassDeserializer {
  val primitiveScalaMapping = Map[Class[_], Class[_]](
    classOf[Int] -> classOf[java.lang.Integer],
    classOf[Long] -> classOf[java.lang.Long],
    classOf[Float] -> classOf[java.lang.Float],
    classOf[Double] -> classOf[java.lang.Double],
    classOf[Byte] -> classOf[java.lang.Byte],
    classOf[Boolean] -> classOf[java.lang.Boolean]
  )
}