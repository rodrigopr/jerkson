package com.codahale.jerkson.ser

import java.lang.reflect.{Field, Modifier}
import com.codahale.jerkson.JsonSnakeCase
import com.codahale.jerkson.Util._
import com.fasterxml.jackson.core.JsonGenerator
import com.fasterxml.jackson.annotation.{JsonIgnore, JsonIgnoreProperties}
import com.fasterxml.jackson.databind.{SerializerProvider, JsonSerializer}
import reflect.runtime.universe._

class CaseClassSerializer(klass: Class[_], mirror: Mirror) extends JsonSerializer[Any] {
  private val isSnakeCase = klass.isAnnotationPresent(classOf[JsonSnakeCase])
  private val tpe = mirror.classSymbol(klass).toType

  private val ignoredFields = if (klass.isAnnotationPresent(classOf[JsonIgnoreProperties])) {
    klass.getAnnotation(classOf[JsonIgnoreProperties]).value().toSet
  } else Set.empty[String]

  def getFieldFromClassOrSuper(cls: Class[_], fieldName: String): Field = {
    try {
      cls.getDeclaredField(fieldName)
    } catch {
      case e: NoSuchFieldException if cls.getSuperclass != classOf[Object] =>
        getFieldFromClassOrSuper(cls.getSuperclass, fieldName)
    }
  }
  
  private val getters = tpe.members.view
    .filter { m => m.isMethod && m.asMethod.isGetter }
    .filter { m =>
      val name = m.name.toString.trim

      val jField = getFieldFromClassOrSuper(klass, name)
      val isTransient = ((jField.getModifiers & Modifier.TRANSIENT) != 0)

      val ignoreField = jField.isAnnotationPresent(classOf[JsonIgnore])

      !ignoredFields(name) && !ignoreField && !isTransient
    }.map {g => (g.name.toString.trim, g.asMethod) }.toList.sortBy(c => c._1)
  
  def serialize(value: Any, json: JsonGenerator, provider: SerializerProvider) {
    json.writeStartObject()
    val reflectInstance = mirror.reflect(value)
    getters.foreach{ case (name, getter) =>
      val fieldValue = reflectInstance.reflectMethod(getter).apply()
      if(None != fieldValue) {
        provider.defaultSerializeField(if (isSnakeCase) snakeCase(name) else name, fieldValue, json)
      }
    }
    json.writeEndObject()
  }
}
