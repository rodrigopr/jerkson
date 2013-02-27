package com.codahale.jerkson.deser

import scala.reflect.runtime.universe._
import com.fasterxml.jackson.core.{JsonToken, JsonParser}
import collection.mutable

//TODO: found a way to make this a normal JsonDeserializer, right now only works on case classes
class EnumerationDeserializer(mirror: Mirror, enumType: Type) {
  val pre = enumType.asInstanceOf[TypeRefApi].pre.typeSymbol

  val moduleSymbol = pre.companionSymbol.asModule
  val moduleMirror = mirror.reflectModule(moduleSymbol)
  val enum = moduleMirror.instance.asInstanceOf[Enumeration]

  def deserialize(jp: JsonParser) = {
    val token = jp.nextToken()

    val value = if(token == JsonToken.VALUE_STRING) {
      val currentToken = jp.getText
      enum.withName(currentToken)
    } else {
      null
    }

    jp.clearCurrentToken()

    value
  }
}

object EnumerationDeserializer {
  private val enumDeserializers = mutable.Map[String, EnumerationDeserializer]()

  def deserializerFor(fullName: String, orElse: => EnumerationDeserializer) = enumDeserializers.getOrElseUpdate(fullName, orElse)
}
