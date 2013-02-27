package com.codahale.jerkson.ser

import scala.reflect.runtime.universe._
import com.fasterxml.jackson.databind.{SerializerProvider, JsonSerializer}
import com.fasterxml.jackson.core.JsonGenerator

class EnumerationSerializer(enumSymbol: ClassSymbol)  extends JsonSerializer[Enumeration#Value] {
  def serialize(value: Enumeration#Value, jgen: JsonGenerator, provider: SerializerProvider) {
    provider.defaultSerializeValue(value.toString, jgen)
  }
}
