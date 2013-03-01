package com.codahale.jerkson.ser

import com.fasterxml.jackson.core.JsonGenerator
import com.fasterxml.jackson.databind.{SerializerProvider, JsonSerializer}
import java.{util => ju}
import scala.collection.JavaConversions._

class JavaAbstractMapSerializer[K,V] extends JsonSerializer[ju.Map[K ,V]] {
  def serialize(map: ju.Map[K ,V], json: JsonGenerator, provider: SerializerProvider) {
    json.writeStartObject()
    for ((key, value) <- map) {
      provider.defaultSerializeField(key.toString, value, json)
    }
    json.writeEndObject()
  }
}
