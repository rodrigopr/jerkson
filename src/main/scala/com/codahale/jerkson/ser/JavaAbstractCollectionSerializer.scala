package com.codahale.jerkson.ser

import com.fasterxml.jackson.core.JsonGenerator
import com.fasterxml.jackson.databind.{SerializerProvider, JsonSerializer}
import java.{util => ju}
import scala.collection.JavaConversions._

class JavaAbstractCollectionSerializer[V] extends JsonSerializer[ju.AbstractCollection[V]] {
  def serialize(value: ju.AbstractCollection[V], json: JsonGenerator,
                provider: SerializerProvider) {
    json.writeStartArray()
    for (element <- value.iterator()) {
      provider.defaultSerializeValue(element, json)
    }
    json.writeEndArray()
  }
}
