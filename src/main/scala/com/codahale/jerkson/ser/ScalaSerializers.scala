package com.codahale.jerkson.ser

import com.codahale.jerkson.AST.JValue
import com.fasterxml.jackson.databind._
import com.fasterxml.jackson.databind.ser.Serializers
import scala.reflect.runtime.universe._
import com.codahale.jerkson.ScalaJsonSerializable
import java.{util => ju}

class ScalaSerializers extends Serializers.Base {
  private[this] val mirror = runtimeMirror(Thread.currentThread().getContextClassLoader)

  override def findSerializer(config: SerializationConfig, javaType: JavaType, beanDesc: BeanDescription) = {
    val ser: Object = if (classOf[Option[_]].isAssignableFrom(beanDesc.getBeanClass)) {
        new OptionSerializer
    } else if (classOf[StringBuilder].isAssignableFrom(beanDesc.getBeanClass)) {
      new StringBuilderSerializer
    } else if (classOf[collection.Map[_,_]].isAssignableFrom(beanDesc.getBeanClass)) {
      new MapSerializer
    } else if (classOf[Range].isAssignableFrom(beanDesc.getBeanClass)) {
      new RangeSerializer
    } else if (classOf[Iterable[_]].isAssignableFrom(beanDesc.getBeanClass)) {
        new IterableSerializer
    } else if (classOf[Iterator[_]].isAssignableFrom(beanDesc.getBeanClass)) {
      new IteratorSerializer
    } else if (classOf[JValue].isAssignableFrom(beanDesc.getBeanClass)) {
      new JValueSerializer
    } else if (classOf[Either[_,_]].isAssignableFrom(beanDesc.getBeanClass)) {
      new EitherSerializer
    } else if (classOf[ju.AbstractCollection[_]].isAssignableFrom(beanDesc.getBeanClass)) {
      new JavaAbstractCollectionSerializer
    } else if (classOf[ju.Map[_,_]].isAssignableFrom(beanDesc.getBeanClass)) {
      new JavaAbstractMapSerializer
    } else if (classOf[Product].isAssignableFrom(beanDesc.getBeanClass) || classOf[ScalaJsonSerializable].isAssignableFrom(beanDesc.getBeanClass)) {
      new CaseClassSerializer(beanDesc.getBeanClass, mirror)
    } else if(classOf[Enumeration#Value].isAssignableFrom(javaType.getRawClass)) {
      new EnumerationSerializer(mirror.classSymbol(javaType.getRawClass))
    } else {
      null
    }


    ser.asInstanceOf[JsonSerializer[Object]]
  }
}
