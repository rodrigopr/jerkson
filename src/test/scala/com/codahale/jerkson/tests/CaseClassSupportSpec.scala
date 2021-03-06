package com.codahale.jerkson.tests

import com.codahale.jerkson.Json._
import org.scalatest.matchers.MustMatchers
import org.scalatest.FreeSpec

import com.codahale.jerkson.ParsingException
import com.fasterxml.jackson.databind.node.IntNode


class CaseClassSupportSpec extends FreeSpec with MustMatchers {
  "A basic case class" - {
    "generates a JSON object with matching field values" in {
      generate(CaseClass(1, "Coda")).must(be("""{"id":1,"name":"Coda"}"""))
    }

    "is parsable from a JSON object with corresponding fields" in {
      parse[CaseClass]("""{"id":1,"name":"Coda"}""").must(be(CaseClass(1, "Coda")))
    }

    "is parsable from a JSON object with extra fields" in {
      parse[CaseClass]("""{"id":1,"name":"Coda","derp":100}""").must(be(CaseClass(1, "Coda")))
    }

    "is not parsable from an incomplete JSON object" in {
      parse[CaseClass]("""{"id":1}""").must(be(CaseClass(1, null)))
    }
  }

  "A case class with enum" - {
    "should parse json with enum values" in {
      parse[CaseClassWithEnum]("""{"id":1,"name":"Coda","enum":"ALT1"}""").must(be(CaseClassWithEnum(1, EnumX.ALT1, "Coda")))
    }

    "should parse json with null enum value" in {
      parse[CaseClassWithEnum]("""{"id":1,"name":"Coda","enum":null}""").must(be(CaseClassWithEnum(1, null, "Coda")))
    }

    "should generate json with enum values" in {
      generate(CaseClassWithEnum(1, EnumX.ALT1, "Coda")).must(be("""{"enum":"ALT1","id":1,"name":"Coda"}"""))
    }

    "should generate json with null enum values" in {
      generate(CaseClassWithEnum(1, null, "Coda")).must(be("""{"enum":null,"id":1,"name":"Coda"}"""))
    }
  }

  "A case class with public fields" - {
    "should parse json with public fields" in {
      val parsed = parse[CaseClassWithPublicFields]( """{"id":1,"uncomfortable":"Good Touch","unpleasant":"The Creeps"}""")
      parsed.must(be(CaseClassWithPublicFields(1)))
      parsed.uncomfortable.must(be("Good Touch"))
      parsed.unpleasant.must(be("The Creeps"))
    }

    "should parse json with public fields missing" in {
      val parsed = parse[CaseClassWithPublicFields]( """{"id":1}""")
      parsed.must(be(CaseClassWithPublicFields(1)))
      parsed.uncomfortable.must(be("Bad Touch"))
      parsed.unpleasant.must(be("The Creeps"))
    }

    "should generate json with public fields" in {
      generate(CaseClassWithPublicFields(1)).must(be("""{"id":1,"uncomfortable":"Bad Touch","unpleasant":"The Creeps"}"""))
    }

    "should parse public fields from super class" in {
      val parsed = parse[CaseClassWithPublicFieldsExp]( """{"id":1,"uncomfortable":"Good Touch","unpleasant":"The Creeps","superUncomfortable":"Nothing"}""")
      parsed.must(be(CaseClassWithPublicFieldsExp(1)))
      parsed.uncomfortable.must(be("Good Touch"))
      parsed.unpleasant.must(be("The Creeps"))
      parsed.superUncomfortable.must(be("Nothing"))
    }

    "should parse public fields with fields from super class missing" in {
      val parsed = parse[CaseClassWithPublicFieldsExp]( """{"id":1,"superUncomfortable":"Nothing"}""")
      parsed.must(be(CaseClassWithPublicFieldsExp(1)))
      parsed.uncomfortable.must(be("Bad Touch"))
      parsed.unpleasant.must(be("The Creeps"))
      parsed.superUncomfortable.must(be("Nothing"))
    }

    "should generate json with fields from super class" in {
      generate(CaseClassWithPublicFieldsExp(1)).must(be("""{"id":1,"superUncomfortable":"Multiple Bad Touch","uncomfortable":"Bad Touch","unpleasant":"The Creeps"}"""))
    }
  }

  "A case class with lazy fields" - {
    "generates a JSON object with those fields evaluated" in {
      generate(CaseClassWithLazyVal(1)).must(be("""{"id":1,"woo":"yeah"}"""))
    }

    "is parsable from a JSON object without those fields" in {
      parse[CaseClassWithLazyVal]("""{"id":1}""").must(be(CaseClassWithLazyVal(1)))
    }

    "is not parsable from an incomplete JSON object" in {
      intercept[ParsingException] {
        parse[CaseClassWithLazyVal]("""{}""")
      }.getMessage must be("Invalid JSON. field 'id' is required cause it's not nullable.")
    }
  }

  "A case class with ignored members" - {
    "generates a JSON object without those fields" in {
      generate(CaseClassWithIgnoredField(1)).must(be("""{"id":1}"""))
      generate(CaseClassWithIgnoredFields(1)).must(be("""{"id":1}"""))
    }

    "is parsable from a JSON object without those fields" in {
      parse[CaseClassWithIgnoredField]("""{"id":1}""").must(be(CaseClassWithIgnoredField(1)))
      parse[CaseClassWithIgnoredFields]("""{"id":1}""").must(be(CaseClassWithIgnoredFields(1)))
    }

    "is not parsable from an incomplete JSON object" in {
      intercept[ParsingException] {
        parse[CaseClassWithIgnoredField]("""{}""")
      }.getMessage must be("Invalid JSON. field 'id' is required cause it's not nullable.")

      intercept[ParsingException] {
        parse[CaseClassWithIgnoredFields]("""{}""")
      }.getMessage  must be("Invalid JSON. field 'id' is required cause it's not nullable.")
    }
  }

  "A case class with transient members" - {
    "generates a JSON object without those fields" in {
      generate(CaseClassWithTransientField(1)).must(be("""{"id":1}"""))
    }

    "is parsable from a JSON object without those fields" in {
      parse[CaseClassWithTransientField]("""{"id":1}""").must(be(CaseClassWithTransientField(1)))
    }

    "is not parsable from an incomplete JSON object" in {
      intercept[ParsingException] {
        parse[CaseClassWithTransientField]("""{}""")
      }.getMessage must be ("Invalid JSON. field 'id' is required cause it's not nullable.")
    }
  }

  "A case class with an overloaded field" - {
    "generates a JSON object with the nullary version of that field" in {
      generate(CaseClassWithOverloadedField(1)).must(be("""{"id":1}"""))
    }
  }

  "A case class with an Option[String] member" - {
    "generates a field if the member is Some" in {
      generate(CaseClassWithOption(Some("what"))).must(be("""{"value":"what"}"""))
    }

    "is parsable from a JSON object with that field" in {
      parse[CaseClassWithOption]("""{"value":"what"}""").must(be(CaseClassWithOption(Some("what"))))
    }

    "doesn't generate a field if the member is None" in {
      generate(CaseClassWithOption(None)).must(be("""{}"""))
    }

    "is parsable from a JSON object without that field" in {
      parse[CaseClassWithOption]("""{}""").must(be(CaseClassWithOption(None)))
    }

    "is parsable from a JSON object with a null value for that field" in {
      parse[CaseClassWithOption]("""{"value":null}""").must(be(CaseClassWithOption(None)))
    }
  }

  "A case class with a JsonNode member" - {
    "generates a field of the given type" in {
      generate(CaseClassWithJsonNode(new IntNode(2))).must(be("""{"value":2}"""))
    }
  }

  "A case class with members of all ScalaSig types" - {
    val jsonParse = """
               {
                 "map": {
                   "one": "two"
                 },
                 "set": [1, 2, 3],
                 "string": "woo",
                 "list": [4, 5, 6],
                 "seq": [7, 8, 9],
                 "sequence": [10, 11, 12],
                 "collection": [13, 14, 15],
                 "indexedSeq": [16, 17, 18],
                 "randomAccessSeq": [19, 20, 21],
                 "vector": [22, 23, 24],
                 "bigDecimal": 12.0,
                 "bigInt": 13,
                 "int": 1,
                 "long": 2,
                 "char": "x",
                 "bool": false,
                 "short": 14,
                 "byte": 15,
                 "float": 34.5,
                 "double": 44.9,
                 "any": true,
                 "anyRef": "wah",
                 "intMap": {
                   "1": "1"
                 },
                 "longMap": {
                   "2": 2
                 }
               }
               """

    val jsonGenerated = """{"any":true,"anyRef":"wah","bigDecimal":12.0,"bigInt":13,"bool":false,"byte":15,"char":"x","double":44.9,"float":34.5,"indexedSeq":[16,17,18],"int":1,"intMap":{"1":1},"list":[4,5,6],"long":2,"longMap":{"2":2},"map":{"one":"two"},"seq":[7,8,9],"set":[1,2,3],"short":14,"string":"woo","vector":[22,23,24]}"""

    val caseClassVal = CaseClassWithAllTypes(
      map = Map("one" -> "two"),
      set = Set(1, 2, 3),
      string = "woo",
      list = List(4, 5, 6),
      seq = Seq(7, 8, 9),
      indexedSeq = IndexedSeq(16, 17, 18),
      vector = Vector(22, 23, 24),
      bigDecimal = BigDecimal("12.0"),
      bigInt = BigInt("13"),
      int = 1,
      long = 2L,
      char = 'x',
      bool = false,
      short = 14,
      byte = 15,
      float = 34.5f,
      double = 44.9d,
      any = true,
      anyRef = "wah",
      intMap = Map(1 -> 1),
      longMap = Map(2L -> 2L)
    )

    "is parsable from a JSON object with those fields" in {
      parse[CaseClassWithAllTypes](jsonParse).must(be(caseClassVal))
    }

    "can generate JSON object with those fields" in {
      generate[CaseClassWithAllTypes](caseClassVal).must(be(jsonGenerated))
    }
  }

  "A case class nested inside of an object" - {
    "is parsable from a JSON object" in {
      parse[OuterObject.NestedCaseClass]("""{"id": 1}""").must(be(OuterObject.NestedCaseClass(1)))
    }
  }

  "A case class nested inside of an object nested inside of an object" - {
    "is parsable from a JSON object" in {
      parse[OuterObject.InnerObject.SuperNestedCaseClass]("""{"id": 1}""").must(be(OuterObject.InnerObject.SuperNestedCaseClass(1)))
    }
  }

  "A case class with two constructors" - {
    "is parsable from a JSON object with the same parameters as the case accessor" in {
      parse[CaseClassWithTwoConstructors]("""{"id":1,"name":"Bert"}""").must(be(CaseClassWithTwoConstructors(1, "Bert")))
    }

    "is parsable from a JSON object which works with the second constructor" in {
      parse[CaseClassWithTwoConstructors]("""{"id":1}""").must(be(parse[CaseClassWithTwoConstructors]("""{"id":1}""")))
    }
  }

  "A case class with snake-cased fields" - {
    "is parsable from a snake-cased JSON object" in {
      parse[CaseClassWithSnakeCase]("""{"one_thing":"yes","two_thing":"good"}""").must(be(CaseClassWithSnakeCase("yes", "good")))
    }

    "generates a snake-cased JSON object" in {
      generate(CaseClassWithSnakeCase("yes", "good")).must(be("""{"one_thing":"yes","two_thing":"good"}"""))
    }

    "throws errors with the snake-cased field names present" in {
        parse[CaseClassWithSnakeCase]("""{"one_thing":"yes"}""") must be (CaseClassWithSnakeCase("yes", null))
    }
  }

  "A case class with array members" - {
    "is parsable from a JSON object" in {
      val c = parse[CaseClassWithArrays]("""{"one":"1","two":["a","b","c"],"three":[1,2,3]}""")

      c.one.must(be("1"))
      c.two.must(be(Array("a", "b", "c")))
      c.three.must(be(Array(1, 2, 3)))
    }

    "generates a JSON object" in {
      generate(CaseClassWithArrays("1", Array("a", "b", "c"), Array(1, 2, 3))).must(be(
        """{"one":"1","three":[1,2,3],"two":["a","b","c"]}"""
      ))
    }
  }
}
