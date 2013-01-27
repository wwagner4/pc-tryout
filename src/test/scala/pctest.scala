package net.entelijan.pc

import org.scalatest._
import scala.util.parsing.combinator._

case class Prop(name: String, value: Value)

trait Value 

case class SingleVal(value: String) extends Value

case class ListVal(value: List[String]) extends Value


object PropParser extends PropParser {

  def apply(conf: String): List[Prop] = parseRule(props, conf)
  
}

/* BNF

props     ::= [prop]
prop      ::= name '=' value
value     ::= listVal | singleVal
listVal   ::= '(' [aVal] ')'
singleVal ::= aVal
aVal      ::= '\w+'
name      ::= '\w+' 

*/


trait PropParser extends RegexParsers {

  def parse(in: String): List[Prop] = {
      parseAll(props, in) match {
         case Success(re, _) => re
         case failure: NoSuccess => {
              val msg = "Could not parse grammar. %s" format (failure)
              throw new IllegalStateException(msg)
         }  
       }
  }

  def props: Parser[List[Prop]]    = rep1(prop)                  ^^ {case v => v}  
  
  def prop: Parser[Prop]           = name ~ "=" ~ value          ^^ {case n ~ _ ~ v => Prop(n, v)}  

  def value: Parser[Value]         = listVal | singleVal         ^^ {case v => v }
  
  def listVal: Parser[ListVal]     = "(" ~ rep1(aVal) ~ ")"      ^^ {case _ ~ v ~ _ => ListVal(v)}

  def singleVal: Parser[SingleVal] = aVal                        ^^ {case v => SingleVal(v)}

  def aVal : Parser[String]        = """\w+""".r                 ^^ {case v => v}

  def name: Parser[String]         = """\w+""".r                 ^^ {case v => v}  


  /**
   * For testing the rules
   */
  protected def parseRule[T](p: Parser[T], in: java.lang.CharSequence): T = {
      parseAll(p, in) match {
         case Success(re, _) => re
         case failure: NoSuccess => {
              val msg = "Could not parse grammar. %s" format (failure)
              throw new IllegalStateException(msg)
         }  
       }
  }




}

class PcSuite extends FunSuite with PropParser {

  test ("call PropParser ") {
           val t = """
a = (1 2)
b =hallo
hallo=b
c = 123456
"""
           val r = List(
              Prop("a", ListVal(List("1", "2"))),
              Prop("b", SingleVal("hallo")),
              Prop("hallo", SingleVal("b")),
              Prop("c", SingleVal("123456")))
           assert(r === PropParser(t))
  }
  
  test ("props: three prop") {
       new PropParser {
           val t = """
a = (1 2)
b =hallo
c = 123456
"""
           val r = List(
              Prop("a", ListVal(List("1", "2"))),
              Prop("b", SingleVal("hallo")),
              Prop("c", SingleVal("123456")))
           assert(r === parseRule(props, t))
       }
  }
  test ("props: two prop") {
       new PropParser {
           val t = """
a = (1 2)
b = hallo
"""
           val r = List(
              Prop("a", ListVal(List("1", "2"))),
              Prop("b", SingleVal("hallo")))
           assert(r === parseRule(props, t))
       }
  }
  test ("props: one prop") {
       new PropParser {
           assert(List(Prop("hallo", ListVal(List("1", "2")))) === parseRule(props, "hallo = (1 2)"))
       }
  }
  test ("prop: 'hallo = (1 2)' OK") {
       new PropParser {
           assert(Prop("hallo", ListVal(List("1", "2"))) === parseRule(prop, "hallo = (1 2)"))
       }
  }
  test ("prop: 'hallo = du' OK") {
       new PropParser {
           assert(Prop("hallo", SingleVal("du")) === parseRule(prop, "hallo = du"))
       }
  }
  test ("prop: 'a=1' OK") {
       new PropParser {
           assert(Prop("a", SingleVal("1")) === parseRule(prop, "a=1"))
       }
  }
  test ("listVal: '(hallo du)' mit '\\t' OK") {
       new PropParser {
           assert(ListVal(List("hallo", "du")) === parseRule(listVal, "(hallo\tdu)"))
       }
  }
  test ("listVal: '(hallo du)' OK") {
       new PropParser {
           assert(ListVal(List("hallo", "du")) === parseRule(listVal, "(hallo du)"))
       }
  }
  test ("listVal: '(1 2)' OK") {
       new PropParser {
           assert(ListVal(List("1", "2")) === parseRule(listVal, "(1 2)"))
       }
  }
  test ("singleVal: 'a b' IllegalStatException") {
       new PropParser {
         intercept[IllegalStateException] {          
            parseRule(singleVal, "a b")
         }
       }
  }
  test ("singleVal: '_hallo' OK") {
       new PropParser {
           assert(SingleVal("_hallo") === parseRule(singleVal, "_hallo"))
       }
  }
  test ("singleVal: 'd' OK") {
       new PropParser {
           assert(SingleVal("d") === parseRule(singleVal, "d"))
       }
  }


}
