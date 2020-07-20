package com.mysema.scalagen

//import com.github.javaparser.ast.`type`._
import org.junit.Test
import org.junit.Assert._
import UnitTransformer._
import java.util.Collections
import com.github.javaparser.ast.body._
import com.github.javaparser.ast.`type`.{PrimitiveType, `Type`}
import com.github.javaparser.ast.NodeList
import com.github.javaparser.ast.`type`.ClassOrInterfaceType

class HelpersTest {
  
  val helpers = new AnyRef with Helpers
  
  @Test
  def IsHashCode {
    val method = new MethodDeclaration(emptyModifiers, "hashCode", PrimitiveType.intType, NodeList.nodeList())
    assertTrue(helpers.isHashCode(method))
  }
  
  @Test
  def IsEquals {
    val method = new MethodDeclaration(emptyModifiers, "equals", PrimitiveType.booleanType, NodeList.nodeList(new Parameter))
    assertTrue(helpers.isEquals(method))
  }
  
  @Test
  def ToString {
    val method = new MethodDeclaration(emptyModifiers, "toString", JavaType.String, NodeList.nodeList())
    assertTrue(helpers.isToString(method))
  } 
}