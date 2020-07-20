/*
 * Copyright (C) 2011, Mysema Ltd
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 * http://www.apache.org/licenses/LICENSE-2.0
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
package com.mysema

import com.github.javaparser.ast.body._
import com.github.javaparser.ast.expr._
import com.github.javaparser.ast.stmt._
import _root_.scala.collection.JavaConversions
import _root_.scala.collection.Set
import com.github.javaparser.ast.NodeList
import com.github.javaparser.ast.Node
import java.{util => ju}
import com.github.javaparser.ast.nodeTypes.NodeWithModifiers
import com.github.javaparser.ast.Modifier
import com.github.javaparser.ast.DataKey

/**
 * scalagen provides common functionality for this package
 */
package object scalagen {

  type JavaCollection[T] = ju.Collection[T]
  
  type JavaList[T] = ju.List[T]
  
  type JavaSet[T] = ju.Set[T]
  
  implicit def toJavaList[T](col: Seq[T]): JavaList[T] = JavaConversions.seqAsJavaList(col) 
  
  implicit def toJavaSet[T](col: Set[T]): JavaSet[T] = JavaConversions.setAsJavaSet(col)
  
  implicit def toNodeList[T <: Node](col: List[T]): NodeList[T] = NodeList.nodeList(col:_*)
  
  implicit def toScalaList[T](col: JavaList[T]): List[T] = {
    if (col != null) JavaConversions.asScalaBuffer(col).toList else Nil 
  }
  
  implicit def toScalaSet[T](col: JavaSet[T]): Set[T] = {
    if (col != null) JavaConversions.asScalaSet(col) else Set[T]()
  }
  

  implicit final class RichOptional[T](val optional: ju.Optional[T]) extends AnyVal {
    def asScala: Option[T] = optional match {
        case null => null
        case _ => if (optional.isPresent) Option(optional.get()) else None
    }
  }

  implicit class RichBlockStmt(val b: BlockStmt) extends AnyVal { 
    def apply(i: Int): Statement = if (isEmpty) null else b.getStatements.get(i)

    def isEmpty: Boolean = b.getStatements == null || b.getStatements.isEmpty

    def add(s: Statement): BlockStmt = b.setStatements(NodeList.nodeList(b.getStatements :+ s))

    def addAll(s: List[Statement]): BlockStmt  =  
      b.setStatements(NodeList.nodeList(b.getStatements ++ s))

    def remove(s: Statement): BlockStmt =
      b.setStatements(NodeList.nodeList(b.getStatements.filterNot(_ == s)))

    def removeAll(s: List[Statement]): BlockStmt =
      b.setStatements(NodeList.nodeList(b.getStatements.filterNot(s.contains)))

    def copy(): BlockStmt = {
      def block = new BlockStmt()
      def stmts = new NodeList[Statement]()
      stmts.addAll(b.getStatements)
      block.setStatements(stmts)
      block
    }
    
    def size = if (b.getStatements != null) b.getStatements.size else 0
  }

  val ScalaModifiersDataKey = new DataKey[Set[ScalaModifier]]() { }

  sealed trait IsScalaModifier[T <: Node] extends Any {
    def node: NodeWithModifiers[_]

    private def hasModifier(m: ScalaModifier): Boolean = {
      val n = node.asInstanceOf[Node]
      n.containsData(ScalaModifiersDataKey) && 
        n.getData(ScalaModifiersDataKey).contains(m)
    }

    final def addModifier(modifier: ScalaModifier): T = {
      val n = node.asInstanceOf[T]
      val existing: Set[ScalaModifier] =
        if (n.containsData(ScalaModifiersDataKey)) n.getData(ScalaModifiersDataKey)
        else Set.empty

      n.setData(ScalaModifiersDataKey, existing + modifier)

      n
    }

    final def setModifiers(modifiers: ScalaModifier*): T = {
      val n = node.asInstanceOf[T]
      n.setData(ScalaModifiersDataKey, modifiers.toSet)
      n
    }

    final def isObject: Boolean   = hasModifier(ScalaModifier.OBJECT)
    final def isLazy: Boolean     = hasModifier(ScalaModifier.LAZY)
    final def isImplicit: Boolean = hasModifier(ScalaModifier.IMPLICIT)
    
    final def isProperty: Boolean = hasModifier(ScalaModifier.PROPERTY)
  }

  sealed trait IsJavaModifier[T <: Node] extends Any {
    def node: NodeWithModifiers[T]

    final def isTransient: Boolean    = node.hasModifier(Modifier.Keyword.TRANSIENT)
    final def isVolatile: Boolean     = node.hasModifier(Modifier.Keyword.VOLATILE)
    final def isPrivate: Boolean      = node.hasModifier(Modifier.Keyword.PRIVATE)
    final def isProtected: Boolean    = node.hasModifier(Modifier.Keyword.PROTECTED)
    final def isAbstract: Boolean     = node.hasModifier(Modifier.Keyword.ABSTRACT)
    final def isStatic: Boolean       = node.hasModifier(Modifier.Keyword.STATIC)
    final def isFinal: Boolean        = node.hasModifier(Modifier.Keyword.FINAL)
    final def isNative: Boolean       = node.hasModifier(Modifier.Keyword.NATIVE)
    final def isStrictfp: Boolean     = node.hasModifier(Modifier.Keyword.STRICTFP)
    final def isSynchronized: Boolean = node.hasModifier(Modifier.Keyword.SYNCHRONIZED)
  }

  implicit final class RichNodeWithModifiers[T <: Node](val node: NodeWithModifiers[T]) extends AnyVal with IsScalaModifier[T] with IsJavaModifier[T]

  //implicit final class RichNodeListModifiers(val nl: NodeList[Modifier]) extends AnyVal with IsScalaModifier with IsJavaModifier
}
