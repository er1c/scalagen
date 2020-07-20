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
package com.mysema.scalagen

import java.util
import java.util.EnumSet

import com.github.javaparser.ast._
import com.github.javaparser.ast.nodeTypes.NodeWithModifiers
import com.github.javaparser.ast.stmt._
import com.github.javaparser.ast.expr._
import com.github.javaparser.ast.body._
import java.{util => ju}

object Helpers {


  // com.github.javaparser.ast.nodeTypes.NodeWithModifiers
}

/**
 * Common helper methods for transformers and ScalaDumpVisitor
 */
trait Helpers {
  import Types._
  import Helpers._

  // implicit def toRichBlockStmt(b: BlockStmt): RichBlockStmt = new RichBlockStmt(b)
  // implicit def toRichNodeWithModifiers(n: NodeWithModifiers[_]): RichNodeWithModifiers = new RichNodeWithModifiers(n)
  // implicit def toRichNodeListModifiers(nl: NodeList[Modifier]): RichNodeListModifiers = new RichNodeListModifiers(nl)

  def emptyModifiers: NodeList[Modifier] = NodeList.nodeList()
    
  //@inline
  def isEmpty(col: JavaCollection[_]): Boolean = col == null || col.isEmpty

  def nonEmptyOption[A](col: JavaCollection[A]): Option[JavaCollection[A]] =
    if(!isEmpty(col)) Some(col) else None
  
  def getAssignment(s: Statement): AssignExpr = s match {
    case Expression(a: AssignExpr) => a
    case _ => null
  }
  
  // TODO use pattern matching
  def getLazyInit(block: BlockStmt): Expression = {
    block.getStatements.get(0).asInstanceOf[IfStmt]
	    .getThenStmt.asInstanceOf[BlockStmt]
	    .getStatements.get(0).asInstanceOf[ExpressionStmt]
      .getExpression.asInstanceOf[AssignExpr]
      .getValue
  }

  //def isLazyCreation(block: Option[BlockStmt], f: String): Boolean = block.map{isLazyCreation(_, f)}.getOrElse(false)

  def isLazyCreation(block: BlockStmt, f: String): Boolean = block match {
    case Block(
        If(
          isnull(field(`f`)), 
          Expression(field(`f`) set init), 
          None,
        ) :: 
        Return(field(`f`)) :: Nil) => true
    case _ => false   
  }
        
  def isAssignment(s: Statement): Boolean = s match {
    case Expression(_ set _) => true
    case _ => false
  }
    
  def isThisConstructor(s: Statement): Boolean = s match {
    case ci: ExplicitConstructorInvocationStmt => ci.isThis
    case _ => false
  }
  
  def isStatic(member: BodyDeclaration[_]): Boolean = member match {
    case t: ClassOrInterfaceDeclaration => t.isStatic || t.isObject || t.isInterface
    case t: TypeDeclaration[_] => t.isStatic || t.isObject
    case f: FieldDeclaration => f.isStatic
    case m: MethodDeclaration => m.isStatic
    case i: InitializerDeclaration => i.isStatic
    case _ => false
  }  
  
  def isHashCode(n: MethodDeclaration): Boolean = n match { 
    case Method("hashCode", JavaType.Int, Nil, _) => true
    case _ => false
  }
    
  def isEquals(n: MethodDeclaration): Boolean = n match {
    case Method("equals", JavaType.Boolean,_ :: Nil, _) => true
    case _ => false
  }
    
  def isReturnFieldStmt(stmt: Statement): Boolean = stmt match {
    case Return(field(_)) => true
    case _ => false
  }
  
  def isSetFieldStmt(stmt: Statement): Boolean = stmt match {
    case Expression(_ set _) => true
    case _ => false
  }
  
  def isToString(n: MethodDeclaration): Boolean = n match {
    case Method("toString", JavaType.String, Nil, _) => true
    case _ => false
  }

  implicit class RichIterator[T](iter: Iterator[T]) {

    def takeUpToWhere(f: T => Boolean): Iterator[T] = new Iterator[T] {
      var done = false

      def next = {
        if (done) throw new NoSuchElementException()
        val n = iter.next;
        done = f(n)
        n
      }

      def hasNext = {
        !done && iter.hasNext;
      }
    }
  }
}