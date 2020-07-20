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

import com.github.javaparser.ast.body._
import com.github.javaparser.ast.expr._
import com.github.javaparser.ast.stmt._
import com.github.javaparser.ast.`type`._
import com.github.javaparser.ast.ImportDeclaration
import com.github.javaparser.ast.Node
import com.github.javaparser.ast.NodeList
import com.github.javaparser.ast.Modifier
import java.{util => ju}
import scala.collection.JavaConverters._
import scala.reflect.ClassTag

object Types extends Types {


}

/**
 * Types contains type aliases and extractor functionality
 */
trait Types {
  def extract(stmt: Statement): Statement = stmt match {
    case b: BlockStmt => if (b.getStatements != null && b.getStatements.size == 1) {
        b.getStatements.get(0)
      } else b
    case _ => stmt
  }

  def extractOption(stmt: Option[Statement]): Option[Statement] = stmt.map{ extract }

  object MaybeInBlock {
    def unapplySeq(statement: Statement): Option[Seq[Statement]] = statement match {
      case b: BlockStmt if b.getStatements != null => Some(b.getStatements)
      case s => Some(Seq(s))
    }
  }
    
  //private def safeToString(obj: AnyRef): String = if (obj != null) obj.toString else null
  
  private def handle(o: BinaryExpr.Operator, b: BinaryExpr) = {
    if (b.getOperator == o) Some(b.getLeft, b.getRight) else None 
  }
    
  object str {
    def unapply(n: Node): Option[String] = if (n != null) Some(n.toString) else None
    def unapply(n: Option[Node]): Option[String] = n.map(_.toString)
  }
  
  object and {
    def unapply(b: BinaryExpr) = handle(Binary.and, b)    
  }
  
  object or {
    def unapply(b: BinaryExpr) = handle(Binary.or, b)    
  } 
  
  object set {
    def unapply(a: AssignExpr) = if (a.getOperator == Assign.assign) Some(a.getTarget, a.getValue) else None
  }
    
  object === {
    def unapply(b: BinaryExpr) = handle(Binary.equals, b) 
  }
  
  object incr {
    def unapply(u: UnaryExpr) = if (u.getOperator.toString.endsWith("Increment")) Some(u.getExpression) else None
  }
  
  object lt {
    def unapply(b: BinaryExpr) = handle(Binary.less, b)
  }
    
  object field {
    def unapply(f: Expression): Option[String] = f match {
      case FieldAccess(str("this"), field) => Some(field)
      case n: NameExpr => Some(n.getNameAsString)
      case _ => None
    }
  }
  
  object isnull {
    def unapply(b: BinaryExpr) = {
      if (b.getOperator == Binary.equals && b.getRight.isInstanceOf[NullLiteralExpr]) Some(b.getLeft)
      else None        
    }
  }
    
  object Assign {
    val assign = AssignExpr.Operator.ASSIGN
    def unapply(a: AssignExpr) = Some(a.getOperator, a.getTarget, a.getValue)
  }
    
  object Binary {
    val or = BinaryExpr.Operator.OR
    val and = BinaryExpr.Operator.AND
    val equals = BinaryExpr.Operator.EQUALS
    val notEquals = BinaryExpr.Operator.NOT_EQUALS
    val less = BinaryExpr.Operator.LESS
    val greater = BinaryExpr.Operator.GREATER
    def unapply(b: BinaryExpr) = Some(b.getOperator, b.getLeft, b.getRight)    
  }
    
  object Block {
    //def unapply(b: BlockStmt) = Some(if (b != null) toScalaList(b.getStatements) else Nil)
    def unapply(s: Statement) = s match {
      case b: BlockStmt => Some(if (b != null) toScalaList(b.getStatements) else Nil)
      case _ => Some(List(s))
    } 
  }
  
  object Cast {
    def unapply(c: CastExpr) = Some(c.getExpression, c.getType)
  }
  
  object Catch {
    def unapply(c: CatchClause) = Some(c.getParameter, extract(c.getBody))
  }
  
  object ClassOrInterface {
    def unapply(c: ClassOrInterfaceDeclaration) = Some(c.getNameAsString, toScalaList(c.getMembers))
  }
  
  object Conditional {
    def unapply(c: ConditionalExpr) = Some(c.getCondition, c.getThenExpr, c.getElseExpr)
  }
  
  object Constructor {
    def unapply(c: ConstructorDeclaration) = Some(toScalaList(c.getParameters), extract(c.getBody))
    def unapply(c: ExplicitConstructorInvocationStmt) = Some(c.isThis, toScalaList(c.getArguments))
  }

  object Enclosed {
    def unapply(e: EnclosedExpr) = Some(e.getInner)
  }
  
  object FieldAccess {
    def unapply(f: FieldAccessExpr) = Some(f.getScope, f.getNameAsString)
  }
  
  object For {
    def unapply(f: ForStmt) = Some(toScalaList(f.getInitialization), f.getCompare.asScala, toScalaList(f.getUpdate), extract(f.getBody))
  }
  
  object ForEach {
    def unapply(f: ForEachStmt) = Some(f.getVariable, f.getIterable, extract(f.getBody))
  }
  
  object If {
    def unapply(i: IfStmt) = Some(i.getCondition, extract(i.getThenStmt), extractOption(i.getElseStmt.asScala))
  }
  
  object InstanceOf {
    def unapply(i: InstanceOfExpr) = Some(i.getExpression, i.getType)
  }
  
  object Initializer {
    def unapply(i: InitializerDeclaration) = Block.unapply(i.getBody)
  }
    
  object Literal {
    def unapply(l: LiteralExpr) = l match {
      case b: BooleanLiteralExpr => Some(b.getValue)
    }
  }
  
  object Method {
    def unapply(m: MethodDeclaration) = {
      //println(s"method: $m")
      try {
        if (m.getBody.isPresent) Some(
          m.getNameAsString,
          m.getType,
          toScalaList(m.getParameters),
          Some(extract(m.getBody.get))
        ) else Some(m.getNameAsString, m.getType, toScalaList(m.getParameters), None)

      } catch {
        case ex: Exception => println(s"Cought ex on $m"); throw ex;
      }
    }

  }
  
  object MethodCall {
    def unapply(m: MethodCallExpr) = {
      Some(m.getScope.asScala, m.getNameAsString, toScalaList(m.getArguments))
    }
  }
  
  object Name {
    def unapply(n: NameExpr): Option[String] = Some(n.getNameAsString)
  }
  
  object Parameter {
    def unapply(p: Parameter): Option[String] = Some(p.getNameAsString)
  }
    
  object Return {
    //def unapply(r: Option[ReturnStmt]): Option[Expression] = r.flatMap{ _.getExpression.asScala }
    //def unapply(r: Option[Expression]): Option[Expression] = r
    //def unapply(r: Expression): Option[Expression] = Some(r)
    def unapply(r: ReturnStmt): Option[Expression] = r.getExpression.asScala
  }
  
  object Expression {
    def unapply(s: ExpressionStmt) = Some(s.getExpression)
  }
  
  object This {
    def unapply(t: ThisExpr) = t.getTypeName.asScala
  }
  
  object JavaType {
    val Boolean = new PrimitiveType(PrimitiveType.Primitive.BOOLEAN)
    val Int = new PrimitiveType(PrimitiveType.Primitive.INT)
    val Object = new ClassOrInterfaceType("Object")
    val String = new ClassOrInterfaceType("String")
    val Void = new VoidType()
  }
    
  object Unary {
    val positive = UnaryExpr.Operator.PLUS
    val negative = UnaryExpr.Operator.MINUS
    val preIncrement = UnaryExpr.Operator.PREFIX_INCREMENT
    val preDecrement = UnaryExpr.Operator.POSTFIX_DECREMENT
    val not = UnaryExpr.Operator.LOGICAL_COMPLEMENT
    val inverse = UnaryExpr.Operator.BITWISE_COMPLEMENT
    val posIncrement = UnaryExpr.Operator.POSTFIX_INCREMENT
    val posDecrement = UnaryExpr.Operator.POSTFIX_DECREMENT
    def unapply(u: UnaryExpr) = Some(u.getOperator, u.getExpression)
  }
  
  object Variable {
    def unapply(v: VariableDeclarator) = Some(v.getNameAsString, v.getInitializer)    
  }
  
  object VariableDeclaration {
    def apply(mod: NodeList[Modifier], name: String, t: Type): VariableDeclarationExpr = {
      val variable = new VariableDeclarator(t, name)
      new VariableDeclarationExpr(mod, variable :: Nil)
    }

    // TODO: v.getElementType() ???
    def unapply(v: VariableDeclarationExpr) = Some(v.getCommonType(), toScalaList(v.getVariables))
  }
}
