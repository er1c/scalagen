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

import com.github.javaparser.ast.visitor._
import UnitTransformer._
import com.mysema.scalagen.ast.BeginClosureExpr
import com.github.javaparser.ast.expr.NameExpr
import com.github.javaparser.ast.CompilationUnit
import com.github.javaparser.ast.Node
import com.github.javaparser.ast.expr.MethodCallExpr
import com.github.javaparser.ast.stmt.ForStmt
import com.github.javaparser.ast.expr.VariableDeclarationExpr
import com.github.javaparser.ast.stmt.ForEachStmt
import com.github.javaparser.ast.stmt.Statement
import com.github.javaparser.ast.expr.Expression
import com.github.javaparser.ast.stmt.BlockStmt
import com.github.javaparser.ast.expr.EnclosedExpr
import com.github.javaparser.ast.stmt.IfStmt
import com.github.javaparser.ast.stmt.ExpressionStmt
import com.github.javaparser.ast.expr.ConditionalExpr
import com.github.javaparser.ast.expr.AssignExpr
import com.github.javaparser.ast.stmt.BreakStmt
import com.github.javaparser.ast.stmt.SwitchEntry
import com.github.javaparser.ast.body.VariableDeclarator

object ControlStatements extends ControlStatements

/**
 * ControlStatements transform ForStmt, SwitchEntryStmt and If statements
 */
class ControlStatements extends UnitTransformerBase {
  
  private val KEY = new NameExpr("key")
  
  private val VALUE = new NameExpr("value")
  
  private val toUnderscore = new ModifierVisitor[Set[String]] {    
    override def visitName(n: String, arg: Set[String]): String = {
      if (arg.contains(n)) "_" else n
    }  
  }
  
  private def numMatchingNames(n: Node, variableName: String): Int = {
    var matched = 0
    val visitor = new ModifierVisitor[Null] {
      override def visitName(n: String, dummy: Null): String = {
        if (n == variableName) matched += 1
        n
      }
    }
    n.accept(visitor, null)
    matched
  }
  
  private val toKeyAndValue = new ModifierVisitor[String] {
    override def visit(nn: MethodCallExpr, arg: String): Node = {
      val n = super.visit(nn, arg).asInstanceOf[MethodCallExpr]
      n match {
        // case MethodCall(str(`arg`), "getKey", Nil) => KEY
        // case MethodCall(str(`arg`), "getValue", Nil) => VALUE
        // Scope, name, arguments
        case MethodCall(str("arg"), "getKey", Nil) => KEY
        case MethodCall(str("arg"), "getValue", Nil) => VALUE
        case _ => n
      }
    }    
  }
     
  def transform(cu: CompilationUnit): CompilationUnit = {
    cu.accept(this, cu).asInstanceOf[CompilationUnit] 
  }  
        
  override def visit(nn: ForStmt, arg: CompilationUnit): Node = {
    // transform
    //   for (int i = 0; i < x; i++) block 
    // into
    //   for (i <- 0 until x) block
    val n = super.visit(nn, arg).asInstanceOf[ForStmt]    
    n match {
      case For((init: VariableDeclarationExpr) :: Nil, Some(l lt r), incr(_) :: Nil, _) => {
        val until = new MethodCallExpr(init.getVariables.get(0).getInitializer().asScala.getOrElse(null), "until", r :: Nil)
        init.getVariables.get(0).setInitializer(null)
        new ForEachStmt(init, until, n.getBody)
      }
      case _ => n
    }
  }
  
  override def visit(nn: MethodCallExpr, arg: CompilationUnit): Node = {
    // transform
    //   System.out.println
    // into 
    //   println
    val n = super.visit(nn, arg).asInstanceOf[MethodCallExpr]
    n match {
      case MethodCall(str("System.out"), "println", args) => {
        new MethodCallExpr(null, "println", args)
      }
      case _ => n
    }
  }
  
  override def visit(nn: ForEachStmt, arg: CompilationUnit): Node = {
    val n = super.visit(nn, arg).asInstanceOf[ForEachStmt]
    n match {
      case ForEach(
          VariableDeclaration(t, (v: VariableDeclarator) :: Nil), 
          MethodCall(scope, "entrySet", Nil), body) => {
        val vid = v.getNameAsString
        new ForEachStmt(
          VariableDeclaration(emptyModifiers, "(key, value)", JavaType.Object), 
          scope.getOrElse(null), 
          n.getBody.accept(toKeyAndValue, vid).asInstanceOf[Statement]
        )
      }
      case _ => n
    }    
  }
  
  // TODO : maybe move this to own class
  override def visit(nn: BlockStmt, arg: CompilationUnit): Node = {
    // simplify
    //   for (format <- values if format.mimetype == contentType) return format
    //   defaultFormat
    // into
    //   values.find(_.mimetype == contenType).getOrElse(defaultFormat)
    val n = super.visit(nn, arg).asInstanceOf[BlockStmt]
    n match {
      case Block( 
          ForEach(v, it, If(cond, Return(rv1), null)) ::
          Return(rv2) :: Nil) => createFindCall(it, v, cond, rv1, rv2)
      case _ => n
    }
  }
  
  private def createClosure(vid: String, expr: Expression): List[Expression] = numMatchingNames(expr, vid) match {
    case 0 => List(new BeginClosureExpr("_"), expr)
    case 1 => List(expr.accept(toUnderscore, Set(vid)).asInstanceOf[Expression])
    case _ => List(new BeginClosureExpr(vid), expr)
  }
  
  private def createFindCall(it: Expression, v: VariableDeclarationExpr, 
      cond: Expression, rv1: Expression, rv2: Expression): Statement = {
    val vid = v.getVariables.get(0).getNameAsString
    val newCond = createClosure(vid, cond)
    val newIt = it match {
      case MethodCall(_, "until", _ :: Nil) => new EnclosedExpr(it)
      case _ => it
    }
    val findCall = new MethodCallExpr(newIt, "find", newCond)
    val expr = if (vid == rv1.toString) findCall
               else new MethodCallExpr(findCall, "map", createClosure(vid, rv1))
    val getOrElse = new MethodCallExpr(expr, "getOrElse", rv2 :: Nil)
    new BlockStmt(new ExpressionStmt(getOrElse) :: Nil)
  } 
  
  override def visit(nn: IfStmt, arg: CompilationUnit): Node = {
    // transform
    //   if (condition) target = x else target = y
    // into
    //   target = if (condition) e else y    
    val n = super.visit(nn, arg).asInstanceOf[IfStmt]    
    n match {
      case If(cond, Types.Expression(t1 set v1), Some(Types.Expression(t2 set v2))) if t1 == t2 => {
        new ExpressionStmt(new AssignExpr(t1, new ConditionalExpr(n.getCondition, v1, v2), Assign.assign))  
      }
      case _ => n
    }    
  }
  
  override def visit(nn: SwitchEntry, arg: CompilationUnit) = {    
    // remove break
    val n = super.visit(nn, arg).asInstanceOf[SwitchEntry]
    val size = if (n.getStatements == null) 0 else n.getStatements.size
    if (size > 1 && n.getStatements.get(size-1).isInstanceOf[BreakStmt]) {
      //n.getStmts.remove(size-1)
      n.setStatements(n.getStatements.dropRight(1))
    }
    n
  }
    
}