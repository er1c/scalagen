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

import UnitTransformer._
import com.github.javaparser.ast.expr._
import com.github.javaparser.ast.stmt._
import com.github.javaparser.ast.`type`.Type
import com.github.javaparser.ast._
import java.beans.MethodDescriptor
import com.github.javaparser.ast.body.MethodDeclaration

object SimpleEquals extends SimpleEquals

/**
 * SimpleEquals simplifies equals method implementations
 */
class SimpleEquals extends UnitTransformerBase {
  
  private val returnFalse: Statement = new ReturnStmt(new BooleanLiteralExpr(false))
  
  private val replacer = new ModifierVisitor[(NameExpr,NameExpr)]() {    
    
    override def visit(n: BlockStmt, arg: (NameExpr,NameExpr)) = { 
      val visited = super.visit(n, arg)
      val matched = n match {
        case Block(Types.Expression(VariableDeclaration(_, Variable(newName, init) :: Nil)) :: Nil) if init == arg._1 => newName
        case _ => null
      }
      if (matched != null) {
        //n.getStmts.remove(0)
        n.setStatements(n.getStatements.drop(1))
        super.visit(n, (new NameExpr(matched),arg._2))
      } else {
        super.visit(n, arg)
      }
    }
    
    override def visit(n: EnclosedExpr, arg: (NameExpr,NameExpr)) = {
      super.visit(n, arg) match  {
       case Enclosed(n: NameExpr) => n
       case o => o
      }
    }
    
    override def visit(n: NameExpr, arg: (NameExpr,NameExpr)) = {
      if (n == arg._1) arg._2 else n
    }
    
    override def visit(n: CastExpr, arg: (NameExpr,NameExpr)) = {
      if (n.getExpression == arg._1) arg._2 else super.visit(n,arg) 
    }
  }
  
  def transform(cu: CompilationUnit): CompilationUnit = {
    cu.accept(this, cu).asInstanceOf[CompilationUnit] 
  }  
    
  override def visit(n: MethodDeclaration, arg: CompilationUnit) = {
    // transform
    //   if (obj == this) { true }
    //   else if (obj.isInstanceOf[Type]) { obj.asInstanceOf[Type].flag == flag }
    //   else { false }
    // into
    //   obj match {
    //     case obj: JoinFlag => obj.flag == flag
    //     case _ => false
    //   }
    n match {
      case Method("equals", JavaType.Boolean, Parameter(name) :: Nil, Some(stmt)) => {
        val converted = stmt match {
          // skip obj == this check
          case If(
              _ === This(_) | This(_) === _, 
              Return(Literal(true)), 
              Some(If(InstanceOf(_,t), action, Some(Return(Literal(false)))))
            ) => createSwitch(name,t, action)
          case If(InstanceOf(_,t), action, Some(Return(Literal(false))))  => createSwitch(name,t, action)
          case Return(InstanceOf(_,t) and cond) => createSwitch(name, t, new ReturnStmt(cond))
          case _ => null
        }
        if (converted != null) {
          n.setBody(new BlockStmt(converted :: Nil)) 
        }
        n
      }
      case _ => n
    }
  }
  
  private def createSwitch(name: String, t: Type, action: Statement): Statement = {
    //  obj match {
    //    case obj: JoinFlag => obj.flag == flag
    //    case _ => false
    //  }    
    val selector = new NameExpr(name)
    val simplified = action.accept(replacer, (selector,selector)).asInstanceOf[Statement]    
    val matches = new SwitchEntry(NodeList.nodeList(VariableDeclaration(emptyModifiers,name,t)), null, simplified :: Nil)
    val doesnt  = new SwitchEntry(NodeList.nodeList(), null, returnFalse :: Nil)       
    new SwitchStmt(selector, matches :: doesnt :: Nil)
  }
  
    
}  