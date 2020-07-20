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

import com.github.javaparser.ast._
import com.github.javaparser.ast.expr._
import com.github.javaparser.ast.stmt._
import UnitTransformer._
import defs._

object VarToVal extends VarToVal

object defs {
  type Vars = List[Map[String,VariableDeclarationExpr]]
}

/**
 * VarToVal changes var to val if no reassignments are done 
 */
class VarToVal extends ModifierVisitor[Vars] with UnitTransformer {
  
  private val operators = Set(Unary.posDecrement, Unary.posIncrement,
      Unary.preDecrement, Unary.preIncrement)
  
  def transform(cu: CompilationUnit): CompilationUnit = {
    cu.accept(this, Nil).asInstanceOf[CompilationUnit] 
  }  
  
  override def visit(n: BlockStmt, arg: Vars): Node = withCommentsFrom(n, arg) {
    if (n.getStatements == null) {
      return n
    }
    val vars = n.getStatements.collect { case UnitTransformer.Expression(v: VariableDeclarationExpr) => v }
      .flatMap(v => v.getVariables.map(va => (va.getNameAsString,v)))
      .toMap    
    // set vars to final
    vars.values.foreach(_.addModifier(Modifier.Keyword.FINAL))    
    super.visit(n,  vars :: arg)
  }
  
  override def visit(n: AssignExpr, arg: Vars): Node = withCommentsFrom(n, arg) {
    removeFinal(n.getTarget.toString, arg)
    n
  }
  
  override def visit(n: UnaryExpr, arg: Vars): Node = {
    if (operators.contains(n.getOperator)) {
      removeFinal(n.getExpression.toString, arg) 
    } 
    n
  }
  
  // leave VariableDeclarations unchanged
  override def visit(n: VariableDeclarationExpr, arg: Vars): Node = n
    
  private def removeFinal(key: String, arg: Vars) {
    arg.find(_.contains(key))        
      .flatMap(_.get(key))
      .foreach(_.removeModifier(Modifier.Keyword.FINAL))
  }
  
}