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
import com.github.javaparser.ast.CompilationUnit
import com.github.javaparser.ast.body._
import com.github.javaparser.ast.stmt._
import com.github.javaparser.ast.expr._
import com.github.javaparser.ast.NodeList

object Constructors extends Constructors

/**
 * Constructors reorders and normalizes constructors
 */
class Constructors extends UnitTransformerBase {

  def transform(cu: CompilationUnit): CompilationUnit = {
    cu.accept(this, cu).asInstanceOf[CompilationUnit]
  }

  override def visit(n: ClassOrInterfaceDeclaration, cu: CompilationUnit):  ClassOrInterfaceDeclaration = {
    val t = super.visit(n, cu).asInstanceOf[ClassOrInterfaceDeclaration]
    // make members list mutable
    t.setMembers(NodeList.nodeList(t.getMembers))

    // get all constructors
    val constr = t.getMembers.collect { case c: ConstructorDeclaration => c }

    if (constr.isEmpty) {
      return t
    }

    // get first without delegating
    val first = constr.find( c =>
      c.getBody.isEmpty || !isThisConstructor(c.getBody()(0)))

    // move in front of others
    first.filter(_ != constr(0)).foreach { c =>
      t.getMembers.remove(c)
      t.getComment.asScala.foreach { comment => t.setComment(comment) }
      // TODO: should we merge the constructor comment with the class comment?
      c.setComment(null)
      t.getMembers.add(t.getMembers.indexOf(constr(0)), c)
    }

    // copy initializer, if constructor block has non-constructor statements
    val c = first.getOrElse(constr(0))

    // add empty constructor invocation for all other constructors without
    // constructor invocations
    constr.filter(_ != c).foreach { c =>
      if (c.getBody.isEmpty) {// || !c.getBlock()(0).isInstanceOf[ConstructorInvocation]) {
        c.getBody.add(new ExplicitConstructorInvocationStmt(true, null, null))
      }
    }

    if (!c.getBody.isEmpty &&
        !c.getBody.getStatements.filter(!_.isInstanceOf[ExplicitConstructorInvocationStmt]).isEmpty) {

      processStatements(cu, t, c)

      if (!c.getBody.isEmpty &&
          !(c.getBody.size == 1 && c.getBody()(0).isExplicitConstructorInvocationStmt &&
          !c.getBody()(0).asExplicitConstructorInvocationStmt().isThis)) {
        val initializer: BodyDeclaration[_] = new InitializerDeclaration(false, c.getBody)
        t.getMembers().add(t.getMembers.indexOf(c), initializer)
      }

    }

    // add missing delegations
    t.getMembers.collect { case c: ConstructorDeclaration => c }.filter(_ != c)
      .foreach { c =>
        if (!c.getBody.isEmpty && !c.getBody()(0).isInstanceOf[ExplicitConstructorInvocationStmt]) {
          //c.getBlock.getStmts.add(0, new ConstructorInvocation(true, null, null))
          c.getBody.setStatements(new ExplicitConstructorInvocationStmt(true, null, null) :: c.getBody.getStatements)
        }
      }
    t
  }

  private def processStatements(cu: CompilationUnit, t: TypeDeclaration[_], c: ConstructorDeclaration) {
    val fields = t.getMembers.collect { case f: FieldDeclaration => f }
    val variables = fields.flatMap(_.getVariables).map(v => (v.getNameAsString, v)).toMap
    val variableToField = fields.flatMap(f => f.getVariables.map(v => (v.getNameAsString,f)) ).toMap

    var replacements = Map[String, String]()
    
    // go through statements and map assignments to variable initializers
    c.getBody.getStatements.collect { case s: ExpressionStmt => s }
      .filter(isAssignment(_))
      .foreach { s =>
        val assign = s.getExpression.asInstanceOf[AssignExpr]
        if (assign.getTarget.isFieldAccessExpr) {
          val fieldAccess = assign.getTarget.asFieldAccessExpr()
          processFieldAssign(s, assign, fieldAccess, c, variables, variableToField)
        } else if (assign.getTarget.isNameExpr) {
          val namedTarget = assign.getTarget.asNameExpr()
          if (variables.contains(namedTarget.getNameAsString)) {
            if (assign.getValue.isNameExpr) { // field = parameter
              val namedValue = assign.getValue.asNameExpr()
              c.getParameters.find(_.getName == namedValue.getName).foreach { param =>
                val field = variableToField(namedTarget.getNameAsString)
                // rename parameter to field name
                param.setName(namedTarget.getName)
                replacements = replacements.+((param.getNameAsString, namedTarget.getNameAsString))
                copyAnnotationsAndModifiers(field, param)
                // remove field
                field.setVariables(field.getVariables.filterNot(_ == variables(namedTarget.getNameAsString)))
              }
            } else { // field = ?!?
              variables(namedTarget.getNameAsString).setInitializer(assign.getValue)
            }
            c.getBody.remove(s)
          }
        }
      }

    // remove empty field declarations
    fields.filter(_.getVariables.isEmpty).foreach { t.getMembers.remove(_) }
    
    // modify variables in other statements
    val renameTransformer = new RenameTransformer(replacements)
    c.getBody.setStatements(c.getBody.getStatements.map(stmt => {
      if (!stmt.isExpressionStmt) {
        stmt.accept(renameTransformer, cu).asInstanceOf[Statement]
      } else {
        stmt
      }
    }))

  }

  private def processFieldAssign(s: ExpressionStmt, assign: AssignExpr, fieldAccess: FieldAccessExpr,
      c: ConstructorDeclaration, variables: Map[String, VariableDeclarator], variableToField: Map[String, FieldDeclaration] ) = {
    if (fieldAccess.getScope.isInstanceOf[ThisExpr] &&
        variables.contains(fieldAccess.getName.asString())) {
      if (fieldAccess.getName.asString == assign.getValue.toString) {
        val field = variableToField(fieldAccess.getNameAsString)
        c.getParameters.find(_.getName == fieldAccess.getName)
          .foreach(copyAnnotationsAndModifiers(field,_))
        // remove field, as constructor parameter can be used
        //field.getVariables.remove(variables(fieldAccess.getField))
        field.setVariables(field.getVariables.filterNot(_ == variables(fieldAccess.getNameAsString)))

      } else {
        // remove statement, put init to field
        variables(fieldAccess.getNameAsString).setInitializer(assign.getValue)
      }
      c.getBody.remove(s)
    }
  }

  private def copyAnnotationsAndModifiers(f: FieldDeclaration, p: Parameter) = {
    if (f.getAnnotations != null) {
      p.setAnnotations(p.getAnnotations.union(f.getAnnotations))
    }

    p.addModifier(ScalaModifier.PROPERTY)
  }

}
