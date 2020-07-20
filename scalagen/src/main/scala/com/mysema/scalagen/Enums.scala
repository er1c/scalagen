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
import com.github.javaparser.ast._
import com.github.javaparser.ast.body._
import com.github.javaparser.ast.`type`.ClassOrInterfaceType
import com.github.javaparser.ast.expr._
import com.github.javaparser.ast.stmt.ReturnStmt

object Enums extends Enums

/**
 * Enums converts Java enum type declarations into Scala enumerations
 */
class Enums extends UnitTransformerBase {
  
  private val enumerationType = new ClassOrInterfaceType("Enumeration")
  
  private val valType = new ClassOrInterfaceType("Val")
  
  private val valueType = new ClassOrInterfaceType("Value")
  
  def transform(cu: CompilationUnit): CompilationUnit = {
    cu.accept(this, cu).asInstanceOf[CompilationUnit] 
  }   
    
  override def visit(n: EnumDeclaration, arg: CompilationUnit) = {
    // transform enums into Scala Enumerations
    val clazz = new ClassOrInterfaceDeclaration()
    clazz.setExtendedTypes(enumerationType :: Nil)
    clazz.setName(n.getName)
    clazz.setModifiers(ScalaModifier.OBJECT)
    clazz.setMembers(createMembers(n))
    clazz
  }
  
  private def createMembers(n: EnumDeclaration): NodeList[BodyDeclaration[_]] = {
    val typeDecl = new ClassOrInterfaceDeclaration(emptyModifiers, false, n.getName.asString)
    typeDecl.setExtendedTypes(valType :: Nil)
    typeDecl.setImplementedTypes(n.getImplementedTypes)
    typeDecl.setMembers(n.getMembers.filterNot(isStatic))
    
    // entries
    val ty = new ClassOrInterfaceType(n.getName.asString)
    val entries = n.getEntries.map(e => {
      val init = new ObjectCreationExpr(null, ty, e.getArguments)
      new FieldDeclaration(
        NodeList.nodeList(Modifier.finalModifier),
        NodeList.nodeList[AnnotationExpr](),
        NodeList.nodeList(new VariableDeclarator(ty, e.getName.asString, init))
      )
    })
        
    // conversion function
    val conversion = new MethodDeclaration(emptyModifiers, ty, "convertValue")
    conversion.addModifier(ScalaModifier.IMPLICIT)
    conversion.setBody(new ReturnStmt(new CastExpr(ty, "v")))
    conversion.setParameters(new Parameter(valueType, "v") :: Nil)
          
    entries ::: typeDecl :: n.getMembers.filter(isStatic) ::: conversion :: Nil
  }
    
}  