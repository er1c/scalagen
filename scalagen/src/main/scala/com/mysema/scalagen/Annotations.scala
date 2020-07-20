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

import com.github.javaparser.ast.{ImportDeclaration, NodeList}
import UnitTransformer._
import com.github.javaparser.ast.`type`.ClassOrInterfaceType
import com.github.javaparser.ast.expr.{Name, SimpleName}
import com.github.javaparser.ast.CompilationUnit
import com.github.javaparser.ast.body.AnnotationDeclaration
import com.github.javaparser.ast.body.BodyDeclaration
import com.github.javaparser.ast.body.ClassOrInterfaceDeclaration
import com.github.javaparser.ast.body.ConstructorDeclaration
import com.github.javaparser.ast.body.AnnotationMemberDeclaration
import com.github.javaparser.ast.body.Parameter
import com.github.javaparser.ast.stmt.BlockStmt
import com.github.javaparser.metamodel.ParameterMetaModel
import com.github.javaparser.metamodel.PropertyMetaModel

/**
 * Annotations turns Annotation type declarations into normal classes which extend
 * StaticAnnotation
 */
class Annotations(targetVersion: ScalaVersion) extends UnitTransformerBase {
  
  private val staticAnnotationType = new ClassOrInterfaceType("StaticAnnotation")
  
  def transform(cu: CompilationUnit): CompilationUnit = {
    cu.accept(this, cu).asInstanceOf[CompilationUnit] 
  }  
    
  override def visit(n: AnnotationDeclaration, arg: CompilationUnit) = {
    // turns annotations into StaticAnnotation subclasses
    arg.getImports().add(new ImportDeclaration(new Name("scala.annotation.StaticAnnotation"), false, false))
    val clazz = new ClassOrInterfaceDeclaration()
    clazz.setName(n.getName)
      val et = new NodeList[ClassOrInterfaceType]
     et.add(staticAnnotationType)
    clazz.setExtendedTypes(et)
    clazz.setMembers(createMembers(n))
    clazz
  }
  
  private def createMembers(n: AnnotationDeclaration): NodeList[BodyDeclaration[_]] = {
    import com.github.javaparser.ast.Modifier

    // TODO : default values
    val params = n.getMembers
      .collect { case m: AnnotationMemberDeclaration => m }
      .map{ m => 
        new Parameter(m.getType, m.getName.clone)
          .addModifier(ScalaModifier.PROPERTY)
      }
      
    if (params.nonEmpty) {
      val constructor = new ConstructorDeclaration()
      constructor.setParameters(params)
      constructor.setBody(new BlockStmt())
      NodeList.nodeList(constructor)
    } else {
      new NodeList[BodyDeclaration[_]]
    }
  }
    
}  
