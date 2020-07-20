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
import com.github.javaparser.ast.expr.Name
import com.github.javaparser.ast.CompilationUnit
import com.github.javaparser.ast.body.ClassOrInterfaceDeclaration
import com.github.javaparser.ast.body.MethodDeclaration
import com.github.javaparser.ast.body.FieldDeclaration
import com.github.javaparser.ast.ImportDeclaration
import com.github.javaparser.ast.Modifier

/**
 * BeanProperties turns field + accessor combinations into @BeanProperty annotated 
 * Scala properties
 */
class BeanProperties(targetVersion: ScalaVersion) extends UnitTransformerBase with BeanHelpers {
  
  val BEAN_PROPERTY_IMPORT =
    if (targetVersion >= Scala210) new ImportDeclaration(new Name("scala.beans.{BeanProperty, BooleanBeanProperty}"), false, false)
    else new ImportDeclaration(new Name("scala.reflect.{BeanProperty, BooleanBeanProperty}"), false, false)
     
  def transform(cu: CompilationUnit): CompilationUnit = {
    cu.accept(this, cu).asInstanceOf[CompilationUnit] 
  }  
  
  override def visit(n: ClassOrInterfaceDeclaration, cu: CompilationUnit): ClassOrInterfaceDeclaration = {      
    // merges getters and setters into properties
    val t = super.visit(n, cu).asInstanceOf[ClassOrInterfaceDeclaration]
    
    // accessors
    val methods = t.getMembers.collect { case m: MethodDeclaration => m }
    val getters = methods.filter(m => isBeanGetter(m) || isBooleanBeanGetter(m))
      .map(m => (getProperty(m) ,m)).toMap      
    val setters = methods.filter(m => isBeanSetter(m))
      .map(m => (getProperty(m), m)).toMap
   
    // fields with accessors
    val fields = t.getMembers.collect { case f: FieldDeclaration => f }
      .filter(_.isPrivate)
      .flatMap( f => f.getVariables.map( v => (v.getNameAsString,v,f) ))
      .filter { case (name,_,_) =>  getters.contains(name) }
          
    // remove accessors 
    for ( (name, variable, field) <- fields) {
      var getter = getters(name)
      //t.getMembers.remove(getter)
      t.setMembers(t.getMembers.filterNot(_ == getter))
      setters.get(name).foreach { s => t.setMembers(t.getMembers.filterNot(_ == s)) }
      
      // make field public
      val isFinal = field.isFinal
      
      //if (isFinal) field
      // field.setModifiers(getter.getModifiers
      //     .addModifier(if (isFinal) Modifier.Key.FINAL else 0))
      
      val annotation = if (getter.getNameAsString.startsWith("is")) BOOLEAN_BEAN_PROPERTY else BEAN_PROPERTY 

      if (field.getAnnotations == null || !field.getAnnotations.contains(annotation)) {
        field.setAnnotations(field.getAnnotations :+ annotation)
      }      
      
      // handle lazy init
      extractOption(getter.getBody.asScala).filter{ isLazyCreation(_, name) }.foreach { body =>
        variable.setInitializer(getLazyInit(body))
        field.addModifier(ScalaModifier.LAZY)
        if (!setters.contains(name)) {
          field.addModifier(Modifier.Keyword.FINAL)
        }
      }
    }
    
    // add BeanProperty import, if properties have been found
    if (!fields.isEmpty && !cu.getImports.contains(BEAN_PROPERTY_IMPORT)) {
      cu.setImports(cu.getImports :+ BEAN_PROPERTY_IMPORT)
    }
    t
  }
    
}
