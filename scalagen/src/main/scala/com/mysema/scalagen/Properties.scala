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

import java.util.ArrayList
import com.mysema.scala.BeanUtils
import UnitTransformer._
import com.github.javaparser.ast._
import com.github.javaparser.ast.body._
import com.github.javaparser.ast.expr._
import com.github.javaparser.ast.stmt._
import com.github.javaparser.ast.`type`.VoidType

object Properties extends Properties

/**
 * Properties turns field + accessor combinations into annotated 
 * Scala properties
 */
class Properties extends UnitTransformerBase {
    
  def transform(cu: CompilationUnit): CompilationUnit = {
    cu.accept(this, cu).asInstanceOf[CompilationUnit] 
  }  
  
  override def visit(n: ClassOrInterfaceDeclaration, cu: CompilationUnit): ClassOrInterfaceDeclaration = {      
    val t = super.visit(n, cu).asInstanceOf[ClassOrInterfaceDeclaration]
    
    // accessors
    val getters = t.getMembers.collect { case m: MethodDeclaration => m }
      .filter(m => isGetter(m))
      .map(m => (m.getName,m)).toMap      
    
    // fields with accessors
    val fields = t.getMembers.collect { case f: FieldDeclaration => f }
      .filter(_.isPrivate)
      .flatMap( f => f.getVariables.map( v => (v.getName,v,f) ))
      .filter { case (name,_,_) => getters.contains(name) }
          
    // remove accessors 
    for ( (name, variable, field) <- fields) {
      var getter = getters(name)
      val body = getter.getBody.asScala
      if (getter.isAbstract) {
        t.setMembers(t.getMembers.filterNot(_ == getter))
        field.removeModifier(Modifier.Keyword.PRIVATE)
      } else if (body.isDefined && isReturnFieldStmt(body.get(0))) {
        //t.getMembers.remove(getter)
        t.setMembers(t.getMembers.filterNot(_ == getter))
        field.setModifiers(getter.getModifiers)
      } else if (body.isDefined && isLazyCreation(body.get,name.toString)) {
        //t.getMembers.remove(getter)
        t.setMembers(t.getMembers.filterNot(_ == getter))
        variable.setInitializer(getLazyInit(body.get))

        field.setModifiers(getter.getModifiers)
        field.addModifier(ScalaModifier.LAZY)
        field.addModifier(Modifier.Keyword.FINAL)
      }            
    }    
    t
  }
  
  private def isGetter(method: MethodDeclaration): Boolean = method match {
    case Method(n, t, Nil, Some(Block(_ :: rest))) if !t.isInstanceOf[VoidType] => true
    case _ => false
  }    
      
}
