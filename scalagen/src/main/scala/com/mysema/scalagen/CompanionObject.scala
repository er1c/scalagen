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
import com.github.javaparser.ast.NodeList
import com.github.javaparser.ast.body.TypeDeclaration
import com.github.javaparser.ast.body.ConstructorDeclaration
import com.github.javaparser.ast.ImportDeclaration
import com.github.javaparser.ast.body.BodyDeclaration
import com.github.javaparser.ast.body.ClassOrInterfaceDeclaration

object CompanionObject extends CompanionObject

/**
 * CompanionObject moves static members into companion objects
 */
// TODO : use ModifierVisitorAdapter
// TODO : get totally rid of mutable lists
class CompanionObject extends UnitTransformer {
  import UnitTransformer._

  def transform(cu: CompilationUnit): CompilationUnit = {
    if (cu.getTypes != null) {
      val types = cu.getTypes.filter(!_.isObject)
      val cuTypes = new NodeList[TypeDeclaration[_]]()
	    cuTypes.addAll(cu.getTypes)
      handleTypes(cu, types, cuTypes)
      cu.setTypes(cuTypes)
    }
    cu
  }
  
  private def handleTypes(cu: CompilationUnit, types: Seq[TypeDeclaration[_]], members: JavaList[_ >: TypeDeclaration[_]]) {    
    types.foreach { t => handleType(cu,t) }
    
    // get companion objects
    val typeToCompanion: Map[TypeDeclaration[_], TypeDeclaration[_]] =
      types
        .map(t => (t, getCompanionObject(t)))
        .filterNot{ _._2 == null }
        .toMap
       
    for ( (cl, companion) <- typeToCompanion) {
      handleClassAndCompanion(cu, members, cl, companion)
    }   
  }
    
  private def handleType(cu: CompilationUnit, clazz: TypeDeclaration[_]) {
    if (clazz.getMembers != null) {
      val types = clazz.getMembers.collect { case t: TypeDeclaration[_] => t }
        .filter(!_.isObject)    
      val members = new NodeList[BodyDeclaration[_]](clazz.getMembers)
      handleTypes(cu, types, members)
      clazz.setMembers(members)
    }   
  }  
  
  private def handleClassAndCompanion(cu: CompilationUnit, members: JavaList[_ >: TypeDeclaration[_]], 
      clazz: TypeDeclaration[_], companion: TypeDeclaration[_]) {
    // add companion
    members.add(members.indexOf(clazz), companion)
    if (clazz.getMembers.isEmpty) {
      members.remove(clazz)
    } else if (clazz.getMembers.size == 1) {
      clazz.getMembers.get(0) match {
        case c: ConstructorDeclaration => {
          // remove private empty constructor
          if (c.isPrivate && isEmpty(c.getParameters)) {
            members.remove(clazz)
          } 
        }
        case _ => 
      }
    }

    // add import for companion object members, if class has not been removed
    if (members.contains(clazz)) {
      cu.setImports(cu.getImports :+ new ImportDeclaration(clazz.getNameAsString, false, true))
    }
  }

  private def getCompanionObject(t: TypeDeclaration[_]): TypeDeclaration[_] = {
    if (t.getMembers == null) {
      return null
    }
    
    val staticMembers = t.getMembers.filter(isStatic)
    if (staticMembers.nonEmpty) {
      t.setMembers(t.getMembers.filterNot(staticMembers.contains))
      var companion = new ClassOrInterfaceDeclaration(emptyModifiers, false, t.getNameAsString)
      companion.addModifier(ScalaModifier.OBJECT)
      companion.setMembers(staticMembers)
      companion
    } else {
      null
    }
  }
}
