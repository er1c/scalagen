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

import com.github.javaparser.ast.expr._
import com.github.javaparser.ast._
import com.github.javaparser.ast.body._
import com.github.javaparser.ast.stmt._

object UnitTransformer extends Helpers with Types {
  
  @inline implicit def toNameExpr(s: String) = new NameExpr(s)
  @inline implicit def toBlockStmt(s: Statement): BlockStmt = new BlockStmt(NodeList.nodeList(s))
  
  //private def safeToString(obj: AnyRef): String = if (obj != null) obj.toString else null
            
  //val BOOLEAN_BEAN_PROPERTY_IMPORT = new Import("scala.reflect.BooleanBeanProperty", false, false)
  
  val BEAN_PROPERTY = new MarkerAnnotationExpr("BeanProperty")
  
  val BOOLEAN_BEAN_PROPERTY = new MarkerAnnotationExpr("BooleanBeanProperty")
      
  abstract class UnitTransformerBase extends ModifierVisitor[CompilationUnit] with UnitTransformer {
        
    override def visit(n: CompilationUnit, arg: CompilationUnit): Node = withCommentsFrom(n, arg) {
      val rv = new CompilationUnit()

      filter(n.getPackageDeclaration, arg).foreach{ rv.setPackageDeclaration }
      rv.setImports(filter[ImportDeclaration](n.getImports, arg))
      // arg is replaced with converted instance here
      val updatedTypes = filter[TypeDeclaration[_]](n.getTypes, rv)
      rv.setTypes(updatedTypes)
      rv
    }
  }
  
}

/**
 * @author tiwe
 *
 */
trait UnitTransformer {
  
  def transform(cu: CompilationUnit): CompilationUnit
  
}