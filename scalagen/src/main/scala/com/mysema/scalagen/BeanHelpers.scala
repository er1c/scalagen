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

import com.mysema.scala.BeanUtils
import Types._
import com.github.javaparser.ast.body.MethodDeclaration
import com.github.javaparser.ast.stmt.ReturnStmt
import com.github.javaparser.ast.stmt.BlockStmt

/**
 * 
 */
trait BeanHelpers extends Helpers {
  import Helpers._
  import Types._
    
  private val getter = "get\\w+".r
  
  private val setter = "set\\w+".r
  
  private val booleanGetter = "is\\w+".r


  def isBeanGetter(method: MethodDeclaration): Boolean = method match {
    case Method(getter(_*), _, Nil, Some(Return(field(_)))) => true
    case Method(getter(_*), _, Nil, Some(b: BlockStmt)) => isLazyCreation(b, getProperty(method))
    case _ => false
  }
  
  def isBooleanBeanGetter(method: MethodDeclaration): Boolean = method match {
    case Method(booleanGetter(_*), JavaType.Boolean, Nil, Some(Return(field(_)))) => true
    case Method(booleanGetter(_*), JavaType.Boolean, Nil, Some(b: BlockStmt)) => isLazyCreation(b, getProperty(method))
    case _ => false
  }
      
  def isBeanSetter(method: MethodDeclaration): Boolean = method match {
    case Method(setter(_*), JavaType.Void, _ :: Nil, Some(Expression(_ set _))) => true
    case _ => false
  }  
  
  def getProperty(method: MethodDeclaration) = {
    val name = method.getName
    BeanUtils.uncapitalize(name.asString.substring(if (name.asString.startsWith("is")) 2 else 3))
  }
}