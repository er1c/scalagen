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

import com.github.javaparser.JavaParser
import com.github.javaparser.ParseException
import com.github.javaparser.ast.{CompilationUnit, ImportDeclaration}
import java.io.File
import java.io.FileInputStream
import scala.reflect.ClassTag
import TestDirectoryStructure._
import com.github.javaparser.ast.NodeList

abstract class AbstractParserTest {
  
  def getCompilationUnit(cl: Class[_]): CompilationUnit = {
    var file = new File(s"$SCALA_TEST_DIR_NAME/${cl.getName.replace('.', '/')}.java")
    var in = new FileInputStream(file)
    val parseRes = new JavaParser().parse(in)

    parseRes.getResult.asScala match  {
      case Some(unit) => 
        if (unit.getImports == null) unit.setImports(NodeList.nodeList[ImportDeclaration]())
        unit
      case None    => throw new ParseException(s"Failed parsing: ${parseRes.getProblems()}")
    }
  }
  
  def toScala(obj: AnyRef): String = toScala(getCompilationUnit(obj.getClass))
  
  def toScala[T](implicit ct: ClassTag[T]): String = toScala(getCompilationUnit(ct.runtimeClass))
  
  def toScala[T](settings: ConversionSettings)(implicit ct: ClassTag[T]): String =
    toScala(getCompilationUnit(ct.runtimeClass), settings)
  
  def toScala(unit: CompilationUnit): String = Converter.getInstance().toScala(unit)
  
  def toScala(unit: CompilationUnit, settings: ConversionSettings): String = Converter.getInstance().toScala(unit, settings)
  
}
