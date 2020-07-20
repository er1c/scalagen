package com.mysema.scalagen

object ScalaModifier {
  case object LAZY extends ScalaModifier
  case object OBJECT extends ScalaModifier
  case object IMPLICIT extends ScalaModifier
  case object PROPERTY extends ScalaModifier
}

sealed trait ScalaModifier