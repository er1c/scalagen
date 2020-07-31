package com.mysema.scalagen

import com.github.javaparser.ast._
import com.github.javaparser.ast.body._
import com.github.javaparser.ast.comments._
import com.github.javaparser.ast.expr._
import com.github.javaparser.ast.stmt._
import com.github.javaparser.ast.`type`._
import com.github.javaparser.ast.modules._
import com.github.javaparser.ast.visitor.GenericVisitor
import java.util.{ArrayList, Collections}
import Helpers._

import com.github.javaparser.ast.nodeTypes.NodeWithJavadoc
import com.mysema.scalagen.ast.BeginClosureExpr

/**
 * 
 */
abstract class ModifierVisitor[A] extends GenericVisitor[Node, A] {
  
  protected def filter[T <: Node](node: java.util.Optional[T], arg: A): Option[T] = {
    node.asScala.map{ _.accept(this, arg).asInstanceOf[T] }
  }

  protected def filter[T <: Node](node: T, arg: A): T = {
    if (node != null) node.accept(this, arg).asInstanceOf[T]
    else null.asInstanceOf[T]
  }
  
  protected def filter[T <: Node](list: NodeList[T], arg: A): NodeList[T]  = {
    if (list == null || list.isEmpty) {
      new NodeList[T]()
    } else {
      //list.map(_.accept(this, arg).asInstanceOf[T]).filter(_ != null)
      val rv = new NodeList[T]()
      val it = list.iterator()
      while (it.hasNext) {
        val node = it.next()
        if (node != null) {
          rv.add(node.accept(this, arg).asInstanceOf[T])
        }
      }
      rv
    }    
  }

  def withCommentsFrom[T <: Node](origNode: Node, arg: A)(node: => T): T = {
    val newNode = node
    val comment = origNode match {
      case d: NodeWithJavadoc[_] => Option(javadocFor(d, arg))
      case _ => None
    }
    comment orElse origNode.getComment.asScala foreach { newNode.setComment }
    origNode.getOrphanComments.foreach(newNode.addOrphanComment)

    newNode
  }

  protected def visitName(name: String, arg: A) = name

  override def visit(n: AnnotationDeclaration, arg: A) : Node = withCommentsFrom(n, arg) {
    val rv = new AnnotationDeclaration()
    rv.setAnnotations(filter(n.getAnnotations, arg))
    rv.setMembers(filter(n.getMembers, arg))
    rv.setModifiers(n.getModifiers)
    rv.setName(filter(n.getName, arg))
    rv    
  }

  override def visit(n: AnnotationMemberDeclaration, arg: A): Node = withCommentsFrom(n, arg) {
    val rv = new AnnotationMemberDeclaration()
    rv.setAnnotations(filter(n.getAnnotations, arg))

    filter(n.getDefaultValue, arg).foreach{ rv.setDefaultValue }
    rv.setComment(javadocFor(n, arg))
    rv.setModifiers(n.getModifiers)
    rv.setName(filter(n.getName, arg))
    rv.setType(filter(n.getType, arg))
    
    n
  }

  override def visit(n: ArrayAccessExpr, arg: A): Node = withCommentsFrom(n, arg) {
    val rv = new ArrayAccessExpr()
    rv.setIndex(filter(n.getIndex, arg))
    rv.setName(filter(n.getName, arg))    
    rv
  }

  override def visit(n: ArrayCreationExpr, arg: A): Node = withCommentsFrom(n, arg) {
    val rv = new ArrayCreationExpr()    
    rv.setLevels(n.getLevels)
    filter(n.getInitializer, arg).foreach{ rv.setInitializer }
    rv.setElementType(filter(n.getElementType, arg))   
    rv
  }

  override def visit(n: ArrayInitializerExpr, arg: A): Node = withCommentsFrom(n, arg) {
    val rv = new ArrayInitializerExpr()
    rv.setValues(filter(n.getValues, arg))   
    rv
  }

  override def visit(n: AssertStmt, arg: A): Node = withCommentsFrom(n, arg) {
    val rv = new AssertStmt()
    rv.setCheck(filter(n.getCheck, arg))
    filter(n.getMessage, arg).foreach{ rv.setMessage }
    rv
  }

  override def visit(n: AssignExpr, arg: A): Node = withCommentsFrom(n, arg) {
    val rv = new AssignExpr()
    rv.setOperator(n.getOperator)
    rv.setTarget(filter(n.getTarget, arg))
    rv.setValue(filter(n.getValue, arg))    
    rv
  }

  override def visit(n: BinaryExpr, arg: A): Node = withCommentsFrom(n, arg) {
    val rv = new BinaryExpr()
    rv.setOperator(n.getOperator)
    rv.setLeft(filter(n.getLeft, arg))
    rv.setRight(filter(n.getRight, arg))    
    rv
  }

  override def visit(n: BlockStmt, arg: A): Node = withCommentsFrom(n, arg) { new BlockStmt(filter(n.getStatements, arg)) }

  override def visit(n: BooleanLiteralExpr, arg: A): Node = new BooleanLiteralExpr(n.getValue)

  override def visit(n: BreakStmt, arg: A): Node = withCommentsFrom(n, arg) { 
    val s = new BreakStmt()
    n.getLabel.asScala.map{ l => s.setLabel(l) }.getOrElse(s)
  }

  override def visit(n: CastExpr, arg: A): Node = withCommentsFrom(n, arg) {
    new CastExpr(filter(n.getType, arg), filter(n.getExpression, arg))    
  }

  override def visit(n: CatchClause, arg: A): Node = withCommentsFrom(n, arg) {
    new CatchClause(filter(n.getParameter, arg), filter(n.getBody, arg))
  }

  override def visit(n: CharLiteralExpr, arg: A): Node = new CharLiteralExpr(n.getValue)

  override def visit(n: ClassExpr, arg: A): Node = withCommentsFrom(n, arg) { new ClassExpr(filter(n.getType, arg)) }

  override def visit(n: ClassOrInterfaceDeclaration, arg: A): Node = withCommentsFrom(n, arg) {
    val rv = new ClassOrInterfaceDeclaration()
    rv.setAnnotations(filter(n.getAnnotations, arg))
    rv.setExtendedTypes(filter(n.getExtendedTypes, arg))
    rv.setImplementedTypes(filter(n.getImplementedTypes, arg))
    rv.setInterface(n.isInterface)
    rv.setMembers(filter(n.getMembers, arg))
    rv.setModifiers(n.getModifiers)
    rv.setName(filter(n.getName, arg))
    rv.setTypeParameters(filter(n.getTypeParameters, arg))        
    rv
  }

  private def javadocFor(n: NodeWithJavadoc[_], arg: A): JavadocComment = {
    filter(n.getJavadocComment, arg).getOrElse{ new JavadocComment() }
  }

  override def visit(n: ClassOrInterfaceType, arg: A): Node = withCommentsFrom(n, arg) {
    val rv = new ClassOrInterfaceType()
    rv.setName(filter(n.getName, arg))
    filter(n.getScope, arg).foreach{ rv.setScope }
    n.getTypeArguments.asScala.foreach { args =>
      rv.setTypeArguments(filter(args, arg))
    }
    
    rv
  }

  override def visit(n: CompilationUnit, arg: A): Node = withCommentsFrom(n, arg) {
    val rv = new CompilationUnit()
    filter(n.getPackageDeclaration, arg).foreach{ rv.setPackageDeclaration }
    rv.setImports(filter(n.getImports, arg))
    rv.setTypes(filter(n.getTypes, arg))
    rv
  }

  override def visit(n: ConditionalExpr, arg: A): Node = withCommentsFrom(n, arg) {
    val rv = new ConditionalExpr()
    rv.setCondition(filter(n.getCondition, arg))
    rv.setThenExpr(filter(n.getThenExpr, arg))
    rv.setElseExpr(filter(n.getElseExpr, arg))
    rv
  }

  override def visit(n: ConstructorDeclaration, arg: A): Node = withCommentsFrom(n, arg) {
    val rv = new ConstructorDeclaration()
    rv.setAnnotations(filter(n.getAnnotations, arg))
    rv.setBody(filter(n.getBody, arg))
    rv.setModifiers(n.getModifiers)
    rv.setName(filter(n.getName, arg))
    rv.setParameters(filter(n.getParameters, arg))
    rv.setThrownExceptions(filter(n.getThrownExceptions, arg))
    rv.setTypeParameters(filter(n.getTypeParameters, arg))
    rv
  }

  override def visit(n: ContinueStmt, arg: A): Node = withCommentsFrom(n, arg) { 
    val rv = new ContinueStmt()
    n.getLabel.asScala.foreach{ rv.setLabel }
    rv
  }

  override def visit(n: DoStmt, arg: A): Node = withCommentsFrom(n, arg) {
    val rv = new DoStmt()
    rv.setBody(filter(n.getBody, arg))
    rv.setCondition(filter(n.getCondition, arg))
    rv
  }

  override def visit(n: DoubleLiteralExpr, arg: A): Node = new DoubleLiteralExpr(n.getValue)

  override def visit(n: EmptyStmt, arg: A): Node = new EmptyStmt()

  override def visit(n: EnclosedExpr, arg: A): Node = withCommentsFrom(n, arg) {
    new EnclosedExpr(filter(n.getInner, arg))
  }

  override def visit(n: EnumConstantDeclaration, arg: A): Node = withCommentsFrom(n, arg) {
    val rv = new EnumConstantDeclaration()
    rv.setAnnotations(filter(n.getAnnotations, arg))
    rv.setArguments(filter(n.getArguments, arg))
    rv.setClassBody(filter(n.getClassBody, arg))
    rv.setComment(javadocFor(n, arg))
    rv.setName(filter(n.getName, arg))
    rv
  }

  override def visit(n: EnumDeclaration, arg: A): Node = withCommentsFrom(n, arg) {
    val rv = new EnumDeclaration()
    rv.setAnnotations(filter(n.getAnnotations, arg))
    rv.setEntries(filter(n.getEntries, arg))
    rv.setImplementedTypes(filter(n.getImplementedTypes, arg))
    rv.setComment(javadocFor(n, arg))
    rv.setMembers(filter(n.getMembers, arg))
    rv.setModifiers(n.getModifiers)
    rv.setName(filter(n.getName, arg))
    rv
  }

  override def visit(n: ExplicitConstructorInvocationStmt, arg: A): Node = withCommentsFrom(n, arg) {
    val rv = new ExplicitConstructorInvocationStmt()
    rv.setArguments(filter(n.getArguments, arg))
    filter(n.getExpression, arg).foreach{ rv.setExpression }
    rv.setThis(n.isThis)    
    n.getTypeArguments.asScala.foreach{ args =>
      rv.setTypeArguments(filter(args, arg))
    }
    
    rv
  }

  override def visit(n: ExpressionStmt, arg: A): Node = withCommentsFrom(n, arg) {
    new ExpressionStmt(filter(n.getExpression, arg))
  }

  override def visit(n: FieldAccessExpr, arg: A): Node = withCommentsFrom(n, arg) {
    new FieldAccessExpr(filter(n.getScope, arg), visitName(n.getNameAsString, arg))
  }

  override def visit(n: FieldDeclaration, arg: A): Node = withCommentsFrom(n, arg) {
    val rv = new FieldDeclaration()
    rv.setAnnotations(filter(n.getAnnotations, arg))
    rv.setModifiers(n.getModifiers)
    rv.setAllTypes(filter(n.getCommonType, arg)) //  TODO: getElementType?
    rv.setVariables(filter(n.getVariables, arg))
    rv
  }

  override def visit(n: ForEachStmt, arg: A): Node = withCommentsFrom(n, arg) {
    val rv = new ForEachStmt()
    rv.setVariable(filter(n.getVariable, arg))
    rv.setIterable(filter(n.getIterable, arg))
    rv.setBody(filter(n.getBody, arg))
    rv
  }

  override def visit(n: ForStmt, arg: A): Node = withCommentsFrom(n, arg) {
    val rv = new ForStmt()
    rv.setInitialization(filter(n.getInitialization, arg))
    filter(n.getCompare, arg).foreach{ rv.setCompare }
    rv.setUpdate(filter(n.getUpdate, arg))
    rv.setBody(filter(n.getBody, arg))
    rv
  }

  override def visit(n: IfStmt, arg: A): Node = withCommentsFrom(n, arg) {
    val rv = new IfStmt()
    rv.setCondition(filter(n.getCondition, arg))
    rv.setThenStmt(filter(n.getThenStmt, arg))
    filter(n.getElseStmt, arg).foreach{ rv.setElseStmt }
    rv
  }

  override def visit(n: ImportDeclaration, arg: A): Node = withCommentsFrom(n, arg) {
    new ImportDeclaration(n.getName, n.isStatic, n.isAsterisk)    
  }

  override def visit(n: InitializerDeclaration, arg: A): Node = withCommentsFrom(n, arg) {
    val rv = new InitializerDeclaration()
    rv.setAnnotations(filter(n.getAnnotations, arg))
    rv.setBody(filter(n.getBody, arg))
    rv.setStatic(n.isStatic)
    rv
  }

  override def visit(n: InstanceOfExpr, arg: A): Node = withCommentsFrom(n, arg) {
    val rv = new InstanceOfExpr()
    rv.setExpression(filter(n.getExpression, arg))
    rv.setType(filter(n.getType, arg))
    rv
  }

  override def visit(n: IntegerLiteralExpr, arg: A): Node = new IntegerLiteralExpr(n.getValue)

  override def visit(n: JavadocComment, arg: A): Node = new JavadocComment(n.getContent)

  override def visit(n: LabeledStmt, arg: A): Node = new LabeledStmt(n.getLabel, filter(n.getStatement, arg))    

  override def visit(n: LongLiteralExpr, arg: A): Node = new LongLiteralExpr(n.getValue)

  override def visit(n: MarkerAnnotationExpr, arg: A): Node = withCommentsFrom(n, arg) {
    new MarkerAnnotationExpr(filter(n.getName, arg))
  } 

  override def visit(n: MemberValuePair, arg: A): Node = withCommentsFrom(n, arg) {
    new MemberValuePair(n.getName, filter(n.getValue, arg))
  }

  override def visit(n: MethodCallExpr, arg: A): Node = withCommentsFrom(n, arg) {
    val rv = new MethodCallExpr()
    rv.setArguments(filter(n.getArguments, arg))
    rv.setName(filter(n.getName, arg))
    filter(n.getScope, arg).foreach{ rv.setScope }
    n.getTypeArguments.asScala.foreach { args =>
      rv.setTypeArguments(filter(args, arg))
    }
    rv
  }

  override def visit(n: MethodDeclaration, arg: A): Node = withCommentsFrom(n, arg) {
    val rv = new MethodDeclaration()
    rv.setAnnotations(filter(n.getAnnotations, arg))
    filter(n.getBody, arg).foreach{ rv.setBody }
    rv.setModifiers(n.getModifiers)
    rv.setName(filter(n.getName, arg))
    rv.setParameters(filter(n.getParameters, arg))
    rv.setThrownExceptions(filter(n.getThrownExceptions, arg))
    rv.setType(filter(n.getType, arg))
    rv.setTypeParameters(filter(n.getTypeParameters, arg))
    rv
  }

  override def visit(n: NameExpr, arg: A): Node = withCommentsFrom(n, arg) {
    n match {
      case closure: BeginClosureExpr => closure
      case _ => new NameExpr(visitName(n.getNameAsString, arg))
    }
  }

  override def visit(n: NormalAnnotationExpr, arg: A): Node = withCommentsFrom(n, arg) {
    val rv = new NormalAnnotationExpr()
    rv.setName(filter(n.getName, arg))
    rv.setPairs(filter(n.getPairs, arg))
    rv
  }

  override def visit(n: NullLiteralExpr, arg: A): Node = new NullLiteralExpr()

  override def visit(n: ObjectCreationExpr, arg: A): Node = withCommentsFrom(n, arg) {
    val rv = new ObjectCreationExpr()
    n.getAnonymousClassBody.asScala.foreach { body =>
      rv.setAnonymousClassBody(filter(body, arg))
    }
    
    rv.setArguments(filter(n.getArguments, arg))
    filter(n.getScope, arg).foreach{ rv.setScope }
    rv.setType(filter(n.getType, arg))

    n.getTypeArguments.asScala.foreach { args =>
      rv.setTypeArguments(filter(args, arg))
    }
    
    rv
  }

  override def visit(n: PackageDeclaration, arg: A): Node = withCommentsFrom(n, arg) {
    val rv = new PackageDeclaration()
    rv.setAnnotations(filter(n.getAnnotations, arg))
    rv.setName(filter(n.getName, arg))
    rv    
  }
  
  override def visit(n: Parameter, arg: A): Node = withCommentsFrom(n, arg) {
    val rv = new Parameter()
    rv.setType(filter(n.getType, arg))
    rv.setVarArgs(n.isVarArgs)
    rv
  }

  override def visit(n: UnionType, arg: A): Node = withCommentsFrom(n, arg) {
    new UnionType(filter(n.getElements, arg))
  }

  override def visit(n: PrimitiveType, arg: A): Node = new PrimitiveType(n.getType)      

  override def visit(n: Name, arg: A): Node = withCommentsFrom(n, arg) {
    if (n.getQualifier.isPresent) new Name(n.getQualifier.get, n.getIdentifier)
    else new Name(n.getIdentifier)
  }

  override def visit(n: ReturnStmt, arg: A): Node = withCommentsFrom(n, arg) {
    n.getExpression.asScala.map{ expr =>
      new ReturnStmt(filter(expr, arg))
    }.getOrElse{ new ReturnStmt() }
  }

  override def visit(n: SingleMemberAnnotationExpr, arg: A): Node = withCommentsFrom(n, arg) {
    new SingleMemberAnnotationExpr(filter(n.getName, arg), filter(n.getMemberValue, arg))
  }

  override def visit(n: StringLiteralExpr, arg: A): Node = new StringLiteralExpr(n.getValue)

  override def visit(n: SuperExpr, arg: A): Node = withCommentsFrom(n, arg) {
    filter(n.getTypeName, arg).map{ nme => new SuperExpr(nme) }.getOrElse{ new SuperExpr() }
  }

  override def visit(n: SwitchEntry, arg: A): Node = withCommentsFrom(n, arg) {
    val rv = new SwitchEntry()
    rv.setLabels(filter(n.getLabels, arg))
    rv.setStatements(filter(n.getStatements, arg))
    rv
  }

  override def visit(n: SwitchStmt, arg: A): Node = withCommentsFrom(n, arg) {
    val rv = new SwitchStmt()
    rv.setSelector(filter(n.getSelector, arg))
    rv.setEntries(filter(n.getEntries, arg))
    rv
  }

  override def visit(n: SynchronizedStmt, arg: A): Node = withCommentsFrom(n, arg) {
    val rv = new SynchronizedStmt() 
    rv.setExpression(filter(n.getExpression, arg))
    rv.setBody(filter(n.getBody, arg))
    rv
  }

  override def visit(n: ThisExpr, arg: A): Node = withCommentsFrom(n, arg) {
    filter(n.getTypeName, arg).map{ nme => new ThisExpr(nme) }.getOrElse{ new ThisExpr() }
  }

  override def visit(n: ThrowStmt, arg: A): Node = withCommentsFrom(n, arg) {
    new ThrowStmt(filter(n.getExpression, arg))
  }

  override def visit(n: TryStmt, arg: A): Node = withCommentsFrom(n, arg) {
    val rv = new TryStmt()
    rv.setResources(filter(n.getResources, arg))
    rv.setTryBlock(filter(n.getTryBlock, arg))
    rv.setCatchClauses(filter(n.getCatchClauses, arg))
    filter(n.getFinallyBlock, arg).foreach{ rv.setFinallyBlock }
    rv
  }

  override def visit(n: TypeParameter, arg: A): Node = withCommentsFrom(n, arg) {
    new TypeParameter(n.getNameAsString, filter(n.getTypeBound, arg))
  }

  override def visit(n: UnaryExpr, arg: A): Node = withCommentsFrom(n, arg) {
    new UnaryExpr(filter(n.getExpression, arg), n.getOperator)    
  }

  override def visit(n: VariableDeclarationExpr, arg: A): Node = withCommentsFrom(n, arg) {
    val rv = new VariableDeclarationExpr()
    rv.setAnnotations(filter(n.getAnnotations, arg))
    rv.setModifiers(n.getModifiers)
    rv.setVariables(filter(n.getVariables, arg))
    rv
  }

  override def visit(n: VariableDeclarator, arg: A): Node = withCommentsFrom(n, arg) {
    val rv = new VariableDeclarator(n.getType, filter(n.getName, arg))
    filter(n.getInitializer, arg).foreach{ rv.setInitializer }
    rv
  }

  override def visit(n: VoidType, arg: A): Node = new VoidType()

  override def visit(n: WhileStmt, arg: A): Node = withCommentsFrom(n, arg) {
    val rv = new WhileStmt()
    rv.setCondition(filter(n.getCondition, arg))
    rv.setBody(filter(n.getBody, arg))
    rv
  }

  override def visit(n: WildcardType, arg: A): Node = withCommentsFrom(n, arg) {
    val rv = new WildcardType()
    filter(n.getExtendedType, arg).foreach{ rv.setExtendedType }
    filter(n.getSuperType, arg).foreach{ rv.setSuperType }
    rv
  }

  override def visit(n: BlockComment, arg: A): Node = new BlockComment(n.getContent)

  override def visit(n: LineComment, arg: A): Node = new LineComment(n.getContent)

  override def visit(n: TypeExpr, arg: A): Node = new TypeExpr(filter(n.getType, arg))

  override def visit(n: MethodReferenceExpr, arg: A): Node = new MethodReferenceExpr(
    filter(n.getScope, arg),
    n.getTypeArguments.asScala.getOrElse(NodeList.nodeList()),
    visitName(n.getIdentifier, arg)
  )

  override def visit(n: LambdaExpr, arg: A): Node =
    new LambdaExpr(
      filter(n.getParameters, arg),
      filter(n.getBody, arg),
      n.isEnclosingParameters
    )

  override def visit(n: UnknownType, arg: A): Node = new UnknownType()

  override def visit(n: IntersectionType, arg: A): Node = new IntersectionType(filter(n.getElements, arg))

  // New Methods in 3.x
  override def visit(n: TextBlockLiteralExpr, arg: A): Node = new TextBlockLiteralExpr(n.asString)
  override def visit(n: YieldStmt, arg: A): Node = new YieldStmt(filter(n.getExpression, arg))
  override def visit(n: SwitchExpr, arg: A): Node = new SwitchExpr(filter(n.getSelector, arg), filter(n.getEntries, arg))
  override def visit(n: Modifier, arg: A): Node = new Modifier(n.getKeyword)
  override def visit(n: VarType, arg: A): Node = new VarType()
  override def visit(n: ReceiverParameter, arg: A): Node = new ReceiverParameter(filter(n.getAnnotations, arg), filter(n.getType, arg), filter(n.getName, arg))
  override def visit(n: UnparsableStmt, arg: A): Node = ???
  override def visit(n: ModuleOpensDirective, arg: A): Node = ???
  override def visit(n: ModuleUsesDirective, arg: A): Node = ???
  override def visit(n: ModuleProvidesDirective, arg: A): Node = ???
  override def visit(n: ModuleExportsDirective, arg: A): Node = ???
  override def visit(n: ModuleRequiresDirective, arg: A): Node = ???
  override def visit(n: ModuleDeclaration, arg: A): Node = ???

  override def visit(n: SimpleName, arg: A): Node = new SimpleName(n.getIdentifier)

  override def visit(n: NodeList[_ <: Node], arg: A): Node = ???
  override def visit(n: LocalClassDeclarationStmt, arg: A): Node = ???
  override def visit(n: ArrayCreationLevel, arg: A): Node = ???


  override def visit(n: ArrayType, arg: A): Node = {
    new ArrayType(n.getElementType)
  }
}
