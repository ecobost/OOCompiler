package toolc
package analyzer

import utils._
import ast.Trees._
import Symbols._
import toolc.analyzer.Types._

object NameAnalysis extends Pipeline[Program, Program] {

  def run(ctx: Context)(prog: Program): Program = {
    import ctx.reporter._

    def collectSymbolsAndType: GlobalScope = {

      // Generates symbols and types for all declared variables, 
      // Returns a global scope with all classes and its appropiate methods and variables
      def symbolizeProgram(program: Program): GlobalScope = {

        val globalScope: GlobalScope = new GlobalScope()

        //Check mostly problems with repeated names. 
        checkNameConsistency(program.classes, program.main)
        

        // Symbolize main object
        globalScope.mainClass = symbolizeMainClass(program.main)

        // Symbolize classes
        globalScope.classes = globalScope.classes ++ program.classes.map(symbolizeClass)
        
        // Assign types to the main symbol and class symbols
        typeClass(program.main.getSymbol)
        globalScope.classes.foreach(x => typeClass(x._2))
        
        // Assign types to the interior of classes
        program.classes.foreach(typeInteriorOfClass(_,globalScope))

       
        // Handle inheritance
        // Set inheritance to class symbols.
        program.classes.foreach(x => setInheritance(x, globalScope))
        // Check for cycles in inheritance. Class A cannot extend A
        globalScope.classes.foreach(x => checkInheritanceConsistency(x._2))
        terminateIfErrors // This is needed to avoid loops in the execution of the other parts of the program.
        // Check overriding in variables(not allowed)
        globalScope.classes.foreach(x => checkVarOverriding(x._2))
        
        // Handle overriding
        // Set overriding in methods and check restraints
        globalScope.classes.foreach(x => setMethodOverriding(x._2))
        
        // Check overloading  on return type (not allowed)
        globalScope.classes.foreach(x=> checkOverloading(x._2))

        globalScope
      }

      // Checks problems with the name consistency throughout the program.
      // Checks for duplicate names in classes, methods, arguments and variables.
      // Also checks for shadowed arguments
      def checkNameConsistency(classes: List[ClassDecl], main: MainObject) {
        
        def checkClassNameConsistency(_class: ClassDecl, objectName: String){
         // Check if _class has the same name as the object class
          if (_class.id.value == main.id.value) {
        		error("Object name used as class name", _class)
          }
          
          // Check for variables with the same name in the same class
          val varNames: List[String] = _class.vars.map(x => x.id.value)
          _class.vars.foreach(x => checkDuplicateVar(x, varNames))

          // Check for methods in the same class with the same name and argument list (not allowed by overloading)
          val methodDecs: List[MethodDecl] = _class.methods
          _class.methods.foreach(x => checkDuplicateMethod(x, methodDecs))
          
          // Check method name consistency
          _class.methods.foreach(checkMethodNameConsistency)          
        }

       def checkMethodNameConsistency(method: MethodDecl) {
          // Check for arguments with the same name
          val argNames: List[String] = method.args.map(x => x.id.value)
          method.args.foreach(x => checkDuplicateArg(x, argNames))

          // Check for variables with the same name
          val varNames: List[String] = method.vars.map(x => x.id.value)
          method.vars.foreach(x => checkDuplicateVar(x, varNames))

          // Check for shadowed arguments
          method.vars.foreach(x => checkForShadowedArg(x, argNames))
        }
       
        def checkDuplicateClass(_class: ClassDecl, classNames: List[String]) {
          if (classNames.count(_ == _class.id.value) > 1) { // Duplicate class
            error("Duplicated class " + _class.id.value, _class)
          }
        }
       
        // To assert that in the same class all overloading rules are allowed, it is
        // if there is more than one method foo, there cannot be two with the same argument list
        def checkDuplicateMethod(method: MethodDecl, methodDecs: List[MethodDecl]) {
          val sameNameMethods: List[MethodDecl] = methodDecs.diff(List(method)).filter(x => (x.id.value == method.id.value))
          
          sameNameMethods.foreach(x => {
            if(areArgumentListTheSame(x.args, method.args))
              error("Duplicated methods " + method.id.value + " or trying to overload only on return type" , method)
            }) 
        }
        
      def areArgumentListTheSame(list1: List[Formal], list2: List[Formal]): Boolean = {
        if (list1.isEmpty && list2.isEmpty) true
        else if (list1.isEmpty || list2.isEmpty) false
        else{
	        val typesAreDifferent: Boolean = list1.head.tpe match {
	          case IntArrayType() => list2.head.tpe != IntArrayType()
	          case IntType() => list2.head.tpe != IntType()
	          case BooleanType() => list2.head.tpe != BooleanType()
	          case StringType() => list2.head.tpe != StringType()
	          case t1:Identifier => list2.head.tpe match {
	            case t2: Identifier => t1.value != t2.value 
	            case _ => true
	          }
	        }
	        if (typesAreDifferent) false
	        else areArgumentListTheSame(list1.tail, list2.tail)
        }
      }

       def checkDuplicateVar(variable: VarDecl, varNames: List[String]) {
          if (varNames.count(_ == variable.id.value) > 1) { // Duplicate variable
            error("Duplicated variable " + variable.id.value, variable)
          }
        }
                
        def checkDuplicateArg(argument: Formal, argNames: List[String]) {
          if (argNames.count(_ == argument.id.value) > 1) { // Duplicate variable
            error("Duplicated argument " + argument.id.value, argument)
          }
        }

        def checkForShadowedArg(variable: VarDecl, argNames: List[String]) {
          if (argNames.contains(variable.id.value)) {
            error("Variable " + variable.id.value + " shadows a method argument", variable)
          }
        }

        // Running point for checkNameConsistency
        // Check classes with duplicate names
        val classNames: List[String] = classes.map(x => x.id.value)
        classes.foreach(x => checkDuplicateClass(x, classNames))
        
        // Check class consistency
        classes.foreach(x => checkClassNameConsistency(x, main.id.value))
      }

      def symbolizeMainClass(main: MainObject): ClassSymbol = {
        val mainSymbol: ClassSymbol = new ClassSymbol(main.id.value)

        mainSymbol.setPos(main)
        main.setSymbol(mainSymbol)
        mainSymbol
      }

      def symbolizeClass(_class: ClassDecl): (String, ClassSymbol) = {
        val classSymbol: ClassSymbol = new ClassSymbol(_class.id.value)

        classSymbol.methods = classSymbol.methods ++ _class.methods.map(x => symbolizeMethod(x, classSymbol))
        classSymbol.members = classSymbol.members ++ _class.vars.map(symbolizeVariable)

        classSymbol.setPos(_class)
        _class.setSymbol(classSymbol)
        (_class.id.value, classSymbol)
      }

      def symbolizeMethod(method: MethodDecl, classSymbol: ClassSymbol): (String, MethodSymbol) = {
        val methodSymbol: MethodSymbol = new MethodSymbol(method.id.value, classSymbol)

        val methodSymbolArgList: List[(String, VariableSymbol)] = method.args.map(symbolizeFormal)
        val methodSymbolVarList: List[(String, VariableSymbol)] = method.vars.map(symbolizeVariable)
        methodSymbol.params = methodSymbol.params ++ methodSymbolArgList
        methodSymbol.members = methodSymbol.members ++ methodSymbolVarList
        methodSymbol.argList = methodSymbol.argList ++ methodSymbolArgList.map(x => x._2)

        methodSymbol.setPos(method)
        method.setSymbol(methodSymbol)
        (method.id.value, methodSymbol)
      }

      def symbolizeFormal(formal: Formal): (String, VariableSymbol) = {
        val formalSymbol = new VariableSymbol(formal.id.value)

        formalSymbol.setPos(formal)
        formal.setSymbol(formalSymbol)
        (formal.id.value, formalSymbol)
      }

      def symbolizeVariable(variable: VarDecl): (String, VariableSymbol) = {
        val variableSymbol = new VariableSymbol(variable.id.value)

        variableSymbol.setPos(variable)
        variable.setSymbol(variableSymbol)
        (variable.id.value, variableSymbol)
      }

      
      
      
      def typeClass(classSymbol: ClassSymbol) {
        // Assign type to class symbol
        classSymbol.setType(TObject(classSymbol))
      }
      
      // We need to do this after the typeClassSymbol, because we will need all classes typed 
      // for the TypeTreeToType method 
      def typeInteriorOfClass(_class: ClassDecl, globalScope: GlobalScope){
        _class.vars.foreach(typeVar(_, globalScope))
        _class.methods.foreach(typeMethod(_, globalScope))
      }

      def typeVar(variable: VarDecl, globalScope: GlobalScope) {
        variable.getSymbol.setType(TypeTreeToType(variable.tpe, globalScope))
      }

      def typeMethod(method: MethodDecl, globalScope: GlobalScope) {
        // A methodDecl symbol has the method return type assigned to it
        method.getSymbol.setType(TypeTreeToType(method.retType, globalScope)) 
        method.args.foreach(typeFormal(_, globalScope))
        method.vars.foreach(typeVar(_, globalScope))
      }

      def typeFormal(formal: Formal, globalScope: GlobalScope) {
        formal.getSymbol.setType(TypeTreeToType(formal.tpe, globalScope))
      }
      
      // Takes a typeTree variable and transforms it to type.
      // Identifiers can only mean classes, though a class is looked up in the globalScope  
      def TypeTreeToType(tpe: TypeTree, globalScope: GlobalScope): Type = tpe match {
        case IntArrayType() => TIntArray
        case IntType() => TInt
        case BooleanType() => TBoolean
        case StringType() => TString
        case id: Identifier =>
          val classSymbol = globalScope.lookupClass(id.value)
          if (classSymbol.isDefined) {
            classSymbol.get.getType
          } else {
            error("Class " + id.value + " does not exist or is object class", id)
            TError
          }
      }
      
      
      

      def setInheritance(_class: ClassDecl, globalScope: GlobalScope) {
        if (_class.parent.isDefined) {
          val inheritingClassSymbol: Option[ClassSymbol] = globalScope.lookupClass(_class.parent.get.value)

          // Check that class to be inherited from exists
          if (inheritingClassSymbol.isDefined) {
            _class.getSymbol.parent = inheritingClassSymbol
          } else {
            error("Class " + _class.parent.get.value + " does not exist or is object class", _class)
          }
        }
      }

      def checkInheritanceConsistency(classSymbol: ClassSymbol) {
      
        def isThereACycle(parentClass: ClassSymbol, listOfInheritance: List[ClassSymbol]): Boolean = {
	        if (listOfInheritance.contains(parentClass)) {
	          true
	        } else {
	          if (parentClass.parent.isEmpty) { // End of check: No cycles
	            false
	          } else {
	            isThereACycle(parentClass.parent.get, parentClass :: listOfInheritance)
	          }
	        }
	      }
        
          // Check for loops in inheritance
          if (classSymbol.parent.isDefined) {
          if (isThereACycle(classSymbol.parent.get, List(classSymbol))) {
            error("Inheritance cycle in declaration of class " + classSymbol.name, classSymbol)
          }
        }
    
      }
      
     def checkVarOverriding(classSymbol: ClassSymbol) {
        if (classSymbol.parent.isDefined) {
          classSymbol.members.foreach(x => checkForOverridingVar(x._2, classSymbol.parent.get.lookupVar(x._2.name)))
        }
        
        def checkForOverridingVar(overridingVar: VariableSymbol, overridedVar: Option[VariableSymbol]) {
		    if (overridedVar.isDefined) { // Variables with the same name in classes with inheritance
		      error("Override of class fields is not allowed at " + overridingVar.name + ". Trying to overload (" + overridedVar.get.position + ")", overridingVar)
		    }
		  }
      }

      // We are taking advantage here of the fact that lookupMethods gives us the methods of the closest class first, 
     // and then goes to the parent class to get more methods. Consider the case where one method overrides of another overriding method
     // in this case is important this ordering, because before setting the overriding method llokupMethods will give us both methods in the same list
      def setMethodOverriding(classSymbol: ClassSymbol) {
        if (classSymbol.parent.isDefined) {
          // Implicit comparison of names when we lookup for a method called like the first one.
          classSymbol.methods.foreach(x => setOverride(x._2, classSymbol.parent.get.lookupMethods(x._2.name))) 
        }
      }

      // Set the method overriding to None or to the method who it is overriden.
      def setOverride(method: MethodSymbol, methList: List[MethodSymbol]) {
        var isOverridingAssigned: Boolean = false;
        var methodList = methList;
        while(!isOverridingAssigned && !methodList.isEmpty){
          val method2: MethodSymbol = methodList.head
          // Check list of arguments is the same in both methods (types)
          if(areTypesEqual(method.getType, method2.getType) && areArgTypesEqual(method.argList, method2.argList)){
            // Assign overriding
            method.overridden = Some(method2)
            isOverridingAssigned = true
          }
          methodList = methodList.tail
        }
        // Assign none if overiding was not assigned
        if(!isOverridingAssigned){
          method.overridden = None
        }
      }

      // Checks two lists and returns true if the types of the two are the same in the same order
      def areArgTypesEqual(list1: List[VariableSymbol], list2: List[VariableSymbol]): Boolean = {
        if (list1.isEmpty && list2.isEmpty) true
        else if (list1.isEmpty || list2.isEmpty) false
        else if (! areTypesEqual(list1.head.getType, list2.head.getType)) false
        else areArgTypesEqual(list1.tail, list2.tail)
      }
      
      // compare to objects types
      def areTypesEqual(t1: Type, t2: Type): Boolean = {
  		t1 match {
  		  case TObject(class1) => t2 match {
  		    case TObject(class2) => class1 == class2
  		    case _ => false
  		  }
  		  case _ => t1 == t2
  		}
	}
      
      // TODO: Check overloading. We need to do it after all overridings are set
      
       def checkOverloading(classSymbol: ClassSymbol) {
        if (classSymbol.parent.isDefined) {
          // Implicit comparison of names when we lookup for a method called like the first one.
          classSymbol.methods.foreach(x => checkOverloadingConstraints(x._2, classSymbol.lookupMethods(x._2.name))) 
        }
      }

      // Set the method overriding to None or to the method who it is overriden.
      def checkOverloadingConstraints(method: MethodSymbol, methList: List[MethodSymbol]) {
        var methodList = methList.diff(List(method));
        methodList.foreach( method2 =>{
          if(areArgTypesEqual(method.argList, method2.argList)){
            error("Trying to overload on return type: Function " + method.name  +" in class " + method.classSymbol + " and " + method2.classSymbol, method)
          }
        })
      }

      // Run line for collectSymbols, returns a globalScope
      symbolizeProgram(prog)
    }

    def assignIdentifiers(globalScope: GlobalScope) {
      def assignProgram(program: Program, globalScope: GlobalScope) {
        assignMainObject(program.main)
        program.classes.foreach(assignClass)
      }

      def assignClass(_class: ClassDecl) {
        _class.id.setSymbol(_class.getSymbol)
        if (_class.parent.isDefined) {
          _class.parent.get.setSymbol(_class.getSymbol.parent.get)
        }
        _class.vars.foreach(assignVarDecl)
        _class.methods.foreach(assignMethodDecl)
      }

      def assignMainObject(main: MainObject) {
        main.id.setSymbol(main.getSymbol)
        main.stats.foreach(x => assignStatTree(x, new MethodSymbol("main", main.getSymbol)))
      }

      def assignStatTree(statement: StatTree, methodSymbol: MethodSymbol) {
        statement match {
          case stat: Block =>
            stat.stats.foreach(x => assignStatTree(x, methodSymbol))
          case stat: If =>
            assignExprTree(stat.expr, methodSymbol)
            assignStatTree(stat.thn, methodSymbol)
            if (stat.els.isDefined) {
              assignStatTree(stat.els.get, methodSymbol)
            }
          case stat: While =>
            assignExprTree(stat.expr, methodSymbol)
            assignStatTree(stat.stat, methodSymbol)
          case stat: Println =>
            assignExprTree(stat.expr, methodSymbol)
          case stat: Assign =>
            assignIdentifierAsVar(stat.id, methodSymbol)
            assignExprTree(stat.expr, methodSymbol)
          case stat: ArrayAssign =>
            assignIdentifierAsVar(stat.id, methodSymbol)
            assignExprTree(stat.index, methodSymbol)
            assignExprTree(stat.expr, methodSymbol)
        }
      }

      def assignIdentifierAsVar(identifier: Identifier, methodSymbol: MethodSymbol) {
        val identifierSymbol = methodSymbol.lookupVar(identifier.value)
        if (identifierSymbol.isDefined) {
          identifier.setSymbol(identifierSymbol.get)
        } else {
          error("Variable " + identifier.value + " not declared in this scope", identifier)
        }
      }

      def assignVarDecl(variable: VarDecl) {
        variable.id.setSymbol(variable.getSymbol)
        assignTypeTree(variable.tpe)
      }

      def assignTypeTree(typeTree: TypeTree) {
        typeTree match {
          case typ: Identifier => assignClassType(typ)
          case _ => // Do not need any assignment
        }
      }

      def assignClassType(identifier: Identifier) {
        val classSymbol = globalScope.lookupClass(identifier.value)
        if (classSymbol.isDefined) {
          identifier.setSymbol(classSymbol.get)
        } else {
          error("Class " + identifier.value + " does not exist or is object class", identifier)
        }
      }

      def assignMethodDecl(method: MethodDecl) {
        assignTypeTree(method.retType)
        method.id.setSymbol(method.getSymbol)
        method.args.foreach(assignFormal)
        method.vars.foreach(assignVarDecl)
        method.stats.foreach(x => assignStatTree(x, method.getSymbol))
        assignExprTree(method.retExpr, method.getSymbol)
      }

      def assignFormal(formal: Formal) {
        formal.id.setSymbol(formal.getSymbol)
        assignTypeTree(formal.tpe)
      }

      def assignExprTree(exprTree: ExprTree, methodSymbol: MethodSymbol) {
        exprTree match {
          case expr: And =>
            assignExprTree(expr.lhs, methodSymbol)
            assignExprTree(expr.rhs, methodSymbol)
          case expr: Or =>
            assignExprTree(expr.lhs, methodSymbol)
            assignExprTree(expr.rhs, methodSymbol)
          case expr: Equals =>
            assignExprTree(expr.lhs, methodSymbol)
            assignExprTree(expr.rhs, methodSymbol)
          case expr: LessThan =>
            assignExprTree(expr.lhs, methodSymbol)
            assignExprTree(expr.rhs, methodSymbol)
          case expr: Plus =>
            assignExprTree(expr.lhs, methodSymbol)
            assignExprTree(expr.rhs, methodSymbol)
          case expr: Minus =>
            assignExprTree(expr.lhs, methodSymbol)
            assignExprTree(expr.rhs, methodSymbol)
          case expr: Times =>
            assignExprTree(expr.lhs, methodSymbol)
            assignExprTree(expr.rhs, methodSymbol)
          case expr: Div =>
            assignExprTree(expr.lhs, methodSymbol)
            assignExprTree(expr.rhs, methodSymbol)
          case expr: ArrayRead =>
            assignExprTree(expr.arr, methodSymbol)
            assignExprTree(expr.index, methodSymbol)
          case expr: ArrayLength =>
            assignExprTree(expr.arr, methodSymbol)
          case expr: MethodCall =>
            assignExprTree(expr.obj, methodSymbol)
            // Change this line. Now assigned to the class where this line is being executed(not the class where the method belongs to)
            expr.meth.setSymbol(methodSymbol.classSymbol)
            expr.args.foreach(x => assignExprTree(x, methodSymbol))
          case expr: Identifier =>
            assignIdentifierAsVar(expr, methodSymbol)
          case expr: This =>
            expr.setSymbol(methodSymbol.classSymbol)
          case expr: NewIntArray =>
            assignExprTree(expr.size, methodSymbol)
          case expr: New =>
            assignClassType(expr.tpe)
          case expr: Not =>
            assignExprTree(expr.expr, methodSymbol)
          case _ => // Do not need any assigning
        }
      }

      assignProgram(prog, globalScope)
    }

    // Step 1: Collect symbols in declarations
    val global: GlobalScope = collectSymbolsAndType
    terminateIfErrors
    // Step 2: Attach symbols to identifiers (except method calls) in method bodies
    assignIdentifiers(global)
    terminateIfErrors
    // (Step 3:) Print tree with symbol ids for debugging

    prog
  }
}
