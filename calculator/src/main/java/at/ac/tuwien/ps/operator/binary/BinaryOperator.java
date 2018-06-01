package at.ac.tuwien.ps.operator.binary;

import java.util.Stack;

import at.ac.tuwien.ps.element.Element;
//import at.ac.tuwien.ps.element.ElementType;
import at.ac.tuwien.ps.Context;
import at.ac.tuwien.ps.Pair;
import at.ac.tuwien.ps.operator.IntegerCheckOperator;
import at.ac.tuwien.ps.operator.Operator;
//import at.ac.tuwien.ps.operator.StackSizeOperator;
//import at.ac.tuwien.ps.operator.OperatorException;
import at.ac.tuwien.ps.operator.OperatorException;

/**
 * Takes two Elements from the stack and checks if they are integers.s
 * An error is reported if one of the two topmost elements on the data stack is not an integer.
 */


public abstract class BinaryOperator implements Operator {

	public Pair<Integer,Integer> checkForIntegers(Context context){
		Stack<Element> stack = context.getDataStack();

		if(stack.size() < 2)
			throw new OperatorException("Error at " + this.getClass().getSimpleName() + " -> Stack needs to contain at least 2 elements but has " + stack.size());

		
//		//check if stack size >= 2
//		StackSizeOperator sizeInt = new StackSizeOperator();
//		sizeInt.execute(context);
//		Element check = stack.pop();
//		if(check.parseToInt() < 2)
//			throw new OperatorException("Error at " + this.getClass().getSimpleName() + " -> Stack size < 2 and binary operation.");
//		
		//check for integers
		IntegerCheckOperator checkInt = new IntegerCheckOperator();
    	
		checkInt.execute(context);        
        Element check = stack.pop();
        if(check.parseToInt() == 0)
        	throw new OperatorException("Error at " + this.getClass().getSimpleName() + " -> First element is not an integer.");
        Element firstElement = stack.pop();
        
        checkInt.execute(context);
        check = stack.pop();
        if(check.parseToInt() == 0)
        	throw new OperatorException("Error at " + this.getClass().getSimpleName() + " -> Second element is not an integer.");
	     Element secondElement = stack.pop();
	     
	     return new Pair<Integer,Integer> (firstElement.parseToInt(),secondElement.parseToInt());
	}
	public abstract void execute(Context context);

}
