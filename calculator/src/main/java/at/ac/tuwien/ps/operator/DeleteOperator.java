package at.ac.tuwien.ps.operator;

import at.ac.tuwien.ps.Context;
import at.ac.tuwien.ps.element.Element;

import java.util.Stack;

/**
 * Takes the top element n from the data stack and removes the nth element from the data stack (counted from the top of
 * the stack). An error is reported if n is not a positive number.
 */
public class DeleteOperator implements Operator {

    @Override
    public void execute(Context context) {
    	IntegerCheckOperator checkInt = new IntegerCheckOperator();
    	checkInt.execute(context);
        Stack<Element> stack = context.getDataStack();
        Element check = stack.pop();
        if(check.parseToInt() == 0)
        	throw new OperatorException("Error at " + this.getClass().getSimpleName() + " -> n is not an integer.");
        
        Element top = stack.peek();
        int topValue = top.parseToInt();
        
        if(topValue < 0)
        	throw new OperatorException("Error at " + this.getClass().getSimpleName() + " -> n is not a positive integer.");
        
        int index = stack.size() - topValue - 1;
        stack.remove(index);
        stack.pop();
    }
}
