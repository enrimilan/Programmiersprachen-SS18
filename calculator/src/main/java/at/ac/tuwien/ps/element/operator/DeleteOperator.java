package at.ac.tuwien.ps.element.operator;

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
        Stack<Element> stack = context.getDataStack();
        if(stack.size() < 2)
            throw new OperatorException("Error at " + this.getClass().getSimpleName() + " -> Stack needs to contain at least 2 elements but has " + stack.size());

    	IntegerCheckOperator checkInt = new IntegerCheckOperator();
    	checkInt.execute(context);

        Element check = stack.pop();
        if(check.parseToInt() == 0)
        	throw new OperatorException("Error at " + this.getClass().getSimpleName() + " -> n is not an integer.");
        
        Element top = stack.peek();
        int topValue = top.parseToInt();
        
        if(topValue < 1)
        	throw new OperatorException("Error at " + this.getClass().getSimpleName() + " -> n is not a positive integer.");

        if(topValue >= stack.size())
            throw new OperatorException("Error at " + this.getClass().getSimpleName() + " -> index is out of range.");
        
        int index = stack.size() - topValue - 1;
        stack.remove(index);
        stack.pop();
    }
}
