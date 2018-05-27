package at.ac.tuwien.ps.operator;

import at.ac.tuwien.ps.Context;
import at.ac.tuwien.ps.element.Element;
import at.ac.tuwien.ps.element.ElementType;

import java.util.Stack;

/**
 * This unary operator changes the sign of its argument. An error is reported if the argument is not an integer.
 */
public class NegationOperator implements Operator {

    @Override
    public void execute(Context context) {
    	IntegerCheckOperator checkInt = new IntegerCheckOperator();
    	checkInt.execute(context);
        Stack<Element> stack = context.getDataStack();
        Element check = stack.pop();
        if(check.parseToInt() == 0)
        	throw new OperatorException("Error at " + this.getClass().getSimpleName() + " -> argument is not an integer.");
        
        Element top = stack.pop();
        int value = -top.parseToInt();
        stack.push(new Element(String.valueOf(value), ElementType.INTEGER));
    }

}