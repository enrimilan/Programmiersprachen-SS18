package at.ac.tuwien.ps.operator;

import java.util.Stack;

import at.ac.tuwien.ps.Context;
import at.ac.tuwien.ps.element.Element;
import at.ac.tuwien.ps.element.ElementType;

/**
 * Checks if the top element on the data stack is an integer (without removing an element) and pushes a corresponding
 * Boolean value (0 or 1) onto the stack.
 */
public class IntegerCheckOperator implements Operator {

    @Override
    public void execute(Context context) {
    	Stack<Element> stack = context.getDataStack();
    	Element topElement = stack.peek();
    	if(topElement.getElementType() == ElementType.INTEGER){
    		stack.push(new Element(String.valueOf(1), ElementType.INTEGER));
    	}
    	else{
    		stack.push(new Element(String.valueOf(0), ElementType.INTEGER));
    	}
    		
    }

}