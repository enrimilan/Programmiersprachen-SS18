package at.ac.tuwien.ps.operator;

import java.util.Stack;

import at.ac.tuwien.ps.Context;
import at.ac.tuwien.ps.element.Element;
import at.ac.tuwien.ps.element.ElementType;

/**
 * Checks if the top element on the data stack is a nonempty list (without removing an element) and
 * pushes a corresponding Boolean value (0 or 1) onto the stack.
 */
public class NonEmptyListCheckOperator implements Operator {

    @Override
    public void execute(Context context) {
    	Stack<Element> stack = context.getDataStack();
		if(stack.size() < 1)
			throw new OperatorException("Error at " + this.getClass().getSimpleName() + " -> Stack needs to contain at least 1 element but has " + stack.size());

    	Element top = stack.peek();
    	if(top.getElementType() == ElementType.LIST){
    		String list = top.getValue();
    		if(list.length() == 2 && list.charAt(0) == '(' && list.charAt(1) == ')') {
				stack.push(new Element(String.valueOf(0), ElementType.INTEGER));
			}
			else {
				stack.push(new Element(String.valueOf(1), ElementType.INTEGER));
			}
    		return;
    	}
    	stack.push(new Element(String.valueOf(0), ElementType.INTEGER));
    }

}