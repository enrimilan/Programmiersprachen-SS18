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
    	Element top = stack.peek();
    	if(top.getElementType() == ElementType.LIST){
    		String data = top.getValue();
    		for(int i=0; i<data.length(); i++) {
                if((data.charAt(i) != '(') && (data.charAt(i) != ')')) {
                	stack.push(new Element(String.valueOf(1), ElementType.INTEGER));
                }
    		}
    		return;
    	}
    	stack.push(new Element(String.valueOf(0), ElementType.INTEGER));
    }

}