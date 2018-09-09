package at.ac.tuwien.ps.element.operator.binary;

import at.ac.tuwien.ps.Context;
import at.ac.tuwien.ps.element.Element;
import at.ac.tuwien.ps.element.ElementType;

import java.util.Stack;

/**
 * Takes two integers from the data stack and pushes its sum as an integer as result onto the data stack.
 * An error is reported if one of the two topmost elements on the data stack is not an integer.
 */

public class AdditionOperator extends BinaryOperator {

    @Override
    public void execute(Context context) {
    	Pair<Integer,Integer> pair = checkForIntegers(context);
    	Stack<Element> stack = context.getDataStack();
    	int result = pair.x + pair.y;
    	stack.push(new Element(String.valueOf(result), ElementType.INTEGER));  
    }

}