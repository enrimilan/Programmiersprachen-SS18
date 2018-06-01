package at.ac.tuwien.ps.operator.binary;

import java.util.Stack;

import at.ac.tuwien.ps.Context;
import at.ac.tuwien.ps.Pair;
import at.ac.tuwien.ps.element.Element;
import at.ac.tuwien.ps.element.ElementType;

/**
 * Takes two integers from the data stack and pushes the result of (first > second) onto the data stack.
 * An error is reported if one of the two topmost elements on the data stack is not an integer.
 */
public class GreaterThanOperator extends BinaryOperator {

    @Override
    public void execute(Context context) {
    	Pair<Integer,Integer> pair = checkForIntegers(context);
    	Stack<Element> stack = context.getDataStack();
    	String result = (pair.x > pair.y) ? "1" : "0";
    	stack.push(new Element(result, ElementType.INTEGER));
    }

}