package at.ac.tuwien.ps.operator.binary;

import java.util.Stack;

import at.ac.tuwien.ps.Context;
import at.ac.tuwien.ps.Pair;
import at.ac.tuwien.ps.element.Element;
import at.ac.tuwien.ps.element.ElementType;
import at.ac.tuwien.ps.operator.OperatorException;

/**
 * An error is reported if one of the two topmost elements on the data stack
 * is not an integer as well as if an argument differs from 0 and 1
 */
public class LogicalAndOperator extends LogicalBinaryOperator {

    @Override
    public void execute(Context context) {
    	Pair<Integer,Integer> pair = checkForBooleans(context);
    	Stack<Element> stack = context.getDataStack();
    	String result = ((pair.x+pair.y)==2) ? "1" : "0";
    	stack.push(new Element(result, ElementType.INTEGER));
    }

}