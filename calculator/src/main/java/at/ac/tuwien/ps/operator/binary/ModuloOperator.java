package at.ac.tuwien.ps.operator.binary;

import java.util.Stack;

import at.ac.tuwien.ps.Context;
import at.ac.tuwien.ps.Pair;
import at.ac.tuwien.ps.element.Element;
import at.ac.tuwien.ps.element.ElementType;
import at.ac.tuwien.ps.operator.OperatorException;

/**
 * An error shall be reported if the top element on the data stack equals 0.
 */
public class ModuloOperator extends BinaryOperator {

    @Override
    public void execute(Context context) {
    	Pair<Integer,Integer> pair = checkForIntegers(context);
    	Stack<Element> stack = context.getDataStack();
    	if(pair.y == 0)
    		throw new OperatorException("Error at " + this.getClass().getSimpleName() + " -> Second argument is 0");
    	int result = pair.x / pair.y;
    	stack.push(new Element(String.valueOf(result), ElementType.INTEGER));
    }

}