package at.ac.tuwien.ps.operator.binary;

import at.ac.tuwien.ps.Context;
import at.ac.tuwien.ps.Pair;
import at.ac.tuwien.ps.element.Element;
import at.ac.tuwien.ps.element.ElementType;
import at.ac.tuwien.ps.operator.OperatorException;

import java.util.Stack;

public class SubtractionOperator extends BinaryOperator {

	
    @Override
    public void execute(Context context) {
    	Pair<Integer,Integer> pair = checkForIntegers(context);
    	Stack<Element> stack = context.getDataStack();
    	int result = pair.x-pair.y;
    	stack.push(new Element(String.valueOf(result), ElementType.INTEGER));
    }

}