package at.ac.tuwien.ps.operator;

import java.util.Stack;

import at.ac.tuwien.ps.Context;
import at.ac.tuwien.ps.Register;
import at.ac.tuwien.ps.element.Element;
import at.ac.tuwien.ps.element.RegisterElement;

/**
 * Takes the top element n and the second elementc x from the data stack and writes x to register n. An error is
 * reported if n is not an integer between 0 and 31.
 */
public class WriteRegisterOperator implements Operator {

    @Override
    public void execute(Context context) {
    	Stack<Element> stack = context.getDataStack();
    	Register register = context.getRegister();
    	
    	IntegerCheckOperator checkInt = new IntegerCheckOperator();
    	checkInt.execute(context);
        Element check = stack.pop();
        if(check.parseToInt() == 0)
        	throw new OperatorException("Error at " + this.getClass().getSimpleName() + " -> n is not an integer.");
        
        Element top = stack.pop();
        Element x = stack.pop();
        int topValue = top.parseToInt();
        
        if(topValue >= 0 && topValue <= 31){
        	RegisterElement element = new RegisterElement(x);
        	register.writeRegister(topValue, element);
        }
        else{
        	throw new OperatorException("Error at " + this.getClass().getSimpleName() + " -> n is not between 0 and 31");
        }
    }

}