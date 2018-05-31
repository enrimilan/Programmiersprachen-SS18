package at.ac.tuwien.ps.operator;

import java.util.List;
import java.util.Stack;

import at.ac.tuwien.ps.Context;
import at.ac.tuwien.ps.register.Register;
import at.ac.tuwien.ps.element.Element;

/**
 * Takes the top element n from the data stack and pushes the contents of register n onto the data stack. An error
 * is reported if n is not an integer between 0 and 31.
 */
public class ReadRegisterOperator implements Operator {

    @Override
    public void execute(Context context) {
    	Stack<Element> stack = context.getDataStack();
    	List<Register> registers = context.getRegisters();
    	
    	IntegerCheckOperator checkInt = new IntegerCheckOperator();
    	checkInt.execute(context);
        Element check = stack.pop();
        if(check.parseToInt() == 0)
        	throw new OperatorException("Error at " + this.getClass().getSimpleName() + " -> n is not an integer.");
        
        Element top = stack.pop();
        int topValue = top.parseToInt();

        if(topValue >= 0 && topValue <= 31) {
            Element element = registers.get(topValue).readElement();
            stack.push(element);
        }
        else{
        	throw new OperatorException("Error at " + this.getClass().getSimpleName() + " -> n is not between 0 and 31");
        }
    }

}