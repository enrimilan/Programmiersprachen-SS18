package at.ac.tuwien.ps.operator.binary;

import at.ac.tuwien.ps.Context;
import at.ac.tuwien.ps.Pair;
import at.ac.tuwien.ps.operator.OperatorException;

/**
 * An error is reported if one of the two topmost elements on the data stack
 * is not an integer as well as if an argument differs from 0 and 1
 */

public abstract class LogicalBinaryOperator extends BinaryOperator {

	public static boolean checkForBooleanInt(int i){
		if(i==0 || i == 1)
			return true;
		else
			return false;
	}
	
	public Pair<Integer,Integer> checkForBooleans(Context context){
		Pair<Integer,Integer> pair = checkForIntegers(context);
		if(LogicalBinaryOperator.checkForBooleanInt(pair.x) && LogicalBinaryOperator.checkForBooleanInt(pair.y))
			return pair;
		else
			throw new OperatorException("Error at " + this.getClass().getSimpleName() + " -> At least one argument is no Boolean!");
	}
	
	public abstract void execute(Context context);

}
