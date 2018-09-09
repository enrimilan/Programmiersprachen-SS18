package at.ac.tuwien.ps.operator;


import at.ac.tuwien.ps.Context;

/**
 * An operator receives the current context and modifies it accordingly.
 */
public interface Operator {

	/**
	 * Receives current context and applies the operator on it.
	 * @param context
	 */
    void execute(Context context);

}
