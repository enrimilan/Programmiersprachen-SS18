package at.ac.tuwien.ps.operator;

import at.ac.tuwien.ps.Context;

/**
 * An operator receives the current context and modifies it accordingly.
 */
public interface Operator {

    void execute(Context context);

}
