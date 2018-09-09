package at.ac.tuwien.ps;

import at.ac.tuwien.ps.element.Element;
import at.ac.tuwien.ps.register.Register;

import java.util.List;
import java.util.Stack;

public class Context {

    private CommandStream commandStream;
    private Stack<Element> dataStack;
    private List<Register> registers;

    public Context(CommandStream commandStream, Stack<Element> dataStack, List<Register> registers) {
        this.commandStream = commandStream;
        this.dataStack = dataStack;
        this.registers = registers;
    }

    public Stack<Element> getDataStack() {
        return dataStack;
    }

    public CommandStream getCommandStream() {
        return commandStream;
    }

    public List<Register> getRegisters() {
        return registers;
    }

}
