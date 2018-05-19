package at.ac.tuwien.ps;

import at.ac.tuwien.ps.element.Element;
import at.ac.tuwien.ps.stream.CommandStream;

import java.util.Stack;

public class Context {

    private Stack<Element> dataStack = new Stack<>();
    private CommandStream commandStream;

    public Context(CommandStream commandStream) {
        this.commandStream = commandStream;
    }

    public Stack<Element> getDataStack() {
        return dataStack;
    }

    public CommandStream getCommandStream() {
        return commandStream;
    }
}
