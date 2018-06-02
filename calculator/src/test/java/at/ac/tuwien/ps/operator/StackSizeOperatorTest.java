package at.ac.tuwien.ps.operator;

import at.ac.tuwien.ps.Context;
import at.ac.tuwien.ps.element.Element;
import at.ac.tuwien.ps.element.ElementType;
import at.ac.tuwien.ps.register.Register;
import at.ac.tuwien.ps.stream.CommandStream;
import org.junit.Assert;
import org.junit.Test;

import java.util.ArrayList;
import java.util.Stack;

public class StackSizeOperatorTest {

    private StackSizeOperator stackSizeOperator = new StackSizeOperator();

    @Test
    public void stackSizeTest1() {
        Stack<Element> stack = new Stack<>();
        stack.push(new Element("1", ElementType.INTEGER));
        stack.push(new Element("1", ElementType.INTEGER));
        Context context = new Context(new CommandStream(""), stack, new ArrayList<Register>());
        stackSizeOperator.execute(context);
        Assert.assertEquals(new Element("2", ElementType.INTEGER), stack.peek());
        Assert.assertEquals(3, stack.size());
    }

    @Test
    public void deleteTest2() {
        Stack<Element> stack = new Stack<>();
        Context context = new Context(new CommandStream(""), stack, new ArrayList<Register>());
        stackSizeOperator.execute(context);
        Assert.assertEquals(new Element("0", ElementType.INTEGER), stack.peek());
        Assert.assertEquals(1, stack.size());
    }
}
