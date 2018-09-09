package at.ac.tuwien.ps.element.operator;

import at.ac.tuwien.ps.Context;
import at.ac.tuwien.ps.element.Element;
import at.ac.tuwien.ps.element.ElementType;
import at.ac.tuwien.ps.register.Register;
import at.ac.tuwien.ps.CommandStream;
import org.junit.Assert;
import org.junit.Test;

import java.util.ArrayList;
import java.util.Stack;

public class ApplyLaterOperatorTest {

    private ApplyLaterOperator applyLaterOperator = new ApplyLaterOperator();

    @Test
    public void applyLaterTest1() {
        CommandStream commandStream = new CommandStream("(1 2*)");
        Stack<Element> stack = new Stack<>();
        stack.push(new Element("(1 1+)", ElementType.LIST));
        Context context = new Context(commandStream, stack, new ArrayList<Register>());
        applyLaterOperator.execute(context);
        Assert.assertEquals("(1 2*)1 1+", commandStream.getContent());
        Assert.assertEquals(0, stack.size());
    }

    @Test
    public void applyLaterTest2() {
        CommandStream commandStream = new CommandStream("0");
        Stack<Element> stack = new Stack<>();
        stack.push(new Element("(8)", ElementType.LIST));
        Context context = new Context(commandStream, stack, new ArrayList<Register>());
        applyLaterOperator.execute(context);
        Assert.assertEquals("0 8", commandStream.getContent());
        Assert.assertEquals(0, stack.size());
    }

    @Test(expected = OperatorException.class)
    public void applyLaterEmptyStackTest() {
        Context context = new Context(new CommandStream(""), new Stack<Element>(), new ArrayList<Register>());
        applyLaterOperator.execute(context);
    }

    @Test(expected = OperatorException.class)
    public void applyLaterStackElementNotListTest() {
        Stack<Element> stack = new Stack<>();
        stack.push(new Element("1", ElementType.INTEGER));
        Context context = new Context(new CommandStream(""), stack, new ArrayList<Register>());
        applyLaterOperator.execute(context);
    }
}
