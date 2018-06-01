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

public class ApplyImmediatelyOperatorTest {

    private ApplyImmediatelyOperator applyImmediatelyOperator = new ApplyImmediatelyOperator();

    @Test
    public void applyImmediatelyTest() {
        CommandStream commandStream = new CommandStream("(1 2*)");
        Stack<Element> stack = new Stack<>();
        stack.push(new Element("(1 1+)", ElementType.LIST));
        Context context = new Context(commandStream, stack, new ArrayList<Register>());
        applyImmediatelyOperator.execute(context);
        Assert.assertEquals("1 1+(1 2*)", commandStream.getContent());
        Assert.assertEquals(0, stack.size());
    }

    @Test(expected = OperatorException.class)
    public void applyImmediatelyEmptyStackTest() {
        Context context = new Context(new CommandStream(""), new Stack<Element>(), new ArrayList<Register>());
        applyImmediatelyOperator.execute(context);
    }

    @Test(expected = OperatorException.class)
    public void applyImmediatelyStackElementNotListTest() {
        Stack<Element> stack = new Stack<>();
        stack.push(new Element("1", ElementType.INTEGER));
        Context context = new Context(new CommandStream(""), stack, new ArrayList<Register>());
        applyImmediatelyOperator.execute(context);
    }

}
