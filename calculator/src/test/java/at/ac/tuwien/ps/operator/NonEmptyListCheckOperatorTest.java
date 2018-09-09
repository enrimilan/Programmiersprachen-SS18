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

public class NonEmptyListCheckOperatorTest {

    private NonEmptyListCheckOperator nonEmptyListCheckOperator = new NonEmptyListCheckOperator();

    @Test
    public void nonEmptyListCheckTest1() {
        Stack<Element> stack = new Stack<>();
        stack.push(new Element("2", ElementType.INTEGER));
        Context context = new Context(new CommandStream(""), stack, new ArrayList<Register>());
        nonEmptyListCheckOperator.execute(context);
        Assert.assertEquals(new Element("0", ElementType.INTEGER), stack.peek());
        Assert.assertEquals(2, stack.size());
    }

    @Test
    public void nonEmptyListCheckTest2() {
        Stack<Element> stack = new Stack<>();
        stack.push(new Element("(1 1+)", ElementType.LIST));
        Context context = new Context(new CommandStream(""), stack, new ArrayList<Register>());
        nonEmptyListCheckOperator.execute(context);
        Assert.assertEquals(new Element("1", ElementType.INTEGER), stack.peek());
        Assert.assertEquals(2, stack.size());
    }

    @Test
    public void nonEmptyListCheckTest3() {
        Stack<Element> stack = new Stack<>();
        stack.push(new Element("()", ElementType.LIST));
        Context context = new Context(new CommandStream(""), stack, new ArrayList<Register>());
        nonEmptyListCheckOperator.execute(context);
        Assert.assertEquals(new Element("0", ElementType.INTEGER), stack.peek());
        Assert.assertEquals(2, stack.size());
    }

    @Test(expected = OperatorException.class)
    public void nonEmptyListCheckEmptyStackTest() {
        Context context = new Context(new CommandStream(""), new Stack<Element>(), new ArrayList<Register>());
        nonEmptyListCheckOperator.execute(context);
    }
}
