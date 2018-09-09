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

public class DeleteOperatorTest {

    private DeleteOperator deleteOperator = new DeleteOperator();

    @Test
    public void deleteTest1() {
        Stack<Element> stack = new Stack<>();
        stack.push(new Element("1", ElementType.INTEGER));
        stack.push(new Element("1", ElementType.INTEGER));
        Context context = new Context(new CommandStream(""), stack, new ArrayList<Register>());
        deleteOperator.execute(context);
        Assert.assertEquals(0, stack.size());
    }

    @Test
    public void deleteTest2() {
        Stack<Element> stack = new Stack<>();
        stack.push(new Element("(1 1+)", ElementType.LIST));
        stack.push(new Element("1", ElementType.INTEGER));
        Context context = new Context(new CommandStream(""), stack, new ArrayList<Register>());
        deleteOperator.execute(context);
        Assert.assertEquals(0, stack.size());
    }

    @Test(expected = OperatorException.class)
    public void deleteEmptyStackTest() {
        Context context = new Context(new CommandStream(""), new Stack<Element>(), new ArrayList<Register>());
        deleteOperator.execute(context);
    }

    @Test(expected = OperatorException.class)
    public void deleteStackOnlyOneElementTest() {
        Stack<Element> stack = new Stack<>();
        stack.push(new Element("1", ElementType.INTEGER));
        Context context = new Context(new CommandStream(""), stack, new ArrayList<Register>());
        deleteOperator.execute(context);
    }

    @Test(expected = OperatorException.class)
    public void deleteStackElementNotIntegerTest() {
        Stack<Element> stack = new Stack<>();
        stack.push(new Element("(1 1+)", ElementType.LIST));
        stack.push(new Element("(1 1+)", ElementType.LIST));
        Context context = new Context(new CommandStream(""), stack, new ArrayList<Register>());
        deleteOperator.execute(context);
    }

    @Test(expected = OperatorException.class)
    public void deleteNegativeTest() {
        Stack<Element> stack = new Stack<>();
        stack.push(new Element("2", ElementType.INTEGER));
        stack.push(new Element("-1", ElementType.INTEGER));
        Context context = new Context(new CommandStream(""), stack, new ArrayList<Register>());
        deleteOperator.execute(context);
    }

    @Test(expected = OperatorException.class)
    public void deleteZeroTest() {
        Stack<Element> stack = new Stack<>();
        stack.push(new Element("2", ElementType.INTEGER));
        stack.push(new Element("0", ElementType.INTEGER));
        Context context = new Context(new CommandStream(""), stack, new ArrayList<Register>());
        deleteOperator.execute(context);
    }

    @Test(expected = OperatorException.class)
    public void deleteOutOfRangeTest() {
        Stack<Element> stack = new Stack<>();
        stack.push(new Element("2", ElementType.INTEGER));
        stack.push(new Element("2", ElementType.INTEGER));
        Context context = new Context(new CommandStream(""), stack, new ArrayList<Register>());
        deleteOperator.execute(context);
    }

}
