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

public class CombineOperatorTest {

    private CombineOperator combineOperator = new CombineOperator();

    @Test
    public void combineTest1() {
        Stack<Element> stack = new Stack<>();
        stack.push(new Element("(2 1+)", ElementType.LIST));
        stack.push(new Element("3", ElementType.INTEGER));
        Context context = new Context(new CommandStream(""), stack, new ArrayList<Register>());
        combineOperator.execute(context);
        Assert.assertEquals(1, stack.size());
        Assert.assertEquals(new Element("(2 1+3)", ElementType.LIST), stack.pop());
    }

    @Test
    public void combineTest2() {
        Stack<Element> stack = new Stack<>();
        stack.push(new Element("(2 1)", ElementType.LIST));
        stack.push(new Element("3", ElementType.INTEGER));
        Context context = new Context(new CommandStream(""), stack, new ArrayList<Register>());
        combineOperator.execute(context);
        Assert.assertEquals(1, stack.size());
        Assert.assertEquals(new Element("(2 1 3)", ElementType.LIST), stack.pop());
    }

    @Test
    public void combineTest3() {
        Stack<Element> stack = new Stack<>();
        stack.push(new Element("(2 1+)", ElementType.LIST));
        stack.push(new Element("(3 4+)", ElementType.LIST));
        Context context = new Context(new CommandStream(""), stack, new ArrayList<Register>());
        combineOperator.execute(context);
        Assert.assertEquals(1, stack.size());
        Assert.assertEquals(new Element("(2 1+(3 4+))", ElementType.LIST), stack.pop());
    }

    @Test
    public void combineTest4() {
        Stack<Element> stack = new Stack<>();
        stack.push(new Element("(2 1)", ElementType.LIST));
        stack.push(new Element("(3 4+)", ElementType.LIST));
        Context context = new Context(new CommandStream(""), stack, new ArrayList<Register>());
        combineOperator.execute(context);
        Assert.assertEquals(1, stack.size());
        Assert.assertEquals(new Element("(2 1(3 4+))", ElementType.LIST), stack.pop());
    }

    @Test
    public void combineTest5() {
        Stack<Element> stack = new Stack<>();
        stack.push(new Element("(2 1+)", ElementType.LIST));
        stack.push(new Element("()", ElementType.LIST));
        Context context = new Context(new CommandStream(""), stack, new ArrayList<Register>());
        combineOperator.execute(context);
        Assert.assertEquals(1, stack.size());
        Assert.assertEquals(new Element("(2 1+())", ElementType.LIST), stack.pop());
    }

    @Test
    public void combineTest6() {
        Stack<Element> stack = new Stack<>();
        stack.push(new Element("(2 1)", ElementType.LIST));
        stack.push(new Element("()", ElementType.LIST));
        Context context = new Context(new CommandStream(""), stack, new ArrayList<Register>());
        combineOperator.execute(context);
        Assert.assertEquals(1, stack.size());
        Assert.assertEquals(new Element("(2 1())", ElementType.LIST), stack.pop());
    }

    @Test
    public void combineTest7() {
        Stack<Element> stack = new Stack<>();
        stack.push(new Element("()", ElementType.LIST));
        stack.push(new Element("(3 4+)", ElementType.LIST));
        Context context = new Context(new CommandStream(""), stack, new ArrayList<Register>());
        combineOperator.execute(context);
        Assert.assertEquals(1, stack.size());
        Assert.assertEquals(new Element("((3 4+))", ElementType.LIST), stack.pop());
    }

    @Test(expected = OperatorException.class)
    public void combineNoListTest() {
        Stack<Element> stack = new Stack<>();
        stack.push(new Element("1", ElementType.INTEGER));
        stack.push(new Element("1", ElementType.INTEGER));
        Context context = new Context(new CommandStream(""), stack, new ArrayList<Register>());
        combineOperator.execute(context);
    }

    @Test(expected = OperatorException.class)
    public void combineEmptyStackTest() {
        Context context = new Context(new CommandStream(""), new Stack<Element>(), new ArrayList<Register>());
        combineOperator.execute(context);
    }
}
