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

public class ExitOperatorTest {

    private ExitOperator exitOperator = new ExitOperator();

    @Test
    public void exitTest() {
        Stack<Element> stack = new Stack<>();
        stack.push(new Element("1", ElementType.INTEGER));
        stack.push(new Element("2", ElementType.INTEGER));
        Context context = new Context(new CommandStream("(1 1*)"), stack, new ArrayList<Register>());
        exitOperator.execute(context);
        Assert.assertTrue(context.getCommandStream().getContent().isEmpty());
        Assert.assertFalse(context.getCommandStream().hasNext());
        Assert.assertEquals(0, stack.size());
    }
}
