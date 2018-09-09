package at.ac.tuwien.ps.register;

import at.ac.tuwien.ps.element.Element;

public class NormalRegisterContent implements RegisterContent {

    private Element element;

    @Override
    public Element read() {
        return element;
    }

    @Override
    public void write(Element element) {
        this.element = element;
    }
}
