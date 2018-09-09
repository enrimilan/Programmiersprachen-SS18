package at.ac.tuwien.ps.register;

import at.ac.tuwien.ps.element.Element;

public class Register {

	private RegisterContent registerContent;

	public Register(RegisterContent registerContent) {
	    this.registerContent = registerContent;
    }

	public void writeElement(Element element) {
        registerContent.write(element);
    }

    public Element readElement() {
        return registerContent.read();
    }

}
