package at.ac.tuwien.ps.element;

public class Element {

    private String value;
    private ElementType elementType;

    public Element(String value, ElementType elementType) {
        this.value = value;
        this.elementType = elementType;
    }

    public String getValue() {
        return value;
    }

    public void setValue(String value) {
        this.value = value;
    }

    public ElementType getElementType() {
        return elementType;
    }

    public void setElementType(ElementType elementType) {
        this.elementType = elementType;
    }

    @Override
    public String toString() {
        return value;
    }

}
