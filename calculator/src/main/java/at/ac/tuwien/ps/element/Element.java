package at.ac.tuwien.ps.element;

import java.util.Objects;

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

    public int parseToInt(){
        return Integer.parseInt(this.getValue());
    }

    public String getListContent() {
        if(elementType != ElementType.LIST) {
            throw new ElementException(value + " is not a list");
        }
        return value.substring(1, value.length() - 1);
    }

    public Element deepCopy(){
        return new Element(this.getValue(),this.getElementType());
    }

    @Override
    public String toString() {
        return value;
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Element element = (Element) o;
        return Objects.equals(value, element.value) && elementType == element.elementType;
    }

    @Override
    public int hashCode() {
        return Objects.hash(value, elementType);
    }
}