package at.ac.tuwien.ps.element;


import java.util.Objects;

public class Element {

    private String value;
    private ElementType elementType;

    public Element(String value, ElementType elementType) {
        this.value = value;
        this.elementType = elementType;
    }
    
    public Element(RegisterElement element){
    	this.value = element.getValue();
    	this.elementType = ElementType.valueOf(element.getElementType().name());
    }

    public String getValue() {
        return value;
    }
    
    public int parseToInt(){
    	return Integer.parseInt(this.getValue());
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
    
    public Element deepCopy(){
    	return new Element(this.getValue(),this.getElementType());
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Element element = (Element) o;
        return Objects.equals(value, element.value) &&
                elementType == element.elementType;
    }

    @Override
    public int hashCode() {

        return Objects.hash(value, elementType);
    }
}
