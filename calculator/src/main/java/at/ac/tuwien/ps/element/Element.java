package at.ac.tuwien.ps.element;


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
    

}
