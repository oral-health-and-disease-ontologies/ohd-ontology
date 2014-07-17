/**
 * Class to denote any Field on a chart
 */
package oor.images.metadata;

import java.util.ArrayList;
import java.util.List;

import oor.images.detectors.OptionDetector;
import oor.images.geometry.ImageArea;
import oor.images.geometry.Shape;
import oor.images.output.ChartOutput;

/**
 * @author nikhillo
 *
 */
public abstract class Field {
	@SuppressWarnings("unused")
	private static final String __REV__ = "$Rev$ : $Date$";
	
	protected final String fieldName;
	protected final ImageArea position;
	private final int level;
	private final FieldTypes type;
	protected Field parent;
	protected List<Field> children;
	protected int numChildren = -1;
	protected String value;
	
	/**
	 * An enumeration representing the supported field types
	 *
	 */
	public enum FieldTypes {
		SECTION {public String toString() {return "S";}}, 
		LABEL {public String toString() {return "L";}},
		SINGLE_OPTION {public String toString() {return "SO";}},
		SINGLE_SELECT {public String toString() {return "SS";}},
		MULTI_SELECT {public String toString() {return "MS";}},
		FLEX {public String toString() {return "FL";}},
		STACK {public String toString() {return "ST";}},
		CELL {public String toString() {return "C";}}, 
		GLYPH {public String toString() {return "G";}}
	}
	
	/**
	 * Default constructor
	 * @param name : The name of the field
	 * @param area : The given location of the field on the chart
	 * @param ft : The field type
	 * @param lvl : The level of the field in the hierarchy of the chart
	 */
	protected Field (String name, ImageArea area, FieldTypes ft, int lvl) {
		this.fieldName = name;
		this.position = area;
		this.type = ft;
		this.level = lvl;
	}
	
	/**
	 * Method to add a given field as a child of the current field
	 * @param child The child to be added
	 */
	protected void add (Field child) {
		if (children == null) {
			children = new ArrayList<Field>();
		}
		
		children.add(child);
		child.parent = this;
	}
	
	/**
	 * Method to get the level of the current field
	 * @return
	 */
	protected int getLevel() {
		return level;
	}
	
	/**
	 * Method to convert the given string representing the field type
	 * to the enumeration value
	 * @param type : The Field type represented as the string
	 * @return The mapped field type if it exists or null otherwise
	 */
	protected static FieldTypes convert(String type) {
		for (FieldTypes ft : FieldTypes.values()) {
			if (type.equals(ft.toString())) {
				return ft;
			}
		}
		
		return null;
	}
	
	/**
	 * Method to get the position of the field on the chart
	 * @return
	 */
	public ImageArea getPosition() {
		return position;
	}
	
	/**
	 * Method to accept the given detector and allow it to detect values within the field
	 * @param detector : The detector being accepted
	 */
	public void accept (OptionDetector detector) {
		//short circuit at highest level, if not delegate to children
		if (detector.isSupported(this.type))
			this.setValue(detector.visit(this));
		else if (children != null) {
			for (Field fld : children) {
				fld.accept(detector);
			}
		}
	}
	
	/**
	 * Method to return the number of children of the current field
	 * @return The number of children if present, 0 otherwise
	 */
	public int getNumChildren() {
		if (numChildren == -1) {
			numChildren = (children != null) ? children.size() : 0 ;
		}
		
		return numChildren;
	}
	
	/**
	 * Method to  get a given numbered child of the current field
	 * @param index : The index of the child as represented in a 0 index numbering scheme
	 * @return The child if present, null otherwise
	 */
	public Field getChild(int index) {
		if (numChildren == -1) {
			getNumChildren();
		}
		
		return (numChildren == 0 || index >= numChildren) ? null : children.get(index);
	}
	
	/**
	 * Method to get the name of the current field
	 * @return
	 */
	public String getName() {
		return fieldName;
	}
	
	/**
	 * Method to set the value of the current field based upon the detected shapes
	 * @param detected : A List representing the shapes detected in the current field
	 */
	protected abstract void setValue (List<Shape> detected);
	
	protected abstract void setValue (String value);
	
	/**
	 * Method to accept the given output generator and allow it to read the value of this field
	 * @param outut : The output generator being accepted
	 */
	public void accept (ChartOutput output) {
		output.visitStart(this);
		
		if (children != null) {
			for (Field fld : children) {
				fld.accept(output);
			}
		}
		
		output.visitEnd(this);
	}
	
	public boolean hasValue() {
		return (value != null && !value.isEmpty());
	}
	
	public FieldTypes getType() {
		return type;
	}
	
	public String getParentName() {
		return (parent == null) ? null : parent.getName();
	}
	
	public void clearValues() {
		if (hasValue()) {
			value = null;
		}
		
		if (children != null) {
			for (Field f : children) {
				f.clearValues();
			}
		}
	}
	
	public String toString() {
		return hasValue() ? fieldName + " = '" + value + "'": fieldName;
	}
	
	public boolean hasChildren() {
		return this.getNumChildren() > 0;
	}
	
	protected Field findFirstType(FieldTypes type) {
		if (hasChildren()) {
			for (Field f : children) {
				if (type.equals(f.getType()))
					return f;
			}
		}
		
		return null;
	}
	
	protected String getValue() {
		return value;
	}
	
	protected void setParentValue(String str) {
		parent.value = str;
	}
	
	public Field find(String fldName) {
		if (hasChildren()) {
			for (Field f : children) {
				if (fldName.equals(f.fieldName)) {
					return f;
				}
			}
		}
		
		return null;
	}
}
