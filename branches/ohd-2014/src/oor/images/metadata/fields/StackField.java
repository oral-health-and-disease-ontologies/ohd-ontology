/**
 * 
 */
package oor.images.metadata.fields;

import java.util.List;

import oor.images.geometry.ImageArea;
import oor.images.geometry.Shape;
import oor.images.metadata.Field;

/**
 * @author nikhillo
 *
 */
public class StackField extends Field {
	@SuppressWarnings("unused")
	private static final String __REV__ = "$Date : Rev$";

	protected StackField(String name, ImageArea area, int lvl) {
		super(name, area, FieldTypes.STACK, lvl);
		// TODO Auto-generated constructor stub
	}

	/* (non-Javadoc)
	 * @see oor.images.metadata.Field#setValue(java.util.Collection)
	 */
	@Override
	protected void setValue(List<Shape> detected) {
		// TODO Auto-generated method stub

	}

	
	public void setSelected() {
		setValue("Missing");
	}

	@Override
	public boolean hasValue() {
		return (this.value != null);
	}

	@Override
	protected void setValue(String value) {
		this.value = value;
		
	}
}
