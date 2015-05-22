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
public class SingleOptionField extends Field {
	@SuppressWarnings("unused")
	private static final String __REV__ = "$Date : Rev$";

	protected SingleOptionField(String name, ImageArea area, int lvl) {
		super(name, area, FieldTypes.SINGLE_OPTION, lvl);
		// TODO Auto-generated constructor stub
	}

	/* (non-Javadoc)
	 * @see oor.images.metadata.Field#setValue(java.util.Collection)
	 */
	@Override
	protected void setValue(List<Shape> detected) {
		if (detected != null && !detected.isEmpty() && detected.size() == 1) {
			if (FieldTypes.SINGLE_SELECT.equals(parent.getType()))
				value = fieldName;
			else
				this.setParentValue(fieldName);
		}

	}

	@Override
	protected void setValue(String value) {
		// TODO Auto-generated method stub
		
	}
}
