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
public class SingleSelectField extends Field {
	@SuppressWarnings("unused")
	private static final String __REV__ = "$Date : Rev$";

	protected SingleSelectField(String name, ImageArea area, int lvl) {
		super(name, area, FieldTypes.SINGLE_SELECT, lvl);
		// TODO Auto-generated constructor stub
	}

	/* (non-Javadoc)
	 * @see oor.images.metadata.Field#setValue(java.util.Collection)
	 */
	@Override
	protected void setValue(List<Shape> detected) {
		if (numChildren > 0 && detected != null) {
			ImageArea shape;
			for (int i = 0; i < numChildren; i++) {
				shape = (ImageArea) detected.get(i);
				if (shape != null) {
					value = this.getChild(i).getName();
				}
			}
		}

	}

	@Override
	protected void setValue(String value) {
		// TODO Auto-generated method stub
		
	}

}
