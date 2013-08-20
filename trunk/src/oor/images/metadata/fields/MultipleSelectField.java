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
public class MultipleSelectField extends Field {
	@SuppressWarnings("unused")
	private static final String __REV__ = "$Date : Rev$";

	protected MultipleSelectField(String name, ImageArea area, int lvl) {
		super(name, area, FieldTypes.MULTI_SELECT, lvl);
		// TODO Auto-generated constructor stub
	}

	/* (non-Javadoc)
	 * @see oor.images.metadata.Field#setValue(java.util.Collection)
	 */
	@Override
	protected void setValue(List<Shape> detected) {
		if (numChildren > 0 && detected != null) {
			StringBuilder bldr = new StringBuilder();
			Shape shape;
			for (int i = 0; i < numChildren; i++) {
				shape = detected.get(i);
				if (shape != null) {
					if (i > 0)
						bldr.append(",");
					
					bldr.append(this.getChild(i).getName());
				}
			}
			
			value = bldr.toString();
		}

	}

	@Override
	protected void setValue(String value) {
		// TODO Auto-generated method stub
		
	}
}
