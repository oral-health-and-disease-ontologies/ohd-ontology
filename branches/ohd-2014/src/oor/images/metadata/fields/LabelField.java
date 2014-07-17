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
public class LabelField extends Field {
	@SuppressWarnings("unused")
	private static final String __REV__ = "$Date : Rev$";

	public LabelField(String name, ImageArea area, int lvl) {
		super(name, area, FieldTypes.LABEL, lvl);
	}


	@Override
	protected void setValue(List<Shape> detected) {
		// TODO Auto-generated method stub
		
	}


	@Override
	public boolean hasValue() {
		// TODO Auto-generated method stub
		return false;
	}


	@Override
	protected void setValue(String value) {
		// TODO Auto-generated method stub
		
	}

}
