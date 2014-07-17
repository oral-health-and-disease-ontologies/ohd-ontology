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
public class GlyphField extends Field {
	@SuppressWarnings("unused")
	private static final String __REV__ = "$Date : Rev$";

	protected GlyphField(String name, ImageArea area, int lvl) {
		super(name, area, FieldTypes.GLYPH, lvl);
		// TODO Auto-generated constructor stub
	}

	/* (non-Javadoc)
	 * @see oor.images.metadata.Field#setValue(java.util.Collection)
	 */
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
