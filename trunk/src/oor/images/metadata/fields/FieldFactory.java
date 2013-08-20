/**
 * Factory class to instantiate a field based upon its type
 */
package oor.images.metadata.fields;

import oor.images.geometry.ImageArea;
import oor.images.metadata.Field;
import oor.images.metadata.Field.FieldTypes;

/**
 * @author nikhillo
 * 
 */
public class FieldFactory {
	@SuppressWarnings("unused")
	private static final String __REV__ = "$Date : Rev$";
	
	public static Field initField(FieldTypes type, String name, ImageArea area,
			int lvl) {
		switch (type) {
		case LABEL:
			return new LabelField(name, area, lvl);
		case CELL:
			return new CellField(name, area, lvl);
		case FLEX:
			return new FlexField(name, area, lvl);
		case GLYPH:
			return new GlyphField(name, area, lvl);
		case MULTI_SELECT:
			return new MultipleSelectField(name, area, lvl);
		case SECTION:
			return new SectionField(name, area, lvl);
		case SINGLE_OPTION:
			return new SingleOptionField(name, area, lvl);
		case SINGLE_SELECT:
			return new SingleSelectField(name, area, lvl);
		case STACK:
			return new StackField(name, area, lvl);
		default:
			return null;
		}
	}
}
