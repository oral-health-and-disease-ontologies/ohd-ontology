/**
 * 
 */
package oor.images.metadata.fields;

import java.util.List;

import oor.images.geometry.ImageArea;
import oor.images.geometry.LineSegment;
import oor.images.geometry.Point;
import oor.images.geometry.Shape;
import oor.images.metadata.Field;

/**
 * @author nikhillo
 *
 */
public class FlexField extends Field {
	@SuppressWarnings("unused")
	private static final String __REV__ = "$Date : Rev$";

	protected FlexField(String name, ImageArea area, int lvl) {
		super(name, area, FieldTypes.FLEX, lvl);
		// TODO Auto-generated constructor stub
	}

	/* (non-Javadoc)
	 * @see oor.images.metadata.Field#setValue(java.util.Collection)
	 */
	@Override
	protected void setValue(List<Shape> detected) {
		if (getNumChildren() > 0 && detected != null) {
			LineSegment segment;
			Point sPt, ePt;
			ImageArea area;
			Field f;
			int startIdx = -1, endIdx = -1;
			
			for (Shape shp : detected) {
				segment = (LineSegment) shp;
				if (segment != null) {
					sPt = segment.start();
					ePt = segment.end();
					
					for (int i = 0; i < numChildren; i++) {
						f = this.getChild(i);
						area = f.getPosition();
						if (area.contains(sPt) && area.contains(ePt)) {
							((StackField) f).setSelected();
							continue;
						} else if (area.contains(sPt)) {
							startIdx = i;
						} else if (area.contains(ePt)) {
							endIdx = i;
						}
					}
					if (startIdx > -1 && endIdx > -1) {
						if (startIdx + endIdx == numChildren - 1) {
							((StackField) this.getChild(startIdx)).setSelected();
							((StackField) this.getChild(endIdx)).setSelected();
						} else {
							if (endIdx < startIdx) {
								int temp = endIdx;
								endIdx = startIdx;
								startIdx = temp;
							}
							
							for (int j = startIdx; j <= endIdx; j++) {
								((StackField) this.getChild(j)).setSelected();
							}
						}
					}
				}
			}
		}
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
