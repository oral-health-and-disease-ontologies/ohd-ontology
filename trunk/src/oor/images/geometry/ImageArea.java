/**
 * Class that represents an area within the image, it is assumed
 * that the area is perfectly rectangular. It is bounded by four
 * vertices represented by four points
 */
package oor.images.geometry;

import java.awt.geom.Area;
import java.awt.geom.Path2D;
import java.awt.image.BufferedImage;

/**
 * @author nikhillo
 * 
 */
public class ImageArea implements Shape{
	@SuppressWarnings("unused")
	private static final String __REV__ = "$Date$ : $Rev$";
	
	private final Point vertexUL;
	private final Point vertexUR;
	private final Point vertexLL;
	private final Point vertexLR;
	private final float height;
	private final float width;
	
	/**
	 * Default constructor
	 * @param xstart : x coordinate of the upper left vertex
	 * @param ystart : y coordinate of the upper left vertex
	 * @param xend : x coordinate of the lower right vertex
	 * @param yend : y coordinate of the lower right vertex
	 */
	public ImageArea(float xstart, float ystart, float xend, float yend) {
		vertexUL = new Point(xstart, ystart);
		vertexLR = new Point(xend, yend);
		vertexUR = new Point(xend, ystart);
		vertexLL = new Point(xstart, yend);
		height = yend - ystart;
		width = xend - xstart;
	}
	
	/**
	 * Method to get the width of the area
	 * @return The width of the area
	 */
	public float getWidth() {
		return width;
	}
	
	/**
	 * Method to get the height of the area
	 * @return The height of the area
	 */
	public float getHeight() {
		return height;
	}
	
	/**
	 * Method to check if the given Point is contained within the given area
	 * @param p : The Point to be checked
	 * @return true iff the Point lies within or on the edge of the area
	 */
	public boolean contains(Point p) {
		float px = p.xc;
		float py = p.yc;

		return (px >= vertexUL.xc && px <= vertexLR.xc && py >= vertexUR.yc && py <= vertexLL.yc);
	}
	
	/**
	 * Method to check if the given line segment is contained within the area
	 * @param segment : The line segment to be checked
	 * @return true iff both the end points of the line segment are contained within the area
	 */
	public boolean contains (LineSegment segment) {
		return (this.contains(segment.startPoint) && this.contains(segment.endPoint));
	}
	
	/**
	 * Method to get the sub image of a given image as defined by the coordinates of this area
	 * @param img : The image whose subimage is to be computed
	 * @return : The computed subimage if the operation is permissible, null otherwise
	 */
	public BufferedImage getSubImage(BufferedImage img) {
		return (img != null && img.getHeight() >= height && img.getWidth() > width) ? img
				.getSubimage((int) vertexUL.xc, (int) vertexUL.yc, (int) width,
						(int) height) : null;
	}
	
	public Area convert() {
		Path2D.Double dbl = new Path2D.Double();
		dbl.moveTo(vertexUL.xc, vertexUL.yc);
		dbl.lineTo(vertexUR.xc, vertexUR.yc);
		dbl.lineTo(vertexLR.xc, vertexLR.yc);
		dbl.lineTo(vertexLL.xc, vertexLL.yc);
		dbl.lineTo(vertexUL.xc, vertexUL.yc);
		return new Area(dbl);
	}
	
	public int getX() {
		return (int) vertexUL.xc;
	}
	
	public int getY() {
		return (int) vertexUL.yc;
	}
	
	@Override
	public String toString() {
		return "[" + vertexUL + " , " + vertexLR + "]";
	}
}
