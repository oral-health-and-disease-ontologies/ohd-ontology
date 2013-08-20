/**
 * 
 */
package oor.images.output;

import java.awt.BasicStroke;
import java.awt.Color;
import java.awt.Graphics2D;
import java.awt.RenderingHints;
import java.awt.image.BufferedImage;
import java.awt.image.ColorModel;
import java.io.File;
import java.io.IOException;

import javax.imageio.ImageIO;

import oor.images.metadata.Field;

/**
 * @author nikhillo
 *
 */
public class ImageAnnotator implements ChartOutput{
	@SuppressWarnings("unused")
	private static final String __REV__ = "$Date : Rev$";
	
	private BufferedImage imageCopy;
	private Graphics2D g2d;
	
	/**
	 * Default constructor
	 * @param img : The image to be annotated, the ROIs are drawn on the image
	 */
	public ImageAnnotator (BufferedImage img) {
		ColorModel cm = img.getColorModel();
		imageCopy = new BufferedImage(cm, img.copyData(null), cm.isAlphaPremultiplied(), null);
		g2d = (Graphics2D) imageCopy.getGraphics();
		g2d.setRenderingHint(RenderingHints.KEY_ANTIALIASING, RenderingHints.VALUE_ANTIALIAS_ON);
	}
	

	@Override
	public void visitStart(Field fld) {
		if (fld.hasValue()) {
			g2d.setStroke(new BasicStroke(5f));
			g2d.setColor(Color.RED);
			g2d.draw(fld.getPosition().convert());
		}
	}

	@Override
	public void visitEnd(Field fld) {
		// TODO Auto-generated method stub
		
	}


	@Override
	public void writeToFile(String filename) throws IOException {
		g2d.dispose();
		ImageIO.write(imageCopy,"png", new File(filename));
	}

}
