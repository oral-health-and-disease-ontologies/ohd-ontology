/**
 * 
 */
package oor.images.output;

import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;
import java.util.ArrayList;

import javax.imageio.ImageIO;

import oor.images.metadata.Field;
import oor.images.metadata.Field.FieldTypes;
import oor.util.ImageTypes;

/**
 * @author nikhillo
 *
 */
public class ImageClipper implements ChartOutput, ImageTypes {
	@SuppressWarnings("unused")
	private static final String __REV__ = "$Date : Rev $";
	
	private BufferedImage img;
	private String rootDir;
	public ArrayList<FieldTypes> restrictedTypes;
	
	public ImageClipper (BufferedImage rootImage, String rootDir, ArrayList<FieldTypes> types) {
		img = rootImage;
		this.rootDir = rootDir;
		restrictedTypes = types;
	}
	
	public ImageClipper (BufferedImage image, String rootDir) {
		this (image, rootDir, null);
	}

	@Override
	public void visitStart(Field fld) {
		if (fld != null && (restrictedTypes == null || restrictedTypes.contains(fld.getType()))) {
			BufferedImage subimg = fld.getPosition().getSubImage(img);
			try {
				String imgNm = fld.getParentName()+"_"+fld.getName();
				System.out.println("Writing : " + imgNm);
				imgNm = rootDir+imgNm+"." + PNG;
				ImageIO.write(subimg, PNG, new File(imgNm));
				//subimg = PreProcessor.morphology(imgNm, Constants.CIRCLE, 1, 1.0f);
				//ImageIO.write(subimg, PNG, new File(imgNm));
			} catch (IOException e) {
				// TODO Auto-generated catch block
				e.printStackTrace();
			}
		}
	}

	@Override
	public void visitEnd(Field fld) {
		// TODO Auto-generated method stub
		
	}

	@Override
	public void writeToFile(String filename) {
		// TODO Auto-generated method stub
		
	}

}
