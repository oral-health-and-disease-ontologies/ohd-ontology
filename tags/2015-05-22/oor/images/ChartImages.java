/**
 * This class is a container for all image representations of a chart. Based on the applied
 * morphology and filtering, a chart may be expressed in multiple images. THis class acts as
 * a container for all those images
 */
package oor.images;

import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;
import java.util.HashMap;
import java.util.Map.Entry;

import javax.imageio.ImageIO;

/**
 * @author nikhillo
 */
public class ChartImages {
	@SuppressWarnings("unused")
	private static final String __REV__ = "$Date$ : $Rev$";
	
	public enum ImageTypes {
		ORIGINAL_IMAGE,
		REGISTERED_IMAGE,
		FILTERED_IMAGE
	}
	
	private static String origDir;
	private static String regDir;
	private static String fltDir;
	
	private String imgName;
	private HashMap<ImageTypes, BufferedImage> images;
	
	/**
	 * Method to initialize the containers with the respective image folders
	 */
	public static void init(HashMap<ImageTypes, String> dirMap) {
		String value;
		for (Entry<ImageTypes, String> entry : dirMap.entrySet()) {
			value = entry.getValue();
			
			switch (entry.getKey()) {
			case ORIGINAL_IMAGE:
				origDir = value;
				break;
			case REGISTERED_IMAGE:
				regDir = value;
				break;
			case FILTERED_IMAGE:
				fltDir = value;
				break;
			}
		}
	}
	
	/**
	 * Default constructor
	 * @param imageName : The name of the image
	 */
	public ChartImages(String imageName) {
		imgName = imageName;
		images = new HashMap<ChartImages.ImageTypes, BufferedImage>(ImageTypes.values().length);
	}
	
	/**
	 * Method to get the BufferedImage representation of a given image and type
	 * @param type : The type of image representation needed
	 * @return
	 * @throws IOException
	 */
	public BufferedImage getImage(ImageTypes type) throws IOException {
		BufferedImage retImage = images.get(type);
		if (retImage == null) {
			retImage = loadImage(type);
		}
		return retImage;
	}
	
	/**
	 * Method to load an image with a given type
	 * @param type : Type of the image to be loaded
	 * @return The loaded image
	 * @throws IOException If there is an exception in loading the image
	 */
	private BufferedImage loadImage(ImageTypes type) throws IOException {
		String directory = decodeDirectory(type);
		return ImageIO.read(new File(directory+"/"+imgName));
	}
	
	/**
	 * Method to map the image type to corresponding directory
	 * @param type
	 * @return
	 */
	private String decodeDirectory(ImageTypes type) {
		switch (type) {
		case ORIGINAL_IMAGE:
			return origDir;
		case REGISTERED_IMAGE:
			return regDir;
		case FILTERED_IMAGE:
			return fltDir;
		default:
			return null;
		}
	}
}
