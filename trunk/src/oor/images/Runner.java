package oor.images;

import java.awt.image.BufferedImage;
import java.io.File;
import java.io.IOException;

import javax.imageio.ImageIO;

import oor.images.detectors.EllipseDetector;
import oor.images.detectors.LineSegmentDetector;
import oor.images.detectors.OptionDetector;
import oor.images.metadata.ChartMetadata;
import oor.images.output.ChartOutput;
import oor.images.output.XmlGenerator;

public class Runner {

	/**
	 * @param args
	 * @throws IOException 
	 */
	public static void main(String[] args) throws IOException {
		ChartMetadata cmd = ChartMetadata.load("/Users/nicarus/Documents/imgproc/chartinfo.txt");
		String dirName = "/Users/nicarus/Documents/imgproc/tgtfiles/processed/";
		File dir = new File(dirName);
		String[] files = dir.list();
		BufferedImage rootImg;

		ChartOutput output;
		OptionDetector detector;
		
		for (String file : files) {
			if (file.endsWith("png")) {
				System.out.println(file);
				System.out.println("------------------------------------------------------");
				cmd.clearValues();
				rootImg = ImageIO.read(new File(dirName+file));
				detector = new EllipseDetector(rootImg);
				cmd.accept(detector);
				detector = new LineSegmentDetector(rootImg);
				cmd.accept(detector);
				output = new XmlGenerator();
				cmd.accept(output);
				((XmlGenerator) output).print();
				System.out.println("------------------------------------------------------");
			}
			

		}
	}

}
