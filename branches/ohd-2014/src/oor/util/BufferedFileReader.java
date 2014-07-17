/**
 * Utility class to read a file line by line via a BufferedReader
 */
package oor.util;

import java.io.BufferedReader;
import java.io.FileNotFoundException;
import java.io.FileReader;
import java.io.IOException;

/**
 * @author nikhillo
 *
 */
public class BufferedFileReader {
	@SuppressWarnings("unused")
	private static final String __REV__ = "$Date : Rev$";
	
	private BufferedReader rdr;
	private String delim;
	private boolean close;
	
	/**
	 * Default constructor
	 * @param location : The URI of the file to be read
	 * @param delim : The delimiter for the fields therein
	 */
	public BufferedFileReader (String location, String delim) {
		try {
			rdr = new BufferedReader(new FileReader(location));
			this.delim = delim;
		} catch (FileNotFoundException e) {
			e.printStackTrace();
		}
	}
	
	/**
	 * Method to read one single line from the file and return it as an array split
	 * by the given delimiter
	 * @return The array of fields as described above or null in case of an exception
	 * or if EOF is reached
	 */
	public String[] read() {
		if (rdr != null) {
			String line = null;
			try {
				try {
					line = rdr.readLine();
					if (line != null) {
						return line.split(delim);
					} else {
						close = true;
					}
				} finally {
					if (close) {
						rdr.close();
					}
				}
			} catch (IOException e) {

			}			
		}
		
		return null;
	}
}
