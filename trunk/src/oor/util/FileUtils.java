package oor.util;

import java.io.File;
import java.io.FilenameFilter;

public class FileUtils {
	public static String[] getFileList(String directory, final String suffix) {
		File dir = new File(directory);
		return dir.list(new FilenameFilter() {

			@Override
			public boolean accept(File arg0, String arg1) {
				return (arg1.endsWith(suffix));
			}
			
		});
	}
}
